(ns lexicon.packages.ediff
  "Ediff and smerge-mode integration (ediff.el, smerge-mode.el).

  Implements Emacs diff comparison and merge functionality:

  Ediff commands:
  - ediff-buffers: Compare two buffers side-by-side
  - ediff-regions-linewise: Compare regions line-by-line

  Smerge-mode for conflict resolution:
  - smerge-mode: Minor mode for resolving merge conflicts
  - smerge-next (C-c ^ n): Go to next conflict
  - smerge-prev (C-c ^ p): Go to previous conflict
  - smerge-keep-upper (C-c ^ u): Keep upper/mine version
  - smerge-keep-lower (C-c ^ l): Keep lower/other version
  - smerge-keep-base (C-c ^ b): Keep base version
  - smerge-keep-current (C-c ^ m): Keep version at point
  - smerge-keep-all (C-c ^ a): Keep all versions

  Conflict marker format:
  <<<<<<< mine
  [upper content]
  ||||||| base  (optional)
  [base content]
  =======
  [lower content]
  >>>>>>> theirs

  Based on Emacs lisp/vc/ediff.el and lisp/vc/smerge-mode.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (Package-local)
;; =============================================================================

;; Smerge state keyed by buffer-id: {buffer-id {:conflicts [] :current-index N}}
(defonce smerge-state (atom {}))

;; Ediff state: {:buffer-a-name str :diffs [] :diff-index N}
(defonce ediff-state (atom {}))

;; =============================================================================
;; Conflict Detection (smerge)
;; =============================================================================

(def conflict-begin-re #"^<<<<<<<\s*(.*)")
(def conflict-base-re #"^\|\|\|\|\|\|\|\s*(.*)")
(def conflict-sep-re #"^=======$")
(def conflict-end-re #"^>>>>>>>\s*(.*)")

(defn- parse-conflicts
  "Parse conflict markers in text. Returns vector of conflict maps:
   {:start-line :end-line :upper :base :lower :upper-label :lower-label}"
  [text]
  (let [lines (str/split text #"\n" -1)]
    (loop [i 0
           conflicts []
           state :outside
           current nil]
      (if (>= i (count lines))
        conflicts
        (let [line (nth lines i)]
          (case state
            :outside
            (if-let [match (re-find conflict-begin-re line)]
              (recur (inc i) conflicts :upper
                     {:start-line i
                      :upper-label (nth match 1)
                      :upper-lines []
                      :base-lines nil
                      :lower-lines []})
              (recur (inc i) conflicts :outside nil))

            :upper
            (cond
              (re-find conflict-base-re line)
              (recur (inc i) conflicts :base
                     (assoc current :base-lines []))

              (re-find conflict-sep-re line)
              (recur (inc i) conflicts :lower current)

              :else
              (recur (inc i) conflicts :upper
                     (update current :upper-lines conj line)))

            :base
            (if (re-find conflict-sep-re line)
              (recur (inc i) conflicts :lower current)
              (recur (inc i) conflicts :base
                     (update current :base-lines conj line)))

            :lower
            (if-let [match (re-find conflict-end-re line)]
              (recur (inc i)
                     (conj conflicts
                           (assoc current
                                  :end-line i
                                  :lower-label (nth match 1)
                                  :upper (str/join "\n" (:upper-lines current))
                                  :base (when (:base-lines current)
                                          (str/join "\n" (:base-lines current)))
                                  :lower (str/join "\n" (:lower-lines current))))
                     :outside nil)
              (recur (inc i) conflicts :lower
                     (update current :lower-lines conj line)))))))))

(defn- resolve-conflict
  "Replace a conflict region in text with the chosen resolution."
  [text conflict resolution]
  (let [lines (str/split text #"\n" -1)
        before (subvec (vec lines) 0 (:start-line conflict))
        after (subvec (vec lines) (inc (:end-line conflict)))
        replacement (case resolution
                      :upper (:upper conflict)
                      :lower (:lower conflict)
                      :base (or (:base conflict) "")
                      :all (str (:upper conflict) "\n"
                                (when (:base conflict)
                                  (str (:base conflict) "\n"))
                                (:lower conflict))
                      (:upper conflict))
        replacement-lines (str/split replacement #"\n" -1)]
    (str/join "\n" (concat before replacement-lines after))))

;; =============================================================================
;; Smerge Commands
;; =============================================================================

(defn smerge-enable!
  "Enable smerge-mode for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        text (lisp/buffer-string)
        conflicts (when text (parse-conflicts text))]
    (if (seq conflicts)
      (do
        (swap! smerge-state assoc buffer-id {:conflicts conflicts :current-index 0})
        ;; Add :smerge-mode to minor-modes set
        (let [current-modes (or (lisp/get-buffer-property buffer-id :minor-modes) #{})]
          (lisp/set-buffer-property buffer-id :minor-modes (conj current-modes :smerge-mode)))
        (lisp/message (str "smerge-mode enabled: " (count conflicts) " conflict"
                           (when (not= 1 (count conflicts)) "s") " found")))
      (lisp/message "No conflict markers found"))))

(defn smerge-next!
  "Go to next conflict (C-c ^ n)."
  []
  (let [buffer-id (lisp/current-buffer)
        state (get @smerge-state buffer-id)
        conflicts (or (:conflicts state) [])
        idx (or (:current-index state) 0)
        new-idx (min (inc idx) (dec (max 1 (count conflicts))))]
    (if (seq conflicts)
      (let [conflict (nth conflicts new-idx)]
        (swap! smerge-state assoc-in [buffer-id :current-index] new-idx)
        (lisp/message (str "Conflict " (inc new-idx) "/" (count conflicts)
                           " (line " (inc (:start-line conflict)) ")")))
      (lisp/message "No conflicts"))))

(defn smerge-prev!
  "Go to previous conflict (C-c ^ p)."
  []
  (let [buffer-id (lisp/current-buffer)
        state (get @smerge-state buffer-id)
        conflicts (or (:conflicts state) [])
        idx (or (:current-index state) 0)
        new-idx (max (dec idx) 0)]
    (if (seq conflicts)
      (let [conflict (nth conflicts new-idx)]
        (swap! smerge-state assoc-in [buffer-id :current-index] new-idx)
        (lisp/message (str "Conflict " (inc new-idx) "/" (count conflicts)
                           " (line " (inc (:start-line conflict)) ")")))
      (lisp/message "No conflicts"))))

(defn smerge-resolve-current!
  "Resolve the current conflict with the given resolution."
  [resolution]
  (let [buffer-id (lisp/current-buffer)
        state (get @smerge-state buffer-id)
        conflicts (or (:conflicts state) [])
        idx (or (:current-index state) 0)]
    (if (and (seq conflicts) (< idx (count conflicts)))
      (let [conflict (nth conflicts idx)
            text (lisp/buffer-string)
            new-text (when text (resolve-conflict text conflict resolution))]
        (when new-text
          (let [remaining (parse-conflicts new-text)]
            ;; Update package-local state
            (swap! smerge-state assoc buffer-id
                   {:conflicts remaining
                    :current-index (min idx (dec (max 1 (count remaining))))})
            ;; Replace buffer content
            (lisp/delete-region 0 (count text))
            (lisp/insert new-text)
            (lisp/message (str "Conflict resolved (" (name resolution) "). "
                               (count remaining) " remaining")))))
      (lisp/message "No conflict at point"))))

(defn smerge-keep-upper!
  "Keep upper (mine) version of current conflict (C-c ^ u)."
  []
  (smerge-resolve-current! :upper))

(defn smerge-keep-lower!
  "Keep lower (other) version of current conflict (C-c ^ l)."
  []
  (smerge-resolve-current! :lower))

(defn smerge-keep-base!
  "Keep base version of current conflict (C-c ^ b)."
  []
  (smerge-resolve-current! :base))

(defn smerge-keep-all!
  "Keep all versions of current conflict (C-c ^ a)."
  []
  (smerge-resolve-current! :all))

;; =============================================================================
;; Ediff - Buffer Comparison
;; =============================================================================

(defn- compute-line-diff
  "Simple line-by-line diff between two texts.
   Returns vector of {:type :same/:added/:removed :line-a :line-b :text}."
  [text-a text-b]
  (let [lines-a (str/split text-a #"\n" -1)
        lines-b (str/split text-b #"\n" -1)
        max-len (max (count lines-a) (count lines-b))]
    (mapv (fn [i]
            (let [a (get lines-a i)
                  b (get lines-b i)]
              (cond
                (and a b (= a b)) {:type :same :line-a (inc i) :line-b (inc i) :text a}
                (and a b)         {:type :changed :line-a (inc i) :line-b (inc i) :text-a a :text-b b}
                (and a (nil? b))  {:type :removed :line-a (inc i) :text a}
                (and (nil? a) b)  {:type :added :line-b (inc i) :text b})))
          (range max-len))))

(defn ediff-run!
  "Run ediff comparison between buffer A (stored in state) and buffer B."
  [buf-name-b]
  (let [buf-name-a (:buffer-a-name @ediff-state)
        buf-id-a (lisp/get-buffer buf-name-a)
        buf-id-b (lisp/get-buffer buf-name-b)]
    (if (and buf-id-a buf-id-b)
      (let [text-a (or (lisp/buffer-text-of buf-id-a) "")
            text-b (or (lisp/buffer-text-of buf-id-b) "")
            diffs (compute-line-diff text-a text-b)
            diff-count (count (filter #(not= :same (:type %)) diffs))
            output (str "Ediff: " buf-name-a " vs " buf-name-b "\n"
                        (str/join "" (repeat 60 "=")) "\n\n"
                        (str/join "\n"
                                  (map (fn [d]
                                         (case (:type d)
                                           :same (str "  " (:text d))
                                           :changed (str "- " (:text-a d) "\n+ " (:text-b d))
                                           :removed (str "- " (:text d))
                                           :added (str "+ " (:text d))
                                           ""))
                                       diffs))
                        "\n\n"
                        diff-count " difference" (when (not= 1 diff-count) "s") " found.\n")]
        ;; Store diffs in package-local state
        (swap! ediff-state assoc :diffs diffs :diff-index 0)

        ;; Kill existing buffer if any
        (when-let [existing-id (lisp/get-buffer "*ediff*")]
          (lisp/kill-buffer existing-id))

        ;; Create *ediff* buffer
        (lisp/create-special-buffer "*ediff*" output
                                    {:major-mode :diff-mode
                                     :read-only true})
        (lisp/split-window-below)
        (lisp/switch-to-buffer "*ediff*")
        (lisp/message (str diff-count " difference"
                           (when (not= 1 diff-count) "s") " found")))
      (lisp/message (str "Buffer not found: "
                         (if buf-id-a buf-name-b buf-name-a))))))

(defn ediff-set-buffer-a!
  "Set buffer A for ediff and prompt for buffer B."
  [buf-name-a]
  (swap! ediff-state assoc :buffer-a-name buf-name-a)
  (lisp/read-from-minibuffer "Ediff buffer B: " ediff-run!))

(defn ediff-buffers-interactive
  "Compare two buffers (M-x ediff-buffers)."
  []
  (lisp/read-from-minibuffer "Ediff buffer A: " ediff-set-buffer-a!))

;; =============================================================================
;; Ediff Navigation
;; =============================================================================

(defn ediff-next-diff!
  "Go to next difference."
  []
  (let [diffs (or (:diffs @ediff-state) [])
        idx (or (:diff-index @ediff-state) 0)
        ;; Find next non-same diff
        remaining (drop (inc idx) diffs)
        next-diff-offset (first (keep-indexed
                                  (fn [i d] (when (not= :same (:type d)) i))
                                  remaining))]
    (if next-diff-offset
      (let [new-idx (+ (inc idx) next-diff-offset)]
        (swap! ediff-state assoc :diff-index new-idx)
        (lisp/message (str "Difference at line " new-idx)))
      (lisp/message "No more differences"))))

(defn ediff-prev-diff!
  "Go to previous difference."
  []
  (let [diffs (or (:diffs @ediff-state) [])
        idx (or (:diff-index @ediff-state) 0)
        before (take idx diffs)
        prev-diff-offset (last (keep-indexed
                                 (fn [i d] (when (not= :same (:type d)) i))
                                 before))]
    (if prev-diff-offset
      (do
        (swap! ediff-state assoc :diff-index prev-diff-offset)
        (lisp/message (str "Difference at line " prev-diff-offset)))
      (lisp/message "No previous differences"))))

(defn ediff-quit!
  "Quit ediff."
  []
  (when-let [grep-id (lisp/get-buffer "*ediff*")]
    (lisp/delete-window)
    (lisp/kill-buffer grep-id)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize ediff and smerge-mode."
  []
  ;; Ediff commands
  (lisp/define-command 'ediff-buffers
    ediff-buffers-interactive
    "Compare two buffers side-by-side")

  ;; Smerge commands
  (lisp/define-command 'smerge-mode
    smerge-enable!
    "Toggle smerge minor mode for conflict resolution")

  (lisp/define-command 'smerge-next
    smerge-next!
    "Go to next conflict (C-c ^ n)")

  (lisp/define-command 'smerge-prev
    smerge-prev!
    "Go to previous conflict (C-c ^ p)")

  (lisp/define-command 'smerge-keep-upper
    smerge-keep-upper!
    "Keep upper (mine) version")

  (lisp/define-command 'smerge-keep-lower
    smerge-keep-lower!
    "Keep lower (other) version")

  (lisp/define-command 'smerge-keep-base
    smerge-keep-base!
    "Keep base version")

  (lisp/define-command 'smerge-keep-all
    smerge-keep-all!
    "Keep all versions")

  ;; Ediff navigation commands
  (lisp/define-command 'ediff-next-difference
    ediff-next-diff!
    "Go to next ediff difference")

  (lisp/define-command 'ediff-previous-difference
    ediff-prev-diff!
    "Go to previous ediff difference")

  (lisp/define-command 'ediff-quit
    ediff-quit!
    "Quit ediff")

  ;; Mode-specific keybindings for diff-mode (ediff results buffer)
  (lisp/define-key-for-mode :diff-mode "n" :ediff-next-difference)
  (lisp/define-key-for-mode :diff-mode "p" :ediff-previous-difference)
  (lisp/define-key-for-mode :diff-mode "q" :ediff-quit))
