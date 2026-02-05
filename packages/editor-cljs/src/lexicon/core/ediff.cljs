(ns lexicon.core.ediff
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

  Based on Emacs lisp/vc/ediff.el and lisp/vc/smerge-mode.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

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
;; Smerge Events
;; =============================================================================

(rf/reg-event-fx
 :smerge/enable
 (fn [{:keys [db]} [_]]
   "Enable smerge-mode for current buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         conflicts (when text (parse-conflicts text))]
     (if (seq conflicts)
       {:db (-> db
                (update-in [:buffers buffer-id :minor-modes] (fnil conj #{}) :smerge-mode)
                (assoc-in [:smerge buffer-id :conflicts] conflicts)
                (assoc-in [:smerge buffer-id :current-index] 0))
        :fx [[:dispatch [:echo/message
                         (str "smerge-mode enabled: " (count conflicts) " conflict"
                              (when (not= 1 (count conflicts)) "s") " found")]]]}
       {:fx [[:dispatch [:echo/message "No conflict markers found"]]]}))))

(rf/reg-event-fx
 :smerge/next
 (fn [{:keys [db]} [_]]
   "Go to next conflict (C-c ^ n)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         conflicts (get-in db [:smerge buffer-id :conflicts] [])
         idx (get-in db [:smerge buffer-id :current-index] 0)
         new-idx (min (inc idx) (dec (max 1 (count conflicts))))]
     (if (seq conflicts)
       (let [conflict (nth conflicts new-idx)]
         {:db (assoc-in db [:smerge buffer-id :current-index] new-idx)
          :fx [[:dispatch [:echo/message
                           (str "Conflict " (inc new-idx) "/" (count conflicts)
                                " (line " (inc (:start-line conflict)) ")")]]]})
       {:fx [[:dispatch [:echo/message "No conflicts"]]]}))))

(rf/reg-event-fx
 :smerge/prev
 (fn [{:keys [db]} [_]]
   "Go to previous conflict (C-c ^ p)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         conflicts (get-in db [:smerge buffer-id :conflicts] [])
         idx (get-in db [:smerge buffer-id :current-index] 0)
         new-idx (max (dec idx) 0)]
     (if (seq conflicts)
       (let [conflict (nth conflicts new-idx)]
         {:db (assoc-in db [:smerge buffer-id :current-index] new-idx)
          :fx [[:dispatch [:echo/message
                           (str "Conflict " (inc new-idx) "/" (count conflicts)
                                " (line " (inc (:start-line conflict)) ")")]]]})
       {:fx [[:dispatch [:echo/message "No conflicts"]]]}))))

(rf/reg-event-fx
 :smerge/keep-upper
 (fn [{:keys [db]} [_]]
   "Keep upper (mine) version of current conflict (C-c ^ u)."
   {:fx [[:dispatch [:smerge/resolve-current :upper]]]}))

(rf/reg-event-fx
 :smerge/keep-lower
 (fn [{:keys [db]} [_]]
   "Keep lower (other) version of current conflict (C-c ^ l)."
   {:fx [[:dispatch [:smerge/resolve-current :lower]]]}))

(rf/reg-event-fx
 :smerge/keep-base
 (fn [{:keys [db]} [_]]
   "Keep base version of current conflict (C-c ^ b)."
   {:fx [[:dispatch [:smerge/resolve-current :base]]]}))

(rf/reg-event-fx
 :smerge/keep-all
 (fn [{:keys [db]} [_]]
   "Keep all versions of current conflict (C-c ^ a)."
   {:fx [[:dispatch [:smerge/resolve-current :all]]]}))

(rf/reg-event-fx
 :smerge/resolve-current
 (fn [{:keys [db]} [_ resolution]]
   "Resolve the current conflict with the given resolution."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         conflicts (get-in db [:smerge buffer-id :conflicts] [])
         idx (get-in db [:smerge buffer-id :current-index] 0)]
     (if (and (seq conflicts) (< idx (count conflicts)))
       (let [conflict (nth conflicts idx)
             ^js wasm (get-in db [:buffers buffer-id :wasm-instance])
             text (when wasm (try (.getText wasm) (catch :default _ "")))
             new-text (when text (resolve-conflict text conflict resolution))]
         (when (and wasm new-text)
           (let [len (.-length wasm)]
             (.delete wasm 0 len)
             (.insert wasm 0 new-text))
           ;; Re-parse conflicts after resolution
           (let [remaining (parse-conflicts new-text)
                 lines (str/split new-text #"\n" -1)]
             {:db (-> db
                      (assoc-in [:buffers buffer-id :cache :text] new-text)
                      (assoc-in [:buffers buffer-id :cache :line-count] (count lines))
                      (assoc-in [:buffers buffer-id :is-modified?] true)
                      (assoc-in [:smerge buffer-id :conflicts] remaining)
                      (assoc-in [:smerge buffer-id :current-index]
                                (min idx (dec (max 1 (count remaining))))))
              :fx [[:dispatch [:echo/message
                               (str "Conflict resolved (" (name resolution) "). "
                                    (count remaining) " remaining")]]]})))
       {:fx [[:dispatch [:echo/message "No conflict at point"]]]}))))

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

(rf/reg-event-fx
 :ediff/buffers
 (fn [{:keys [db]} [_]]
   "Compare two buffers (M-x ediff-buffers)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Ediff buffer A: "
                      :on-confirm [:ediff/set-buffer-a]}]]]}))

(rf/reg-event-fx
 :ediff/set-buffer-a
 (fn [{:keys [db]} [_ buf-name-a]]
   "Set buffer A for ediff and prompt for buffer B."
   {:db (assoc-in db [:ediff :buffer-a-name] buf-name-a)
    :fx [[:dispatch [:minibuffer/activate
                     {:prompt "Ediff buffer B: "
                      :on-confirm [:ediff/run]}]]]}))

(rf/reg-event-fx
 :ediff/run
 (fn [{:keys [db]} [_ buf-name-b]]
   "Run ediff comparison between buffer A and B."
   (let [buf-name-a (get-in db [:ediff :buffer-a-name])
         buf-a (first (filter #(= (:name (val %)) buf-name-a) (:buffers db)))
         buf-b (first (filter #(= (:name (val %)) buf-name-b) (:buffers db)))]
     (if (and buf-a buf-b)
       (let [wasm-a (:wasm-instance (val buf-a))
             wasm-b (:wasm-instance (val buf-b))
             text-a (when wasm-a (try (.getText ^js wasm-a) (catch :default _ "")))
             text-b (when wasm-b (try (.getText ^js wasm-b) (catch :default _ "")))
             diffs (compute-line-diff (or text-a "") (or text-b ""))
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
                         diff-count " difference" (when (not= 1 diff-count) "s") " found.\n")
             buffers (:buffers db)
             buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. output))
             lines (str/split output #"\n" -1)
             line-count (count lines)]
         (if-not wasm-instance
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
           {:db (-> db
                    (assoc-in [:buffers buffer-id]
                              {:id buffer-id
                               :name "*ediff*"
                               :wasm-instance wasm-instance
                               :file-handle nil
                               :major-mode :diff-mode
                               :is-read-only? true
                               :is-modified? false
                               :mark-position nil
                               :cursor-position {:line 0 :column 0}
                               :selection-range nil
                               :minor-modes #{}
                               :buffer-local-vars {}
                               :ast nil
                               :language :text
                               :diagnostics []
                               :undo-stack []
                               :undo-in-progress? false
                               :editor-version 0
                               :text-properties {}
                               :overlays {}
                               :next-overlay-id 1
                               :cache {:text output
                                       :line-count line-count}})
                    (assoc-in [:ediff :diffs] diffs)
                    (assoc-in [:ediff :diff-index] 0))
            :fx [[:dispatch [:switch-buffer buffer-id]]
                 [:dispatch [:echo/message
                             (str diff-count " difference"
                                  (when (not= 1 diff-count) "s") " found")]]]}))
       {:fx [[:dispatch [:echo/message
                         (str "Buffer not found: "
                              (if buf-a buf-name-b buf-name-a))]]]}))))

;; =============================================================================
;; Ediff Navigation
;; =============================================================================

(rf/reg-event-fx
 :ediff/next-diff
 (fn [{:keys [db]} [_]]
   "Go to next difference."
   (let [diffs (get-in db [:ediff :diffs] [])
         idx (get-in db [:ediff :diff-index] 0)
         ;; Find next non-same diff
         remaining (drop (inc idx) diffs)
         next-diff-offset (first (keep-indexed
                                   (fn [i d] (when (not= :same (:type d)) i))
                                   remaining))]
     (if next-diff-offset
       (let [new-idx (+ (inc idx) next-diff-offset)]
         {:db (assoc-in db [:ediff :diff-index] new-idx)
          :fx [[:dispatch [:echo/message (str "Difference at line " new-idx)]]]})
       {:fx [[:dispatch [:echo/message "No more differences"]]]}))))

(rf/reg-event-fx
 :ediff/prev-diff
 (fn [{:keys [db]} [_]]
   "Go to previous difference."
   (let [diffs (get-in db [:ediff :diffs] [])
         idx (get-in db [:ediff :diff-index] 0)
         before (take idx diffs)
         prev-diff-offset (last (keep-indexed
                                  (fn [i d] (when (not= :same (:type d)) i))
                                  before))]
     (if prev-diff-offset
       {:db (assoc-in db [:ediff :diff-index] prev-diff-offset)
        :fx [[:dispatch [:echo/message (str "Difference at line " prev-diff-offset)]]]}
       {:fx [[:dispatch [:echo/message "No previous differences"]]]}))))

(rf/reg-event-fx
 :ediff/quit
 (fn [_ [_]]
   "Quit ediff."
   {:fx [[:dispatch [:kill-buffer-by-name "*ediff*"]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize ediff and smerge-mode."
  []
  ;; Ediff commands
  (rf/dispatch [:register-command :ediff-buffers
                {:docstring "Compare two buffers side-by-side"
                 :interactive nil
                 :handler [:ediff/buffers]}])

  ;; Smerge commands
  (rf/dispatch [:register-command :smerge-mode
                {:docstring "Toggle smerge minor mode for conflict resolution"
                 :interactive nil
                 :handler [:smerge/enable]}])

  (rf/dispatch [:register-command :smerge-next
                {:docstring "Go to next conflict (C-c ^ n)"
                 :interactive nil
                 :handler [:smerge/next]}])

  (rf/dispatch [:register-command :smerge-prev
                {:docstring "Go to previous conflict (C-c ^ p)"
                 :interactive nil
                 :handler [:smerge/prev]}])

  (rf/dispatch [:register-command :smerge-keep-upper
                {:docstring "Keep upper (mine) version"
                 :interactive nil
                 :handler [:smerge/keep-upper]}])

  (rf/dispatch [:register-command :smerge-keep-lower
                {:docstring "Keep lower (other) version"
                 :interactive nil
                 :handler [:smerge/keep-lower]}])

  (rf/dispatch [:register-command :smerge-keep-base
                {:docstring "Keep base version"
                 :interactive nil
                 :handler [:smerge/keep-base]}])

  (rf/dispatch [:register-command :smerge-keep-all
                {:docstring "Keep all versions"
                 :interactive nil
                 :handler [:smerge/keep-all]}]))
