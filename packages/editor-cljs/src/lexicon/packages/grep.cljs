(ns lexicon.packages.grep
  "Grep search integration (grep.el).

  Implements Emacs grep.el core functionality:
  - grep: Run grep with specified pattern
  - lgrep: Grep in a directory (searches open buffers)
  - rgrep: Recursive grep (searches all open buffers)
  - grep-find: Grep via find

  Output goes to *grep* buffer in grep-mode with navigation:
  - n/M-n: Next match
  - p/M-p: Previous match
  - Enter: Go to match
  - g: Re-run grep
  - q: Quit grep buffer

  In browser context, searches across open buffers since
  filesystem grep is not available.

  Based on Emacs lisp/progmodes/grep.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce grep-state (atom {:last-pattern nil
                           :matches []
                           :match-index 0}))

;; =============================================================================
;; Grep Engine - Search across open buffers
;; =============================================================================

(defn- search-buffer
  "Search a single buffer for pattern matches.
   Returns vector of {:buffer-name :line-num :line-text :match}."
  [buffer-name text pattern-re]
  (let [lines (str/split text #"\n" -1)]
    (reduce-kv
     (fn [results idx line]
       (if-let [match (re-find pattern-re line)]
         (conj results {:buffer-name buffer-name
                        :line-num (inc idx)
                        :line-text line
                        :match (if (string? match) match (first match))})
         results))
     []
     (vec lines))))

(defn- run-grep
  "Run grep across all open buffers. Returns formatted output and match count."
  [pattern]
  (let [buffers (lisp/buffer-list)
        pattern-re (try (js/RegExp. pattern "g")
                        (catch :default _ nil))]
    (if-not pattern-re
      {:output (str "grep: invalid regexp: " pattern "\n")
       :matches []
       :count 0}
      (let [all-matches
            (reduce
             (fn [acc buffer-info]
               (let [buffer-id (:id buffer-info)
                     name (:name buffer-info)
                     text (lisp/buffer-text-of buffer-id)]
                 (if (and text
                          (not (str/starts-with? name "*"))
                          (seq text))
                   (let [matches (search-buffer name text pattern-re)]
                     (into acc matches))
                   acc)))
             []
             buffers)
            output (if (seq all-matches)
                     (str "Grep results for: " pattern "\n\n"
                          (str/join "\n"
                                    (map (fn [m]
                                           (str (:buffer-name m) ":"
                                                (:line-num m) ":"
                                                (:line-text m)))
                                         all-matches))
                          "\n\n"
                          "Grep finished with " (count all-matches) " match"
                          (when (not= 1 (count all-matches)) "es")
                          " found.\n")
                     (str "Grep results for: " pattern "\n\n"
                          "Grep finished with 0 matches found.\n"))]
        {:output output
         :matches all-matches
         :count (count all-matches)}))))

;; =============================================================================
;; Grep Buffer Management
;; =============================================================================

(defn grep-run!
  "Run grep and display results in *grep* buffer."
  [pattern]
  (let [{:keys [output matches count]} (run-grep pattern)
        existing-id (lisp/get-buffer "*grep*")]
    ;; Update grep state
    (swap! grep-state assoc
           :last-pattern pattern
           :matches matches
           :match-index 0)

    ;; Kill existing buffer if any, then create new
    (when existing-id
      (lisp/kill-buffer existing-id))

    ;; Create *grep* buffer
    (let [_buffer-id (lisp/create-special-buffer "*grep*" output
                                                  {:major-mode :grep-mode
                                                   :read-only true})]
      (lisp/split-window-below)
      (lisp/switch-to-buffer "*grep*")
      (lisp/message (str "Grep finished: " count " match"
                        (when (not= 1 count) "es") " found")))))

;; =============================================================================
;; Grep Navigation
;; =============================================================================

(defn grep-next-match!
  "Go to next grep match."
  []
  (let [{:keys [matches match-index]} @grep-state
        new-idx (min (inc match-index) (dec (count matches)))]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (swap! grep-state assoc :match-index new-idx)
        (lisp/message (str (:buffer-name match) ":"
                          (:line-num match) ": "
                          (subs (:line-text match) 0
                                (min 60 (count (:line-text match)))))))
      (lisp/message "No grep matches"))))

(defn grep-prev-match!
  "Go to previous grep match."
  []
  (let [{:keys [matches match-index]} @grep-state
        new-idx (max (dec match-index) 0)]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (swap! grep-state assoc :match-index new-idx)
        (lisp/message (str (:buffer-name match) ":"
                          (:line-num match) ": "
                          (subs (:line-text match) 0
                                (min 60 (count (:line-text match)))))))
      (lisp/message "No grep matches"))))

(defn grep-goto-match!
  "Jump to current grep match in source buffer."
  []
  (let [{:keys [matches match-index]} @grep-state]
    (if (and (seq matches) (< match-index (count matches)))
      (let [match (nth matches match-index)
            target-name (:buffer-name match)
            buffers (lisp/buffer-list)
            target-buffer (first (filter #(= (:name %) target-name) buffers))]
        (if target-buffer
          (do
            (lisp/switch-to-buffer (:id target-buffer))
            (lisp/goto-line (:line-num match))
            (lisp/message (str "Line " (:line-num match))))
          (lisp/message (str "Buffer " target-name " not found"))))
      (lisp/message "No match to visit"))))

(defn grep-rerun!
  "Re-run the last grep."
  []
  (if-let [pattern (:last-pattern @grep-state)]
    (grep-run! pattern)
    (lisp/message "No previous grep to re-run")))

(defn grep-quit!
  "Quit grep buffer."
  []
  (when-let [grep-id (lisp/get-buffer "*grep*")]
    (lisp/delete-window)
    (lisp/kill-buffer grep-id)))

;; =============================================================================
;; Interactive Commands
;; =============================================================================

(defn grep-interactive []
  (lisp/read-from-minibuffer
   "Grep (regexp): "
   grep-run!))

(defn lgrep-interactive []
  (lisp/read-from-minibuffer
   "Local grep (regexp): "
   grep-run!))

(defn rgrep-interactive []
  (lisp/read-from-minibuffer
   "Recursive grep (regexp): "
   grep-run!))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-grep!
  "Initialize grep module and register commands."
  []
  (lisp/define-command 'grep
    grep-interactive
    "Run grep and display results in *grep* buffer")

  (lisp/define-command 'lgrep
    lgrep-interactive
    "Local grep - search in open buffers")

  (lisp/define-command 'rgrep
    rgrep-interactive
    "Recursive grep - search all open buffers")

  (lisp/define-command 'grep-find
    grep-interactive
    "Run grep via find (searches open buffers)")

  ;; Navigation commands
  (lisp/define-command 'next-error
    grep-next-match!
    "Go to next grep/compilation match")

  (lisp/define-command 'previous-error
    grep-prev-match!
    "Go to previous grep/compilation match")

  ;; grep-mode keybindings
  (lisp/define-key-for-mode :grep-mode "n" :next-error)
  (lisp/define-key-for-mode :grep-mode "p" :previous-error)
  (lisp/define-key-for-mode :grep-mode "RET" :grep-goto-match)
  (lisp/define-key-for-mode :grep-mode "g" :grep-rerun)
  (lisp/define-key-for-mode :grep-mode "q" :grep-quit)

  ;; Register additional commands for mode
  (lisp/define-command 'grep-goto-match
    grep-goto-match!
    "Jump to current grep match")

  (lisp/define-command 'grep-rerun
    grep-rerun!
    "Re-run the last grep")

  (lisp/define-command 'grep-quit
    grep-quit!
    "Quit grep buffer"))
