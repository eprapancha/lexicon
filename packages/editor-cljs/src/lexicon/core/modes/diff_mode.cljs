(ns lexicon.core.modes.diff-mode
  "Diff mode - view and navigate unified diffs.

  Implements Emacs diff-mode.el core functionality:
  - Parse unified diff format
  - Navigate between hunks and files
  - Highlight added/removed/context lines
  - Kill hunks or file diffs

  Commands:
  - diff-hunk-next / diff-hunk-prev: Navigate between hunks
  - diff-file-next / diff-file-prev: Navigate between files
  - diff-goto-source: Jump to source location
  - diff-hunk-kill: Remove current hunk

  Key bindings:
  - n/p: Next/previous hunk
  - N/P: Next/previous file
  - RET: Go to source
  - k: Kill current hunk
  - q: Quit
  - g: Revert

  Based on Emacs lisp/vc/diff-mode.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Diff Format Patterns
;; =============================================================================

(def unified-hunk-header-re
  "Regex for unified diff hunk headers: @@ -10,5 +12,8 @@"
  #"^@@\s+-\d+(?:,\d+)?\s+\+\d+(?:,\d+)?\s+@@")

;; =============================================================================
;; Diff Parsing
;; =============================================================================

(defn- parse-hunk-header
  "Parse a unified diff hunk header line.
   Returns {:old-start :old-count :new-start :new-count} or nil."
  [line]
  (when-let [match (re-find #"@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@" line)]
    {:old-start (js/parseInt (nth match 1))
     :old-count (if (nth match 2) (js/parseInt (nth match 2)) 1)
     :new-start (js/parseInt (nth match 3))
     :new-count (if (nth match 4) (js/parseInt (nth match 4)) 1)}))

(defn- parse-file-header
  "Extract filename from diff file header.
   Handles 'diff --git a/file b/file', '--- a/file', '+++ b/file'."
  [line]
  (or
   (when-let [match (re-find #"^diff --git a/(.*) b/(.*)" line)]
     {:old-file (nth match 1) :new-file (nth match 2)})
   (when-let [match (re-find #"^--- (?:a/)?(.*)" line)]
     {:old-file (nth match 1)})
   (when-let [match (re-find #"^\+\+\+ (?:b/)?(.*)" line)]
     {:new-file (nth match 1)})))

;; =============================================================================
;; Navigation Helpers
;; =============================================================================

(defn- get-buffer-lines
  "Get buffer text split into lines."
  [db buffer-id]
  (let [wasm (get-in db [:buffers buffer-id :wasm-instance])]
    (when wasm
      (let [text (try (.getText wasm) (catch :default _ ""))]
        (str/split text #"\n")))))

(defn- cursor-line
  "Get current cursor line number (0-based)."
  [db]
  (let [cursor-pos (get-in db [:ui :cursor-position] 0)
        window (get-in db [:window-tree])
        buffer-id (when (= (:type window) :leaf) (:buffer-id window))
        wasm (get-in db [:buffers buffer-id :wasm-instance])
        text (when wasm (try (.getText wasm) (catch :default _ "")))]
    (when text
      (count (filter #(= % \newline) (take cursor-pos text))))))

(defn- line-start-position
  "Get the character position of the start of a given line (0-based line number)."
  [text line-num]
  (loop [pos 0 current-line 0]
    (if (= current-line line-num)
      pos
      (if (>= pos (count text))
        pos
        (if (= (nth text pos) \newline)
          (recur (inc pos) (inc current-line))
          (recur (inc pos) current-line))))))

;; =============================================================================
;; Re-frame Events - Navigation
;; =============================================================================

(rf/reg-event-fx
 :diff/hunk-next
 (fn [{:keys [db]} [_]]
   "Move to the next hunk header."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       (let [next-hunk (first (keep-indexed
                               (fn [idx line]
                                 (when (and (> idx current)
                                            (re-find unified-hunk-header-re line))
                                   idx))
                               lines))]
         (if next-hunk
           (let [text (str/join "\n" lines)
                 pos (line-start-position text next-hunk)]
             {:fx [[:dispatch [:goto-char buffer-id pos]]]})
           {:fx [[:dispatch [:echo/message "No more hunks"]]]}))))))

(rf/reg-event-fx
 :diff/hunk-prev
 (fn [{:keys [db]} [_]]
   "Move to the previous hunk header."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       (let [prev-hunk (last (keep-indexed
                              (fn [idx line]
                                (when (and (< idx current)
                                           (re-find unified-hunk-header-re line))
                                  idx))
                              lines))]
         (if prev-hunk
           (let [text (str/join "\n" lines)
                 pos (line-start-position text prev-hunk)]
             {:fx [[:dispatch [:goto-char buffer-id pos]]]})
           {:fx [[:dispatch [:echo/message "No previous hunk"]]]}))))))

(rf/reg-event-fx
 :diff/file-next
 (fn [{:keys [db]} [_]]
   "Move to the next file header."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       (let [next-file (first (keep-indexed
                               (fn [idx line]
                                 (when (and (> idx current)
                                            (re-find #"^diff --git" line))
                                   idx))
                               lines))]
         (if next-file
           (let [text (str/join "\n" lines)
                 pos (line-start-position text next-file)]
             {:fx [[:dispatch [:goto-char buffer-id pos]]]})
           {:fx [[:dispatch [:echo/message "No more files"]]]}))))))

(rf/reg-event-fx
 :diff/file-prev
 (fn [{:keys [db]} [_]]
   "Move to the previous file header."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       (let [prev-file (last (keep-indexed
                              (fn [idx line]
                                (when (and (< idx current)
                                           (re-find #"^diff --git" line))
                                  idx))
                              lines))]
         (if prev-file
           (let [text (str/join "\n" lines)
                 pos (line-start-position text prev-file)]
             {:fx [[:dispatch [:goto-char buffer-id pos]]]})
           {:fx [[:dispatch [:echo/message "No previous file"]]]}))))))

;; =============================================================================
;; Re-frame Events - Operations
;; =============================================================================

(rf/reg-event-fx
 :diff/goto-source
 (fn [{:keys [db]} [_]]
   "Jump to the corresponding source location."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       ;; Find the file and hunk info for this line
       (let [;; Find nearest file header before current line
             file-info (last (keep-indexed
                              (fn [idx line]
                                (when (<= idx current)
                                  (parse-file-header line)))
                              lines))
             ;; Find nearest hunk header before current line
             hunk-line (last (keep-indexed
                              (fn [idx line]
                                (when (and (<= idx current)
                                           (re-find unified-hunk-header-re line))
                                  idx))
                              lines))
             hunk-info (when hunk-line
                         (parse-hunk-header (nth lines hunk-line)))
             filename (or (:new-file file-info) (:old-file file-info))]
         (if (and filename hunk-info)
           (let [;; Calculate line offset from hunk start
                 lines-from-hunk (- current hunk-line 1)
                 target-line (+ (:new-start hunk-info) (max 0 lines-from-hunk))]
             {:fx [[:dispatch [:echo/message
                               (str filename ":" target-line)]]]})
           {:fx [[:dispatch [:echo/message "Cannot determine source location"]]]}))))))

(rf/reg-event-fx
 :diff/hunk-kill
 (fn [{:keys [db]} [_]]
   "Kill (remove) the current hunk from the diff."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         lines (get-buffer-lines db buffer-id)
         current (cursor-line db)]
     (when (and lines current)
       (let [;; Find current hunk boundaries
             hunk-start (or (last (keep-indexed
                                   (fn [idx line]
                                     (when (and (<= idx current)
                                                (re-find unified-hunk-header-re line))
                                       idx))
                                   lines))
                            current)
             hunk-end (or (first (keep-indexed
                                  (fn [idx line]
                                    (when (and (> idx hunk-start)
                                              (or (re-find unified-hunk-header-re line)
                                                  (re-find #"^diff --git" line)))
                                      idx))
                                  lines))
                          (count lines))
             new-lines (concat (take hunk-start lines)
                               (drop hunk-end lines))
             new-text (str/join "\n" new-lines)
             ^js wasm (get-in db [:buffers buffer-id :wasm-instance])]
         (when wasm
           (let [current-len (.-length wasm)]
             (.delete wasm 0 current-len)
             (.insert wasm 0 new-text))
           {:db (-> db
                    (assoc-in [:buffers buffer-id :cache :text] new-text)
                    (assoc-in [:buffers buffer-id :cache :line-count]
                              (count (str/split new-text #"\n" -1))))
            :fx [[:dispatch [:echo/message "Hunk killed"]]]}))))))

;; =============================================================================
;; Mode Activation
;; =============================================================================

(rf/reg-event-fx
 :diff-mode/activate
 (fn [{:keys [db]} [_ buffer-id]]
   "Activate diff-mode for a buffer."
   {:db (-> db
            (assoc-in [:buffers buffer-id :major-mode] :diff-mode)
            (assoc-in [:buffers buffer-id :is-read-only?] true))}))

;; =============================================================================
;; Open Diff Buffer
;; =============================================================================

(rf/reg-event-fx
 :diff/open-buffer
 (fn [{:keys [db]} [_ content name]]
   "Create a new diff buffer with the given content."
   (let [buffers (:buffers db)
         buffer-id (db/next-buffer-id buffers)
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
         lines (str/split content #"\n" -1)
         line-count (count lines)]
     (if-not wasm-instance
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
       {:db (assoc-in db [:buffers buffer-id]
                       {:id buffer-id
                        :name (or name "*diff*")
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
                        :cache {:text content
                                :line-count line-count}})
        :fx [[:dispatch [:switch-buffer buffer-id]]]}))))

;; =============================================================================
;; Quit
;; =============================================================================

(rf/reg-event-fx
 :diff/quit
 (fn [{:keys [_db]} [_]]
   {:fx [[:dispatch [:kill-buffer-by-name ""]]]}))

;; =============================================================================
;; Commands & Initialization
;; =============================================================================

(defn init!
  "Initialize diff-mode module and register commands."
  []
  ;; Navigation commands
  (rf/dispatch [:register-command :diff-hunk-next
                {:docstring "Move to next diff hunk"
                 :interactive nil
                 :handler [:diff/hunk-next]}])

  (rf/dispatch [:register-command :diff-hunk-prev
                {:docstring "Move to previous diff hunk"
                 :interactive nil
                 :handler [:diff/hunk-prev]}])

  (rf/dispatch [:register-command :diff-file-next
                {:docstring "Move to next diff file"
                 :interactive nil
                 :handler [:diff/file-next]}])

  (rf/dispatch [:register-command :diff-file-prev
                {:docstring "Move to previous diff file"
                 :interactive nil
                 :handler [:diff/file-prev]}])

  ;; Operation commands
  (rf/dispatch [:register-command :diff-goto-source
                {:docstring "Jump to source location for current diff line"
                 :interactive nil
                 :handler [:diff/goto-source]}])

  (rf/dispatch [:register-command :diff-hunk-kill
                {:docstring "Kill current diff hunk"
                 :interactive nil
                 :handler [:diff/hunk-kill]}])

  ;; Mode key bindings
  (rf/dispatch [:keymap/set-mode-key :diff-mode "n" :diff-hunk-next])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "p" :diff-hunk-prev])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "N" :diff-file-next])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "P" :diff-file-prev])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "RET" :diff-goto-source])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "k" :diff-hunk-kill])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "q" :diff/quit])
  (rf/dispatch [:keymap/set-mode-key :diff-mode "g" :revert-buffer])

  ;; Register diff-mode command
  (rf/dispatch [:register-command :diff-mode
                {:docstring "Major mode for viewing diffs"
                 :interactive nil
                 :handler [:diff-mode/activate]}]))
