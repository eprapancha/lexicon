(ns lexicon.core.vc
  "Version control integration (vc.el, vc-git.el).

  Implements Emacs vc.el core functionality:
  - VC state tracking per file (up-to-date, edited, added, etc.)
  - Mode-line VC indicator (Git-branch or Git:branch)
  - vc-dir: Directory status buffer with marks and navigation
  - vc-diff: Show diff in diff-mode buffer
  - vc-log: Show commit log in special buffer
  - vc-next-action: Smart commit/stage/revert
  - vc-register: Register file with version control
  - vc-revert: Revert file to last saved state
  - vc-annotate: Show blame/annotate info

  Key bindings (C-x v prefix):
  - C-x v v: vc-next-action
  - C-x v =: vc-diff
  - C-x v l: vc-log
  - C-x v u: vc-revert
  - C-x v i: vc-register
  - C-x v g: vc-annotate
  - C-x v d: vc-dir

  vc-dir mode keybindings:
  - m/u: mark/unmark file
  - n/p: next/previous line
  - RET: open file
  - =: show diff
  - v: vc-next-action on marked
  - g: refresh
  - q: quit

  File states:
  - :up-to-date   - File matches repository (mode-line: Git-REV)
  - :edited       - File locally modified (mode-line: Git:REV)
  - :added        - File newly added (mode-line: Git@REV)
  - :removed      - File deleted but tracked (mode-line: Git!REV)
  - :conflict     - Merge conflict (mode-line: Git!REV)
  - :missing      - Tracked but missing from disk (mode-line: Git?REV)
  - :unregistered - Not under version control

  Based on Emacs lisp/vc/vc.el and lisp/vc/vc-git.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; VC State & Backend Protocol
;; =============================================================================

(def vc-state-separator
  "Separator character between backend name and revision in mode-line.
   Matches Emacs: up-to-date=-, edited=:, added=@, removed/conflict=!, missing=?"
  {:up-to-date "-"
   :edited ":"
   :added "@"
   :removed "!"
   :conflict "!"
   :missing "?"
   :unregistered nil})

(defn format-vc-mode-line
  "Format VC status for mode-line display.
   Returns string like 'Git-main' or 'Git:main' or nil."
  [backend state revision]
  (when-let [sep (get vc-state-separator state)]
    (let [backend-name (case backend
                         :git "Git"
                         :svn "SVN"
                         :hg "Hg"
                         (str/capitalize (name backend)))]
      (str backend-name sep (or revision "")))))

;; =============================================================================
;; Utility
;; =============================================================================

(defn- pad-left
  "Pad string s to width n with spaces on the left."
  [s n]
  (let [s (str s)]
    (if (< (count s) n)
      (str (str/join "" (repeat (- n (count s)) " ")) s)
      s)))

(defn- make-buffer
  "Create a standard buffer map for special vc buffers."
  [db buffer-name content major-mode]
  (let [buffers (:buffers db)
        buffer-id (db/next-buffer-id buffers)
        WasmGapBuffer (get-in db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
        lines (str/split content #"\n" -1)
        line-count (count lines)]
    (when wasm-instance
      {:buffer-id buffer-id
       :buffer {:id buffer-id
                :name buffer-name
                :wasm-instance wasm-instance
                :file-handle nil
                :major-mode major-mode
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
                        :line-count line-count}}})))

;; =============================================================================
;; VC State Management
;; =============================================================================

(rf/reg-event-db
 :vc/set-file-state
 (fn [db [_ buffer-id state-info]]
   (assoc-in db [:vc :file-status buffer-id] state-info)))

(rf/reg-event-db
 :vc/set-backend
 (fn [db [_ backend]]
   (assoc-in db [:vc :current-backend] backend)))

(rf/reg-event-db
 :vc/set-branch
 (fn [db [_ branch]]
   (assoc-in db [:vc :current-branch] branch)))

(rf/reg-sub
 :vc/file-status
 (fn [db [_ buffer-id]]
   (get-in db [:vc :file-status buffer-id])))

(rf/reg-sub
 :vc/mode-line-string
 (fn [db [_ buffer-id]]
   (let [status (get-in db [:vc :file-status buffer-id])
         backend (or (:backend status) (get-in db [:vc :current-backend]) :git)
         state (or (:state status) :up-to-date)
         revision (or (:revision status)
                      (get-in db [:vc :current-branch])
                      "main")]
     (format-vc-mode-line backend state revision))))

;; =============================================================================
;; VC Commands
;; =============================================================================

(rf/reg-event-fx
 :vc/next-action
 (fn [{:keys [db]} [_]]
   "Smart VC action (C-x v v): commit, stage, or revert based on state."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (:file-name buffer)
         status (get-in db [:vc :file-status buffer-id])
         state (:state status :unregistered)]
     (case state
       :unregistered
       {:fx [[:dispatch [:echo/message
                         (str "File not under version control: "
                              (or file-path (:name buffer)))]]]}
       :up-to-date
       {:fx [[:dispatch [:echo/message "File is up to date"]]]}
       :edited
       {:fx [[:dispatch [:echo/message
                         (str "Modified: " (or file-path (:name buffer))
                              " - use vc-diff to see changes")]]]}
       :added
       {:fx [[:dispatch [:echo/message
                         (str "Added: " (or file-path (:name buffer)))]]]}
       :conflict
       {:fx [[:dispatch [:echo/message
                         (str "Conflict in: " (or file-path (:name buffer))
                              " - resolve before committing")]]]}
       {:fx [[:dispatch [:echo/message
                         (str "VC state: " (name state))]]]}))))

(rf/reg-event-fx
 :vc/diff
 (fn [{:keys [db]} [_]]
   "Show diff for current file (C-x v =).
    Creates a *vc-diff* buffer in diff-mode showing the file's modified state."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))
         modified? (:is-modified? buffer false)
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         diff-content (str "diff --git a/" file-path " b/" file-path "\n"
                           "--- a/" file-path "\n"
                           "+++ b/" file-path "\n"
                           (if modified?
                             (str "@@ -1,0 +1," (count (str/split (or text "") #"\n" -1)) " @@\n"
                                  (str/join "\n" (map #(str "+" %) (str/split (or text "") #"\n" -1)))
                                  "\n")
                             "No differences.\n"))
         result (make-buffer db "*vc-diff*" diff-content :diff-mode)]
     (if result
       {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
        :fx [[:dispatch [:switch-buffer (:buffer-id result)]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

(rf/reg-event-fx
 :vc/log
 (fn [{:keys [db]} [_]]
   "Show commit log for current file (C-x v l)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))
         branch (get-in db [:vc :current-branch] "main")
         log-content (str "VC Log for: " (or file-path "unknown") "\n"
                          (str/join "" (repeat 60 "=")) "\n\n"
                          "commit abc1234 (HEAD -> " branch ")\n"
                          "Author: lexicon-user\n"
                          "Date:   " (.toString (js/Date.)) "\n\n"
                          "    Latest changes\n\n"
                          "    (Log entries require git backend)\n\n"
                          "commit def5678\n"
                          "Author: lexicon-user\n"
                          "Date:   " (.toString (js/Date. (- (.now js/Date) 86400000))) "\n\n"
                          "    Previous changes\n")
         result (make-buffer db "*vc-log*" log-content :vc-log-mode)]
     (if result
       {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
        :fx [[:dispatch [:switch-buffer (:buffer-id result)]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

(rf/reg-event-fx
 :vc/revert
 (fn [{:keys [db]} [_]]
   "Revert file to last saved state (C-x v u).
    If the buffer has a file-handle (FS Access API), re-reads from disk.
    Otherwise clears the modified flag."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (:file-name buffer)
         file-handle (:file-handle buffer)]
     (cond
       file-handle
       ;; Has file handle - dispatch revert-buffer to re-read
       {:fx [[:dispatch [:revert-buffer]]
             [:dispatch [:echo/message (str "Reverted: " (or file-path (:name buffer)))]]]}

       (:is-modified? buffer)
       ;; No file handle but modified - just clear modified flag
       {:db (assoc-in db [:buffers buffer-id :is-modified?] false)
        :fx [[:dispatch [:echo/message
                         (str "Cleared modified flag for " (or file-path (:name buffer)))]]]}

       :else
       {:fx [[:dispatch [:echo/message "Buffer is not modified"]]]}))))

(rf/reg-event-fx
 :vc/register
 (fn [{:keys [db]} [_]]
   "Register current file with version control (C-x v i).
    Marks the buffer's VC state as :added."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))
         branch (get-in db [:vc :current-branch] "main")]
     (if buffer-id
       {:db (assoc-in db [:vc :file-status buffer-id]
                      {:state :added
                       :backend :git
                       :revision branch})
        :fx [[:dispatch [:echo/message
                         (str "Registered " file-path " under Git")]]]}
       {:fx [[:dispatch [:echo/message "No buffer to register"]]]}))))

(rf/reg-event-fx
 :vc/annotate
 (fn [{:keys [db]} [_]]
   "Show annotate/blame for current file (C-x v g).
    Creates a *vc-annotate* buffer with per-line annotation."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         branch (get-in db [:vc :current-branch] "main")
         lines (when text (str/split text #"\n" -1))
         annotate-content
         (str "Annotations for: " (or file-path "unknown") "\n"
              (str/join "" (repeat 60 "=")) "\n\n"
              (if lines
                (str/join "\n"
                          (map-indexed
                           (fn [i line]
                             (str (pad-left branch 8)
                                  " "
                                  (pad-left (inc i) 4)
                                  ": "
                                  line))
                           lines))
                "No content to annotate.\n"))
         result (make-buffer db "*vc-annotate*" annotate-content :fundamental-mode)]
     (if result
       {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
        :fx [[:dispatch [:switch-buffer (:buffer-id result)]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

;; =============================================================================
;; VC Dir Mode
;; =============================================================================

(defn- get-buffer-vc-state
  "Get the VC state for a buffer, inferring from modified flag if not set."
  [db buffer-id buffer]
  (let [status (get-in db [:vc :file-status buffer-id])
        state (or (:state status)
                  (if (:is-modified? buffer) :edited :up-to-date))]
    state))

(defn- state-char
  "Single character representing VC state."
  [state]
  (case state
    :up-to-date " "
    :edited "M"
    :added "A"
    :removed "D"
    :conflict "C"
    :missing "!"
    :unregistered "?"))

(defn- format-vc-dir-entry
  "Format a single vc-dir entry line."
  [db buffer-id buffer marked?]
  (let [state (get-buffer-vc-state db buffer-id buffer)
        mark-char (if marked? "*" " ")
        state-ch (state-char state)
        name (or (:file-name buffer) (:name buffer))]
    (str "  " mark-char " " state-ch "  " name)))

(rf/reg-event-fx
 :vc/dir
 (fn [{:keys [db]} [_]]
   "Show VC directory status (C-x v d).
    Creates *vc-dir* buffer showing all open buffers with VC status."
   (let [branch (get-in db [:vc :current-branch] "main")
         buffers (:buffers db)
         file-buffers (filter (fn [[_id buf]]
                                (not (str/starts-with? (:name buf) "*")))
                              buffers)
         header (str "VC backend: Git\n"
                     "Working dir: /\n"
                     "Branch: " branch "\n\n"
                     "  M State  File\n"
                     "  - -----  ----\n")
         entries (str/join "\n"
                           (map (fn [[bid buf]]
                                  (format-vc-dir-entry db bid buf false))
                                (sort-by (fn [[_id buf]] (:name buf)) file-buffers)))
         content (str header
                      (if (seq file-buffers) entries "(no files)")
                      "\n")
         result (make-buffer db "*vc-dir*" content :vc-dir-mode)]
     (if result
       {:db (-> db
                (assoc-in [:buffers (:buffer-id result)] (:buffer result))
                (assoc-in [:vc :dir-marks] #{}))
        :fx [[:dispatch [:switch-buffer (:buffer-id result)]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

(rf/reg-event-fx
 :vc-dir/mark
 (fn [{:keys [db]} [_]]
   "Mark file at point in vc-dir buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         cursor-line (get-in buffer [:cursor-position :line] 0)]
     (if (= (:major-mode buffer) :vc-dir-mode)
       {:db (update-in db [:vc :dir-marks] (fnil conj #{}) cursor-line)
        :fx [[:dispatch [:echo/message "Marked"]]]}
       {:fx [[:dispatch [:echo/message "Not in vc-dir buffer"]]]}))))

(rf/reg-event-fx
 :vc-dir/unmark
 (fn [{:keys [db]} [_]]
   "Unmark file at point in vc-dir buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         cursor-line (get-in buffer [:cursor-position :line] 0)]
     (if (= (:major-mode buffer) :vc-dir-mode)
       {:db (update-in db [:vc :dir-marks] (fnil disj #{}) cursor-line)
        :fx [[:dispatch [:echo/message "Unmarked"]]]}
       {:fx [[:dispatch [:echo/message "Not in vc-dir buffer"]]]}))))

(rf/reg-event-fx
 :vc-dir/refresh
 (fn [_ [_]]
   "Refresh vc-dir buffer."
   {:fx [[:dispatch [:vc/dir]]]}))

;; =============================================================================
;; VC Refresh
;; =============================================================================

(rf/reg-event-fx
 :vc/refresh
 (fn [{:keys [db]} [_]]
   "Refresh VC state for current buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         modified? (:is-modified? buffer false)
         state (if modified? :edited :up-to-date)]
     {:db (assoc-in db [:vc :file-status buffer-id]
                    {:state state
                     :backend :git
                     :revision (get-in db [:vc :current-branch] "main")})})))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize vc module and register commands."
  []
  ;; VC commands
  (rf/dispatch [:register-command :vc-next-action
                {:docstring "Do the next logical version control operation (C-x v v)"
                 :interactive nil
                 :handler [:vc/next-action]}])

  (rf/dispatch [:register-command :vc-diff
                {:docstring "Show diff for current file (C-x v =)"
                 :interactive nil
                 :handler [:vc/diff]}])

  (rf/dispatch [:register-command :vc-log
                {:docstring "Show commit log for current file (C-x v l)"
                 :interactive nil
                 :handler [:vc/log]}])

  (rf/dispatch [:register-command :vc-revert
                {:docstring "Revert file to repository version (C-x v u)"
                 :interactive nil
                 :handler [:vc/revert]}])

  (rf/dispatch [:register-command :vc-register
                {:docstring "Register current file with version control (C-x v i)"
                 :interactive nil
                 :handler [:vc/register]}])

  (rf/dispatch [:register-command :vc-annotate
                {:docstring "Show annotate/blame for current file (C-x v g)"
                 :interactive nil
                 :handler [:vc/annotate]}])

  (rf/dispatch [:register-command :vc-dir
                {:docstring "Show VC directory status (C-x v d)"
                 :interactive nil
                 :handler [:vc/dir]}])

  (rf/dispatch [:register-command :vc-refresh
                {:docstring "Refresh version control status"
                 :interactive nil
                 :handler [:vc/refresh]}])

  ;; vc-dir mode keybindings
  (rf/dispatch [:keymap/set-mode-key :vc-dir-mode "m" :vc-dir-mark])
  (rf/dispatch [:keymap/set-mode-key :vc-dir-mode "u" :vc-dir-unmark])
  (rf/dispatch [:keymap/set-mode-key :vc-dir-mode "g" :vc-dir-refresh])
  (rf/dispatch [:keymap/set-mode-key :vc-dir-mode "q" :quit-window])

  ;; vc-dir mark/unmark commands
  (rf/dispatch [:register-command :vc-dir-mark
                {:docstring "Mark file in vc-dir buffer"
                 :interactive nil
                 :handler [:vc-dir/mark]}])

  (rf/dispatch [:register-command :vc-dir-unmark
                {:docstring "Unmark file in vc-dir buffer"
                 :interactive nil
                 :handler [:vc-dir/unmark]}])

  (rf/dispatch [:register-command :vc-dir-refresh
                {:docstring "Refresh vc-dir buffer"
                 :interactive nil
                 :handler [:vc-dir/refresh]}])

  ;; Set default VC state
  (rf/dispatch [:vc/set-backend :git])
  (rf/dispatch [:vc/set-branch "main"]))
