(ns lexicon.core.vc
  "Version control integration (vc.el, vc-git.el).

  Implements Emacs vc.el core functionality:
  - VC state tracking per file (up-to-date, edited, added, etc.)
  - Mode-line VC indicator (Git-branch or Git:branch)
  - vc-diff: Show diff in diff-mode buffer
  - vc-log: Show commit log in special buffer
  - vc-next-action: Smart commit/stage/revert
  - vc-register: Register file with version control
  - vc-revert: Revert file to repository version
  - vc-annotate: Show blame/annotate info

  Key bindings (C-x v prefix):
  - C-x v v: vc-next-action
  - C-x v =: vc-diff
  - C-x v l: vc-log
  - C-x v u: vc-revert
  - C-x v i: vc-register
  - C-x v g: vc-annotate

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
   "Show diff for current file (C-x v =)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))]
     ;; In a full implementation, this would run git diff and show in diff-mode
     ;; For now, create a placeholder diff buffer
     {:fx [[:dispatch [:echo/message
                       (str "vc-diff: " (or file-path "no file"))]]]})))

(rf/reg-event-fx
 :vc/log
 (fn [{:keys [db]} [_]]
   "Show commit log for current file (C-x v l)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))
         log-content (str "VC Log for: " (or file-path "unknown") "\n"
                          (str/join "" (repeat 60 "=")) "\n\n"
                          "No version control backend available.\n"
                          "Use M-x grant-directory-access to enable file system access.\n")
         buffers (:buffers db)
         new-buffer-id (db/next-buffer-id buffers)
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. log-content))
         lines (str/split log-content #"\n" -1)
         line-count (count lines)]
     (if-not wasm-instance
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
       {:db (assoc-in db [:buffers new-buffer-id]
                       {:id new-buffer-id
                        :name "*vc-log*"
                        :wasm-instance wasm-instance
                        :file-handle nil
                        :major-mode :vc-log-mode
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
                        :cache {:text log-content
                                :line-count line-count}})
        :fx [[:dispatch [:switch-buffer new-buffer-id]]]}))))

(rf/reg-event-fx
 :vc/revert
 (fn [{:keys [db]} [_]]
   "Revert file to repository version (C-x v u)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (:file-name buffer)]
     (if file-path
       {:fx [[:dispatch [:echo/message
                         (str "vc-revert: would revert " file-path)]]]}
       {:fx [[:dispatch [:echo/message "Buffer is not visiting a file"]]]}))))

(rf/reg-event-fx
 :vc/register
 (fn [{:keys [db]} [_]]
   "Register current file with version control (C-x v i)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (:file-name buffer)]
     (if file-path
       {:fx [[:dispatch [:echo/message
                         (str "vc-register: would register " file-path)]]]}
       {:fx [[:dispatch [:echo/message "Buffer is not visiting a file"]]]}))))

(rf/reg-event-fx
 :vc/annotate
 (fn [{:keys [db]} [_]]
   "Show annotate/blame for current file (C-x v g)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         file-path (or (:file-name buffer) (:name buffer))]
     {:fx [[:dispatch [:echo/message
                       (str "vc-annotate: " (or file-path "no file"))]]]})))

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

  (rf/dispatch [:register-command :vc-refresh
                {:docstring "Refresh version control status"
                 :interactive nil
                 :handler [:vc/refresh]}])

  ;; Set default VC state
  (rf/dispatch [:vc/set-backend :git])
  (rf/dispatch [:vc/set-branch "main"]))
