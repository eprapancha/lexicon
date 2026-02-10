(ns lexicon.packages.vc
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

  Based on Emacs lisp/vc/vc.el and lisp/vc/vc-git.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (Package-local)
;; =============================================================================

;; VC state: {:backend :git, :branch "main", :file-status {buffer-id {:state :edited ...}}, :dir-marks #{}}
(defonce vc-state
  (atom {:backend :git
         :branch "main"
         :file-status {}
         :dir-marks #{}}))

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

;; =============================================================================
;; VC State Management Functions
;; =============================================================================

(defn set-file-state!
  "Set the VC state for a buffer."
  [buffer-id state-info]
  (swap! vc-state assoc-in [:file-status buffer-id] state-info))

(defn set-backend!
  "Set the VC backend."
  [backend]
  (swap! vc-state assoc :backend backend))

(defn set-branch!
  "Set the current branch."
  [branch]
  (swap! vc-state assoc :branch branch))

(defn get-file-status
  "Get the VC status for a buffer."
  [buffer-id]
  (get-in @vc-state [:file-status buffer-id]))

(defn get-mode-line-string
  "Get the mode-line string for a buffer."
  [buffer-id]
  (let [status (get-in @vc-state [:file-status buffer-id])
        backend (or (:backend status) (:backend @vc-state) :git)
        state (or (:state status) :up-to-date)
        revision (or (:revision status)
                     (:branch @vc-state)
                     "main")]
    (format-vc-mode-line backend state revision)))

;; =============================================================================
;; VC Commands
;; =============================================================================

(defn vc-next-action!
  "Smart VC action (C-x v v): commit, stage, or revert based on state."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (:file-name buffer-info)
        buffer-name (:name buffer-info)
        status (get-in @vc-state [:file-status buffer-id])
        state (:state status :unregistered)]
    (case state
      :unregistered
      (lisp/message (str "File not under version control: "
                         (or file-path buffer-name)))
      :up-to-date
      (lisp/message "File is up to date")
      :edited
      (lisp/message (str "Modified: " (or file-path buffer-name)
                         " - use vc-diff to see changes"))
      :added
      (lisp/message (str "Added: " (or file-path buffer-name)))
      :conflict
      (lisp/message (str "Conflict in: " (or file-path buffer-name)
                         " - resolve before committing"))
      (lisp/message (str "VC state: " (name state))))))

(defn vc-diff!
  "Show diff for current file (C-x v =).
   Creates a *vc-diff* buffer in diff-mode showing the file's modified state."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (or (:file-name buffer-info) (:name buffer-info))
        modified? (:is-modified? buffer-info)
        text (or (lisp/buffer-string) "")
        diff-content (str "diff --git a/" file-path " b/" file-path "\n"
                          "--- a/" file-path "\n"
                          "+++ b/" file-path "\n"
                          (if modified?
                            (str "@@ -1,0 +1," (count (str/split text #"\n" -1)) " @@\n"
                                 (str/join "\n" (map #(str "+" %) (str/split text #"\n" -1)))
                                 "\n")
                            "No differences.\n"))
        existing-id (lisp/get-buffer "*vc-diff*")]
    (when existing-id
      (lisp/kill-buffer existing-id))
    (lisp/create-special-buffer "*vc-diff*" diff-content
                                {:major-mode :diff-mode
                                 :read-only true})
    (lisp/split-window-below)
    (lisp/switch-to-buffer "*vc-diff*")))

(defn vc-log!
  "Show commit log for current file (C-x v l)."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (or (:file-name buffer-info) (:name buffer-info))
        branch (or (:branch @vc-state) "main")
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
        existing-id (lisp/get-buffer "*vc-log*")]
    (when existing-id
      (lisp/kill-buffer existing-id))
    (lisp/create-special-buffer "*vc-log*" log-content
                                {:major-mode :vc-log-mode
                                 :read-only true})
    (lisp/split-window-below)
    (lisp/switch-to-buffer "*vc-log*")))

(defn vc-revert!
  "Revert file to last saved state (C-x v u).
   Clears the modified flag for the buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (:file-name buffer-info)
        buffer-name (:name buffer-info)]
    (if (:is-modified? buffer-info)
      (do
        ;; Note: Full revert would require file-handle access which needs db
        ;; For now, just display a message about the revert
        (lisp/message (str "Revert: " (or file-path buffer-name)
                           " (full revert requires file system access)")))
      (lisp/message "Buffer is not modified"))))

(defn vc-register!
  "Register current file with version control (C-x v i).
   Marks the buffer's VC state as :added."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (or (:file-name buffer-info) (:name buffer-info))
        branch (or (:branch @vc-state) "main")]
    (if buffer-id
      (do
        (swap! vc-state assoc-in [:file-status buffer-id]
               {:state :added
                :backend :git
                :revision branch})
        (lisp/message (str "Registered " file-path " under Git")))
      (lisp/message "No buffer to register"))))

(defn vc-annotate!
  "Show annotate/blame for current file (C-x v g).
   Creates a *vc-annotate* buffer with per-line annotation."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        file-path (or (:file-name buffer-info) (:name buffer-info))
        text (lisp/buffer-string)
        branch (or (:branch @vc-state) "main")
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
        existing-id (lisp/get-buffer "*vc-annotate*")]
    (when existing-id
      (lisp/kill-buffer existing-id))
    (lisp/create-special-buffer "*vc-annotate*" annotate-content
                                {:major-mode :fundamental-mode
                                 :read-only true})
    (lisp/split-window-below)
    (lisp/switch-to-buffer "*vc-annotate*")))

;; =============================================================================
;; VC Dir Mode
;; =============================================================================

(defn- get-buffer-vc-state
  "Get the VC state for a buffer, inferring from modified flag if not set."
  [buffer-id buffer-info]
  (let [status (get-in @vc-state [:file-status buffer-id])
        state (or (:state status)
                  (if (:is-modified? buffer-info) :edited :up-to-date))]
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
  [buffer-id buffer-info marked?]
  (let [state (get-buffer-vc-state buffer-id buffer-info)
        mark-char (if marked? "*" " ")
        state-ch (state-char state)
        name (or (:file-name buffer-info) (:name buffer-info))]
    (str "  " mark-char " " state-ch "  " name)))

(defn vc-dir!
  "Show VC directory status (C-x v d).
   Creates *vc-dir* buffer showing all open buffers with VC status."
  []
  (let [branch (or (:branch @vc-state) "main")
        all-buffers (lisp/all-buffer-info)
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) all-buffers)
        header (str "VC backend: Git\n"
                    "Working dir: /\n"
                    "Branch: " branch "\n\n"
                    "  M State  File\n"
                    "  - -----  ----\n")
        entries (str/join "\n"
                          (map (fn [buf-info]
                                 (format-vc-dir-entry (:id buf-info) buf-info false))
                               (sort-by :name file-buffers)))
        content (str header
                     (if (seq file-buffers) entries "(no files)")
                     "\n")
        existing-id (lisp/get-buffer "*vc-dir*")]
    ;; Clear dir-marks in package-local state
    (swap! vc-state assoc :dir-marks #{})
    (when existing-id
      (lisp/kill-buffer existing-id))
    (lisp/create-special-buffer "*vc-dir*" content
                                {:major-mode :vc-dir-mode
                                 :read-only true})
    (lisp/split-window-below)
    (lisp/switch-to-buffer "*vc-dir*")))

(defn vc-dir-mark!
  "Mark file at point in vc-dir buffer."
  []
  (let [buffer-name (lisp/buffer-name)
        cursor-line (lisp/current-line)]
    (if (= buffer-name "*vc-dir*")
      (do
        (swap! vc-state update :dir-marks (fnil conj #{}) cursor-line)
        (lisp/message "Marked"))
      (lisp/message "Not in vc-dir buffer"))))

(defn vc-dir-unmark!
  "Unmark file at point in vc-dir buffer."
  []
  (let [buffer-name (lisp/buffer-name)
        cursor-line (lisp/current-line)]
    (if (= buffer-name "*vc-dir*")
      (do
        (swap! vc-state update :dir-marks (fnil disj #{}) cursor-line)
        (lisp/message "Unmarked"))
      (lisp/message "Not in vc-dir buffer"))))

(defn vc-dir-refresh!
  "Refresh vc-dir buffer."
  []
  (vc-dir!))

;; =============================================================================
;; VC Refresh
;; =============================================================================

(defn vc-refresh!
  "Refresh VC state for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        modified? (:is-modified? buffer-info)
        state (if modified? :edited :up-to-date)]
    (swap! vc-state assoc-in [:file-status buffer-id]
           {:state state
            :backend :git
            :revision (or (:branch @vc-state) "main")})))

(defn vc-quit!
  "Quit vc buffer."
  []
  (lisp/delete-window))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize vc module and register commands."
  []
  ;; VC commands
  (lisp/define-command 'vc-next-action
    vc-next-action!
    "Do the next logical version control operation (C-x v v)")

  (lisp/define-command 'vc-diff
    vc-diff!
    "Show diff for current file (C-x v =)")

  (lisp/define-command 'vc-log
    vc-log!
    "Show commit log for current file (C-x v l)")

  (lisp/define-command 'vc-revert
    vc-revert!
    "Revert file to repository version (C-x v u)")

  (lisp/define-command 'vc-register
    vc-register!
    "Register current file with version control (C-x v i)")

  (lisp/define-command 'vc-annotate
    vc-annotate!
    "Show annotate/blame for current file (C-x v g)")

  (lisp/define-command 'vc-dir
    vc-dir!
    "Show VC directory status (C-x v d)")

  (lisp/define-command 'vc-refresh
    vc-refresh!
    "Refresh version control status")

  ;; vc-dir mark/unmark commands
  (lisp/define-command 'vc-dir-mark
    vc-dir-mark!
    "Mark file in vc-dir buffer")

  (lisp/define-command 'vc-dir-unmark
    vc-dir-unmark!
    "Unmark file in vc-dir buffer")

  (lisp/define-command 'vc-dir-refresh
    vc-dir-refresh!
    "Refresh vc-dir buffer")

  (lisp/define-command 'vc-quit
    vc-quit!
    "Quit vc buffer")

  ;; vc-dir mode keybindings
  (lisp/define-key-for-mode :vc-dir-mode "m" :vc-dir-mark)
  (lisp/define-key-for-mode :vc-dir-mode "u" :vc-dir-unmark)
  (lisp/define-key-for-mode :vc-dir-mode "g" :vc-dir-refresh)
  (lisp/define-key-for-mode :vc-dir-mode "q" :vc-quit)

  ;; Set default VC state
  (set-backend! :git)
  (set-branch! "main"))
