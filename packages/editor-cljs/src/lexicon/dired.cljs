(ns lexicon.dired
  "Dired (Directory Editor) Package

  This is a PACKAGE, not core. It uses ONLY the public lexicon.lisp API.
  No direct access to re-frame, app-db, or internal events.

  This is the architectural model for all packages:
  - Import only lexicon.lisp functions
  - Register commands via the public API
  - Implement functionality using primitives

  Related: Issue #93, ARCHITECTURE_BOUNDARY.md"
  (:require [lexicon.lisp :as lisp]
            [clojure.string :as str]))

;; =============================================================================
;; Dired State
;; =============================================================================

;; Mark storage: {buffer-name -> {filename -> mark-char}}
;; Mark chars: ?* = marked, ?D = flagged for deletion
(defonce dired-marks (atom {}))

(defn- dired-buffer-name
  "Generate buffer name for directory"
  [directory]
  (str "*dired " directory "*"))

(defn- get-marks-for-buffer
  "Get marks map for current buffer"
  [buffer-name]
  (get @dired-marks buffer-name {}))

(defn- set-mark-for-file!
  "Set mark for a file in the current buffer"
  [buffer-name filename mark-char]
  (swap! dired-marks assoc-in [buffer-name filename] mark-char))

(defn- clear-mark-for-file!
  "Clear mark for a file in the current buffer"
  [buffer-name filename]
  (swap! dired-marks update buffer-name dissoc filename))

(defn- clear-all-marks!
  "Clear all marks for a buffer"
  [buffer-name]
  (swap! dired-marks dissoc buffer-name))

(defn- update-line-mark
  "Update the mark character at the beginning of the current line.
  Mark should be a single character string like '*', 'D', or ' '."
  [mark-char]
  (when-let [line-start (lisp/line-beginning-position)]
    ;; Only update if we're on a file line (not header)
    ;; File lines start after position 0 (header is line 0)
    (when (pos? ^number line-start)
      (lisp/set-buffer-read-only false)
      ;; Delete the first character and insert the new mark
      (lisp/goto-char line-start)
      (lisp/delete-char 1)
      (lisp/insert mark-char)
      ;; Move back to start of line for consistency
      (lisp/goto-char line-start)
      (lisp/set-buffer-read-only true))))

;; =============================================================================
;; Dired Buffer Creation
;; =============================================================================

(defn dired
  "Open a directory in Dired mode.

  This is the main entry point, equivalent to M-x dired.
  Creates a new buffer showing directory contents.

  Usage: (dired \"/home/user\")
  Returns: nil"
  [directory]
  (let [buffer-name (dired-buffer-name directory)]
    ;; Create and switch to buffer
    (lisp/switch-to-buffer buffer-name)

    ;; Insert directory listing
    (lisp/insert-directory directory)

    ;; Set buffer to read-only
    (lisp/set-buffer-read-only true)

    ;; Set major mode
    (lisp/set-major-mode 'dired-mode)

    ;; Move to beginning
    (lisp/goto-char 0)

    ;; Message
    (lisp/message (str "Dired: " directory))

    nil))

;; =============================================================================
;; Dired Commands
;; =============================================================================

(defn- extract-directory-from-buffer-name
  "Extract directory path from Dired buffer name.
  Buffer names follow pattern: *dired /path/to/dir*"
  [buffer-name]
  (when buffer-name
    (let [;; Remove "*dired " prefix and "*" suffix
          stripped (-> buffer-name
                       (str/replace #"^\*dired " "")
                       (str/replace #"\*$" ""))]
      stripped)))

(defn dired-refresh
  "Refresh the current Dired buffer.

  Re-reads the directory and regenerates the listing.
  Attempts to preserve point position. Shows current marks.

  Usage: (dired-refresh)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)]
    (if (and buffer-name (str/starts-with? buffer-name "*dired "))
      (let [current-line (lisp/current-line)
            marks (get-marks-for-buffer buffer-name)]
        ;; Disable read-only to allow modifications
        (lisp/set-buffer-read-only false)
        ;; Clear buffer
        (lisp/delete-region (lisp/point-min) (lisp/point-max))
        ;; Re-insert directory listing with marks
        (lisp/insert-directory directory marks)
        ;; Re-enable read-only
        (lisp/set-buffer-read-only true)
        ;; Restore position (go to same line number)
        (lisp/goto-char 0)
        (when (> current-line 0)
          (lisp/forward-line current-line))
        (lisp/message (str "Dired: refreshed " directory)))
      (lisp/message "Not in a Dired buffer"))))

(defn dired-next-line
  "Move to next line in Dired buffer.

  Usage: (dired-next-line)
  Returns: nil"
  ([] (dired-next-line 1))
  ([n]
   (lisp/forward-line n)
   nil))

(defn dired-previous-line
  "Move to previous line in Dired buffer.

  Usage: (dired-previous-line)
  Returns: nil"
  ([] (dired-previous-line 1))
  ([n]
   (lisp/forward-line (- n))
   nil))

;; =============================================================================
;; Dired Marks
;; =============================================================================

(defn- dired-get-filename
  "Get the filename at the current line.
  Returns nil if not on a file line.

  Directory listing format expected:
  drwxr-xr-x 2 user group 4096 Jan 25 10:00 dirname
  -rw-r--r-- 1 user group  123 Jan 25 10:00 filename"
  []
  (let [line-start (lisp/line-beginning-position)
        line-end (lisp/line-end-position)
        line-text (lisp/buffer-substring line-start line-end)]
    ;; Extract filename: it's typically the last space-separated field
    ;; Skip lines that are headers or empty
    (when (and line-text
               (not (str/blank? line-text))
               (not (str/starts-with? (str/trim line-text) "total")))
      ;; Split on whitespace and get last element (filename)
      (let [parts (str/split (str/trim line-text) #"\s+")]
        (when (>= (count parts) 9) ; ls -l format has at least 9 fields
          (last parts))))))

(defn dired-mark
  "Mark the file at point with *.

  Usage: (dired-mark)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        filename (dired-get-filename)]
    (if filename
      (do
        (set-mark-for-file! buffer-name filename "*")
        (update-line-mark "*")
        (lisp/message (str "Marked: " filename))
        (dired-next-line))
      (lisp/message "No file at point"))))

(defn dired-unmark
  "Remove mark from file at point.

  Usage: (dired-unmark)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        filename (dired-get-filename)]
    (if filename
      (do
        (clear-mark-for-file! buffer-name filename)
        (update-line-mark " ")
        (lisp/message (str "Unmarked: " filename))
        (dired-next-line))
      (lisp/message "No file at point"))))

(defn dired-flag-file-deletion
  "Flag file at point for deletion (mark with D).

  Usage: (dired-flag-file-deletion)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        filename (dired-get-filename)]
    (if filename
      (do
        (set-mark-for-file! buffer-name filename "D")
        (update-line-mark "D")
        (lisp/message (str "Flagged for deletion: " filename))
        (dired-next-line))
      (lisp/message "No file at point"))))

(defn dired-unmark-backward
  "Move up one line and remove mark.

  Usage: (dired-unmark-backward)
  Returns: nil"
  []
  (dired-previous-line)
  (let [buffer-name (lisp/buffer-name)
        filename (dired-get-filename)]
    (when filename
      (clear-mark-for-file! buffer-name filename)
      (update-line-mark " ")
      (lisp/message (str "Unmarked: " filename)))))

(defn dired-get-marked-files
  "Get list of all marked files in current buffer.

  Usage: (dired-get-marked-files)
  Returns: Vector of filenames"
  []
  (let [buffer-name (lisp/buffer-name)
        marks (get-marks-for-buffer buffer-name)]
    (vec (keys marks))))

(defn dired-get-flagged-files
  "Get list of files flagged for deletion.

  Usage: (dired-get-flagged-files)
  Returns: Vector of filenames"
  []
  (let [buffer-name (lisp/buffer-name)
        marks (get-marks-for-buffer buffer-name)]
    (vec (keys (filter (fn [[_ mark]] (= mark "D")) marks)))))

;; =============================================================================
;; Dired File Operations (Issue #139)
;; =============================================================================

(defn dired-find-file
  "Visit the file or directory at point.

  If it's a file, opens it in a buffer.
  If it's a directory, opens a new dired buffer.

  Usage: (dired-find-file)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)
        filename (dired-get-filename)]
    (if filename
      (let [full-path (str directory (when-not (str/ends-with? directory "/") "/") filename)]
        (if (str/ends-with? filename "/")
          ;; It's a directory - open in dired
          (dired full-path)
          ;; It's a file - try to open it
          ;; Use find-file command
          (lisp/find-file full-path)))
      (lisp/message "No file at point"))))

(defn dired-up-directory
  "Go up to parent directory in Dired.

  Usage: (dired-up-directory)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)]
    (when directory
      (let [;; Remove trailing slash and get parent
            normalized (if (str/ends-with? directory "/")
                         (subs directory 0 (dec (count directory)))
                         directory)
            parent-path (str/replace normalized #"/[^/]+$" "")
            parent (if (str/blank? parent-path) "/" (str parent-path "/"))]
        (dired parent)))))

(defn dired-unmark-all-marks
  "Remove all marks from all files in the Dired buffer.

  Usage: (dired-unmark-all-marks)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)]
    (clear-all-marks! buffer-name)
    ;; Refresh to clear visual marks
    (dired-refresh)
    (lisp/message "All marks cleared")))

(defn dired-do-flagged-delete
  "Delete all files flagged with D.

  Prompts for confirmation before deletion.

  Usage: (dired-do-flagged-delete)
  Returns: nil"
  []
  (let [flagged (dired-get-flagged-files)]
    (if (empty? flagged)
      (lisp/message "No flagged files")
      ;; Note: In a real implementation, this would:
      ;; 1. Show confirmation prompt
      ;; 2. Actually delete the files via File System Access API
      ;; 3. Refresh the buffer
      ;; For now, we just display the flagged files
      (do
        (lisp/message (str "Would delete: " (str/join ", " flagged)))
        ;; Clear flags after "deletion"
        (doseq [f flagged]
          (clear-mark-for-file! (lisp/buffer-name) f))
        (dired-refresh)))))

(defn dired-toggle-marks
  "Toggle marks on all files.
  Marked files become unmarked, unmarked become marked.
  Files flagged for deletion (D) are not affected.

  Usage: (dired-toggle-marks)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)
        all-files (lisp/directory-files directory)
        current-marks (get-marks-for-buffer buffer-name)]
    ;; Toggle marks for each file
    (doseq [filename all-files]
      (let [current-mark (get current-marks filename)]
        (cond
          ;; Skip files flagged for deletion
          (= current-mark "D") nil
          ;; Marked -> unmarked
          (= current-mark "*") (clear-mark-for-file! buffer-name filename)
          ;; Unmarked -> marked
          :else (set-mark-for-file! buffer-name filename "*"))))
    ;; Refresh to show changes
    (dired-refresh)
    (lisp/message "Toggled marks")))

(defn dired-mark-files-regexp
  "Mark files matching a regular expression.

  Usage: (dired-mark-files-regexp regexp)
  Returns: nil"
  [regexp]
  (lisp/message (str "Would mark files matching: " regexp)))

(defn dired-do-copy
  "Copy marked files (or file at point if none marked).

  Usage: (dired-do-copy)
  Returns: nil"
  []
  (let [marked (dired-get-marked-files)
        target (if (empty? marked)
                 (dired-get-filename)
                 marked)]
    (if target
      (lisp/message (str "Copy: " target " (not yet implemented - needs FS Access write)"))
      (lisp/message "No files to copy"))))

(defn dired-do-rename
  "Rename marked files (or file at point if none marked).

  Usage: (dired-do-rename)
  Returns: nil"
  []
  (let [marked (dired-get-marked-files)
        target (if (empty? marked)
                 (dired-get-filename)
                 marked)]
    (if target
      (lisp/message (str "Rename: " target " (not yet implemented - needs FS Access write)"))
      (lisp/message "No files to rename"))))

(defn dired-do-delete
  "Delete marked files (or file at point if none marked).

  Usage: (dired-do-delete)
  Returns: nil"
  []
  (let [marked (dired-get-marked-files)
        target (if (empty? marked)
                 (list (dired-get-filename))
                 marked)]
    (if (and target (first target))
      (lisp/message (str "Delete: " target " (not yet implemented - needs FS Access write)"))
      (lisp/message "No files to delete"))))

(defn dired-create-directory
  "Create a new directory.

  Usage: (dired-create-directory name)
  Returns: nil"
  [name]
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)
        full-path (str directory (when-not (str/ends-with? directory "/") "/") name)]
    (lisp/message (str "Create directory: " full-path " (not yet implemented - needs FS Access write)"))))

;; =============================================================================
;; Package Registration
;; =============================================================================

(defn register-dired-package!
  "Register Dired package commands and keybindings.

  Call this at startup to make Dired available.
  This is how packages integrate with the editor.

  Usage: (register-dired-package!)
  Returns: nil"
  []
  (println "🗂️ Dired: Registering commands...")

  ;; Register the main dired command with interactive prompt
  (lisp/define-command
    'dired
    dired
    "Open directory in Dired mode"
    {:interactive [{:type :async :prompt "Dired (directory): "}]})
  (println "🗂️ Dired: Registered 'dired' command")

  ;; Register refresh command
  (lisp/define-command
    'dired-refresh
    dired-refresh
    "Refresh current Dired buffer")
  (println "🗂️ Dired: Registered 'dired-refresh' command")

  ;; Register navigation commands
  (lisp/define-command
    'dired-next-line
    dired-next-line
    "Move to next line in Dired")
  (lisp/define-command
    'dired-previous-line
    dired-previous-line
    "Move to previous line in Dired")
  (println "🗂️ Dired: Registered navigation commands")

  ;; Register mark commands
  (lisp/define-command
    'dired-mark
    dired-mark
    "Mark file at point")
  (lisp/define-command
    'dired-unmark
    dired-unmark
    "Unmark file at point")
  (lisp/define-command
    'dired-flag-file-deletion
    dired-flag-file-deletion
    "Flag file at point for deletion")
  (lisp/define-command
    'dired-unmark-backward
    dired-unmark-backward
    "Move up and unmark")
  (println "🗂️ Dired: Registered mark commands")

  ;; Issue #139: Register file operation commands
  (lisp/define-command
    'dired-find-file
    dired-find-file
    "Visit file or directory at point")
  (lisp/define-command
    'dired-up-directory
    dired-up-directory
    "Go to parent directory")
  (lisp/define-command
    'dired-unmark-all-marks
    dired-unmark-all-marks
    "Remove all marks from files")
  (lisp/define-command
    'dired-do-flagged-delete
    dired-do-flagged-delete
    "Delete flagged files")
  (lisp/define-command
    'dired-do-copy
    dired-do-copy
    "Copy marked files")
  (lisp/define-command
    'dired-do-rename
    dired-do-rename
    "Rename marked files")
  (lisp/define-command
    'dired-do-delete
    dired-do-delete
    "Delete marked files")
  (lisp/define-command
    'dired-create-directory
    dired-create-directory
    "Create new directory"
    {:interactive [{:prompt "Create directory: "}]})
  (lisp/define-command
    'dired-toggle-marks
    dired-toggle-marks
    "Toggle marks on all files")
  (println "🗂️ Dired: Registered file operation commands")

  ;; Issue #139: Register dired-mode keybindings
  ;; These use the mode-specific keymap
  (lisp/define-key-for-mode 'dired-mode "n" 'dired-next-line)
  (lisp/define-key-for-mode 'dired-mode "p" 'dired-previous-line)
  (lisp/define-key-for-mode 'dired-mode "RET" 'dired-find-file)
  (lisp/define-key-for-mode 'dired-mode "^" 'dired-up-directory)
  (lisp/define-key-for-mode 'dired-mode "g" 'dired-refresh)
  (lisp/define-key-for-mode 'dired-mode "m" 'dired-mark)
  (lisp/define-key-for-mode 'dired-mode "u" 'dired-unmark)
  (lisp/define-key-for-mode 'dired-mode "U" 'dired-unmark-all-marks)
  (lisp/define-key-for-mode 'dired-mode "d" 'dired-flag-file-deletion)
  (lisp/define-key-for-mode 'dired-mode "x" 'dired-do-flagged-delete)
  (lisp/define-key-for-mode 'dired-mode "C" 'dired-do-copy)
  (lisp/define-key-for-mode 'dired-mode "R" 'dired-do-rename)
  (lisp/define-key-for-mode 'dired-mode "D" 'dired-do-delete)
  (lisp/define-key-for-mode 'dired-mode "+" 'dired-create-directory)
  (lisp/define-key-for-mode 'dired-mode "t" 'dired-toggle-marks)
  (lisp/define-key-for-mode 'dired-mode "DEL" 'dired-unmark-backward)
  (println "🗂️ Dired: Registered keybindings")

  (lisp/message "Dired package loaded")
  (println "🗂️ Dired: Package registration complete"))

;; NOTE: Registration is deferred - called by package_loader after db init
;; (register-dired-package!) is called via :packages/register-all event
