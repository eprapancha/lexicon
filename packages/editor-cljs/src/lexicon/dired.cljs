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
  Attempts to preserve point position.

  Usage: (dired-refresh)
  Returns: nil"
  []
  (let [buffer-name (lisp/buffer-name)
        directory (extract-directory-from-buffer-name buffer-name)]
    (if (and buffer-name (str/starts-with? buffer-name "*dired "))
      (let [current-line (lisp/current-line)]
        ;; Disable read-only to allow modifications
        (lisp/set-buffer-read-only false)
        ;; Clear buffer
        (lisp/delete-region (lisp/point-min) (lisp/point-max))
        ;; Re-insert directory listing
        (lisp/insert-directory directory)
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

  ;; Set up dired-mode keybindings
  ;; Note: local-set-key binds to current buffer's local keymap
  ;; For mode-specific bindings, we need mode keymap support
  ;; For now, register global bindings that check mode
  ;; TODO: Implement proper mode-specific keymaps

  (lisp/message "Dired package loaded")
  (println "🗂️ Dired: Package registration complete"))

;; NOTE: Registration is deferred - called by package_loader after db init
;; (register-dired-package!) is called via :packages/register-all event
