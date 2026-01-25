(ns lexicon.dired
  "Dired (Directory Editor) Package

  This is a PACKAGE, not core. It uses ONLY the public lexicon.lisp API.
  No direct access to re-frame, app-db, or internal events.

  This is the architectural model for all packages:
  - Import only lexicon.lisp functions
  - Register commands via the public API
  - Implement functionality using primitives

  Related: Issue #93, ARCHITECTURE_BOUNDARY.md"
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; Dired State (stored in buffer-local variables via text properties)
;; =============================================================================

(defn- dired-buffer-name
  "Generate buffer name for directory"
  [directory]
  (str "*dired " directory "*"))

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

(defn dired-refresh
  "Refresh the current Dired buffer.

  Re-reads the directory and regenerates the listing.
  Attempts to preserve point position.

  Usage: (dired-refresh)
  Returns: nil"
  []
  ;; TODO: Implement refresh
  ;; Need to:
  ;; 1. Get current directory from buffer
  ;; 2. Save current position
  ;; 3. Clear buffer (need set-buffer-read-only false first)
  ;; 4. Re-insert directory
  ;; 5. Restore position
  ;; 6. Set read-only again
  (lisp/message "Dired refresh not yet implemented"))

(defn dired-next-line
  "Move to next line in Dired buffer.

  Usage: (dired-next-line)
  Returns: nil"
  []
  ;; Use forward-line when available, for now just message
  (lisp/message "TODO: dired-next-line"))

(defn dired-previous-line
  "Move to previous line in Dired buffer.

  Usage: (dired-previous-line)
  Returns: nil"
  []
  (lisp/message "TODO: dired-previous-line"))

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

  (lisp/message "Dired package loaded")
  (println "🗂️ Dired: Package registration complete"))

;; NOTE: Registration is deferred - called by package_loader after db init
;; (register-dired-package!) is called via :packages/register-all event
