(ns lexicon.core.context
  "Dynamic execution context for Lexicon.

  Provides Emacs-style dynamic scope for command execution. These dynamic vars
  are bound during command execution and can be accessed by hooks, advice, and
  commands themselves.

  Key dynamic vars:
  - *current-command* - The command being executed
  - *current-buffer* - The active buffer ID
  - *prefix-arg* - Universal argument (C-u)
  - *this-command* - For pre/post-command-hook")

;; =============================================================================
;; Dynamic Variables
;; =============================================================================

(def ^:dynamic *current-command*
  "The command currently being executed.
  Bound to the command keyword (e.g., :insert-char, :forward-char)."
  nil)

(def ^:dynamic *current-buffer*
  "The ID of the currently active buffer.
  Bound during command execution."
  nil)

(def ^:dynamic *prefix-arg*
  "The universal argument value (C-u).
  Bound during command execution. Values:
  - nil - No prefix argument
  - N - Numeric argument (e.g., 4 for C-u, 16 for C-u C-u)
  - Raw list like (4) for raw prefix arg"
  nil)

(def ^:dynamic *this-command*
  "The current command for pre/post-command-hook.
  Similar to *current-command* but available in hook context."
  nil)

;; =============================================================================
;; Context Binding Utilities
;; =============================================================================

(defn with-command-context
  "Execute BODY with command execution context bound.

  Options:
  - :command - The command keyword being executed
  - :buffer - The buffer ID
  - :prefix-arg - The prefix argument value

  Example:
    (with-command-context {:command :insert-char
                           :buffer \"buffer-1\"
                           :prefix-arg 4}
      (fn [] (do-something)))"
  [{:keys [command buffer prefix-arg]} f]
  (binding [*current-command* command
            *current-buffer* buffer
            *prefix-arg* prefix-arg
            *this-command* command]
    (f)))

;; =============================================================================
;; Context Accessors
;; =============================================================================

(defn current-command
  "Get the currently executing command, or nil if not in command context."
  []
  *current-command*)

(defn current-buffer
  "Get the current buffer ID, or nil if not in command context."
  []
  *current-buffer*)

(defn prefix-arg
  "Get the current prefix argument, or nil if none."
  []
  *prefix-arg*)

(defn this-command
  "Get the current command (for hooks), or nil if not in command context."
  []
  *this-command*)
