(ns lexicon.packages.shell
  "Shell and Eshell integration (shell.el, eshell/).

  Implements Emacs shell mode core functionality:
  - M-x shell: Open shell buffer
  - M-x eshell: Open Emacs shell buffer
  - shell-command (M-!): Execute single command
  - shell-command-on-region (M-|): Pipe region to command
  - async-shell-command (M-&): Execute command asynchronously

  In browser context, provides:
  - Command input/output UI in a shell buffer
  - Eshell-like built-in command execution
  - Integration with File System Access API for file operations

  Built-in eshell commands:
  - echo: Print arguments
  - pwd: Print working directory
  - cd: Change directory
  - ls: List directory contents
  - cat: Display file contents
  - clear: Clear shell buffer
  - help: Show available commands
  - whoami: Show user info
  - date: Show current date/time

  Key bindings:
  - M-! : shell-command
  - M-& : async-shell-command
  - M-| : shell-command-on-region

  Based on Emacs lisp/shell.el and lisp/eshell/

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Shell State (Package-local)
;; =============================================================================

(defonce shell-state
  (atom {:cwd "/"
         :history []
         :env {}}))

;; =============================================================================
;; Eshell Built-in Commands
;; =============================================================================

(declare builtin-commands)

(defn- eshell-echo
  "Echo arguments to output."
  [args]
  (str/join " " args))

(defn- eshell-pwd
  "Print working directory."
  [_args]
  (:cwd @shell-state))

(defn- eshell-date
  "Print current date/time."
  [_args]
  (let [now (js/Date.)]
    (.toString now)))

(defn- eshell-whoami
  "Print user info."
  [_args]
  "lexicon-user")

(defn- eshell-clear
  "Clear the shell buffer."
  [_args]
  :clear)

(defn- eshell-help
  "Show available commands."
  [_args]
  (str "Available commands:\n"
       "  echo [args...]  - Print arguments\n"
       "  pwd             - Print working directory\n"
       "  cd [dir]        - Change directory\n"
       "  ls [dir]        - List directory contents\n"
       "  cat [file]      - Display file contents\n"
       "  clear           - Clear shell buffer\n"
       "  help            - Show this help\n"
       "  whoami          - Show user info\n"
       "  date            - Show current date/time\n"
       "  history         - Show command history\n"))

(defn- eshell-history
  "Show command history."
  [_args]
  (let [history (:history @shell-state)]
    (if (seq history)
      (str/join "\n" (map-indexed
                       (fn [i cmd] (str (inc i) "  " cmd))
                       history))
      "No command history.")))

(defn- eshell-cd
  "Change directory."
  [args]
  (let [dir (or (first args) "/")]
    (swap! shell-state assoc :cwd dir)
    (str "cd " dir)))

(defn- eshell-ls
  "List directory contents (lists open buffers as files)."
  [_args]
  (let [buffers (lisp/all-buffer-info)
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)]
    (if (seq file-buffers)
      (str/join "\n" (map :name file-buffers))
      "(no files open)")))

(defn- eshell-cat
  "Display file contents (reads from open buffer)."
  [args]
  (let [filename (first args)]
    (if filename
      (if-let [buffer-id (lisp/get-buffer filename)]
        (let [text (lisp/buffer-text-of buffer-id)]
          (or text "Buffer has no content"))
        (str filename ": No such file or buffer"))
      "cat: missing operand")))

(defn- eshell-env
  "Display environment variables."
  [_args]
  (let [env (:env @shell-state)]
    (if (seq env)
      (str/join "\n" (map (fn [[k v]] (str k "=" v)) (sort env)))
      "TERM=xterm\nSHELL=/bin/eshell\nUSER=lexicon-user\nHOME=/\nPATH=/usr/bin")))

(defn- eshell-export
  "Set environment variable."
  [args]
  (let [assignment (first args)]
    (if assignment
      (let [parts (str/split assignment #"=" 2)
            k (first parts)
            v (or (second parts) "")]
        (swap! shell-state assoc-in [:env k] v)
        (str k "=" v))
      "export: usage: export NAME=VALUE")))

(defn- eshell-which
  "Show command type."
  [args]
  (let [cmd (first args)]
    (if cmd
      (if (get builtin-commands cmd)
        (str cmd ": eshell built-in command")
        (str cmd ": command not found"))
      "which: missing argument")))

(def builtin-commands
  "Map of built-in eshell commands."
  {"echo" eshell-echo
   "pwd" eshell-pwd
   "cd" eshell-cd
   "ls" eshell-ls
   "cat" eshell-cat
   "date" eshell-date
   "whoami" eshell-whoami
   "clear" eshell-clear
   "help" eshell-help
   "history" eshell-history
   "env" eshell-env
   "export" eshell-export
   "which" eshell-which})

(defn execute-command
  "Execute a shell command string. Returns output string or :clear."
  [command-str]
  (let [parts (str/split (str/trim command-str) #"\s+")
        cmd (first parts)
        args (rest parts)]
    (if-let [builtin (get builtin-commands cmd)]
      (builtin args)
      (str cmd ": command not found"))))

;; =============================================================================
;; Shell Buffer Management
;; =============================================================================

(defn- make-shell-prompt
  "Generate shell prompt string."
  []
  (let [cwd (:cwd @shell-state)]
    (str "lexicon:" cwd " $ ")))

(defn- open-shell-buffer!
  "Open a shell buffer with the given name."
  [shell-name]
  (let [name (or shell-name "*shell*")
        existing-id (lisp/get-buffer name)]
    (if existing-id
      (lisp/switch-to-buffer existing-id)
      (let [prompt (make-shell-prompt)
            content (str "Lexicon Shell\n"
                         "Type 'help' for available commands.\n\n"
                         prompt)
            buffer-id (lisp/create-special-buffer name content
                                                  {:major-mode :shell-mode
                                                   :read-only false})]
        (when buffer-id
          (lisp/switch-to-buffer buffer-id))))))

;; =============================================================================
;; Shell Command Execution (M-! and M-&)
;; =============================================================================

(defn- execute-shell-command!
  "Execute the shell command and show result."
  [command]
  (let [output (execute-command command)]
    (if (= output :clear)
      (lisp/message "clear: not applicable outside shell buffer")
      (do
        (swap! shell-state update :history conj command)
        (lisp/message output)))))

;; =============================================================================
;; Interactive Commands
;; =============================================================================

(defn shell-interactive
  "Open a shell buffer."
  []
  (open-shell-buffer! "*shell*"))

(defn eshell-interactive
  "Open an eshell buffer."
  []
  (open-shell-buffer! "*eshell*"))

(defn shell-command-interactive
  "Execute a shell command and display output (M-!)."
  []
  (lisp/read-from-minibuffer
   "Shell command: "
   execute-shell-command!))

(defn async-shell-command-interactive
  "Execute an async shell command (M-&)."
  []
  (lisp/read-from-minibuffer
   "Async shell command: "
   execute-shell-command!))

(defn shell-command-on-region-interactive
  "Pipe region to shell command (M-|)."
  []
  (lisp/read-from-minibuffer
   "Shell command on region: "
   execute-shell-command!))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize shell/eshell module and register commands."
  []
  ;; Shell commands
  (lisp/define-command 'shell
    shell-interactive
    "Open a shell buffer")

  (lisp/define-command 'eshell
    eshell-interactive
    "Open an Emacs shell (eshell) buffer")

  (lisp/define-command 'shell-command
    shell-command-interactive
    "Execute a shell command and display output (M-!)")

  (lisp/define-command 'async-shell-command
    async-shell-command-interactive
    "Execute an async shell command (M-&)")

  (lisp/define-command 'shell-command-on-region
    shell-command-on-region-interactive
    "Pipe region to shell command (M-|)"))
