(ns lexicon.core.shell
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

  Based on Emacs lisp/shell.el and lisp/eshell/"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

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
  (let [db @re-frame.db/app-db
        cwd (get-in db [:shell :cwd] "/")]
    cwd))

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
  (let [db @re-frame.db/app-db
        history (get-in db [:shell :history] [])]
    (if (seq history)
      (str/join "\n" (map-indexed
                       (fn [i cmd] (str (inc i) "  " cmd))
                       history))
      "No command history.")))

(defn- eshell-cd
  "Change directory."
  [args]
  (let [dir (or (first args) "/")]
    (rf/dispatch [:shell/set-cwd dir])
    (str "cd " dir)))

(defn- eshell-ls
  "List directory contents (lists open buffers as files)."
  [_args]
  (let [db @re-frame.db/app-db
        buffers (vals (:buffers db))
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)]
    (if (seq file-buffers)
      (str/join "\n" (map :name file-buffers))
      "(no files open)")))

(defn- eshell-cat
  "Display file contents (reads from open buffer)."
  [args]
  (let [filename (first args)]
    (if filename
      (let [db @re-frame.db/app-db
            buffer (first (filter #(= (:name (val %)) filename) (:buffers db)))]
        (if buffer
          (let [wasm (:wasm-instance (val buffer))]
            (if wasm
              (try (.getText ^js wasm) (catch :default _ "Error reading buffer"))
              "Buffer has no content"))
          (str filename ": No such file or buffer")))
      "cat: missing operand")))

(defn- eshell-env
  "Display environment variables."
  [_args]
  (let [db @re-frame.db/app-db
        env (get-in db [:shell :env] {})]
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
        (rf/dispatch [:shell/set-env k v])
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
;; Shell State Management
;; =============================================================================

(rf/reg-event-db
 :shell/set-cwd
 (fn [db [_ dir]]
   (assoc-in db [:shell :cwd] dir)))

(rf/reg-event-db
 :shell/set-env
 (fn [db [_ key val]]
   (assoc-in db [:shell :env key] val)))

;; =============================================================================
;; Shell Buffer Management
;; =============================================================================

(defn- make-shell-prompt
  "Generate shell prompt string."
  [db]
  (let [cwd (get-in db [:shell :cwd] "/")]
    (str "lexicon:" cwd " $ ")))

(rf/reg-event-fx
 :shell/open
 (fn [{:keys [db]} [_ shell-name]]
   "Open a shell buffer (M-x shell)."
   (let [name (or shell-name "*shell*")
         ;; Check if shell buffer already exists
         existing (first (filter #(= (:name (val %)) name) (:buffers db)))]
     (if existing
       {:fx [[:dispatch [:switch-buffer (key existing)]]]}
       (let [prompt (make-shell-prompt db)
             content (str "Lexicon Shell\n"
                          "Type 'help' for available commands.\n\n"
                          prompt)
             buffers (:buffers db)
             buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
             lines (str/split content #"\n" -1)
             line-count (count lines)]
         (if-not wasm-instance
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
           {:db (-> db
                    (assoc-in [:buffers buffer-id]
                              {:id buffer-id
                               :name name
                               :wasm-instance wasm-instance
                               :file-handle nil
                               :major-mode :shell-mode
                               :is-read-only? false
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
                    (assoc-in [:shell :cwd] "/")
                    (assoc-in [:shell :history] []))
            :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

(rf/reg-event-fx
 :eshell/open
 (fn [{:keys [db]} [_]]
   "Open an eshell buffer (M-x eshell)."
   {:fx [[:dispatch [:shell/open "*eshell*"]]]}))

;; =============================================================================
;; Shell Command Execution (M-! and M-&)
;; =============================================================================

(rf/reg-event-fx
 :shell-command
 (fn [{:keys [db]} [_]]
   "Execute a shell command and display output (M-!)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Shell command: "
                      :on-confirm [:shell-command/execute]}]]]}))

(rf/reg-event-fx
 :shell-command/execute
 (fn [{:keys [db]} [_ command]]
   "Execute the shell command and show result."
   (let [output (execute-command command)]
     (if (= output :clear)
       {:fx [[:dispatch [:echo/message "clear: not applicable outside shell buffer"]]]}
       ;; Add to history
       {:db (update-in db [:shell :history] (fnil conj []) command)
        :fx [[:dispatch [:echo/message output]]]}))))

(rf/reg-event-fx
 :async-shell-command
 (fn [{:keys [db]} [_]]
   "Execute an async shell command (M-&)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Async shell command: "
                      :on-confirm [:shell-command/execute]}]]]}))

(rf/reg-event-fx
 :shell-command-on-region
 (fn [{:keys [db]} [_]]
   "Pipe region to shell command (M-|)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Shell command on region: "
                      :on-confirm [:shell-command/execute]}]]]}))

;; =============================================================================
;; Shell Input Processing
;; =============================================================================

(rf/reg-event-fx
 :shell/send-input
 (fn [{:keys [db]} [_ buffer-id input]]
   "Process input in shell buffer."
   (let [output (execute-command input)
         ^js wasm (get-in db [:buffers buffer-id :wasm-instance])]
     (when wasm
       (let [current-text (try (.getText wasm) (catch :default _ ""))
             new-output (if (= output :clear)
                          (make-shell-prompt db)
                          (str "\n" output "\n" (make-shell-prompt db)))
             new-text (if (= output :clear)
                        new-output
                        (str current-text new-output))
             lines (str/split new-text #"\n" -1)
             line-count (count lines)]
         (when (= output :clear)
           (let [len (.-length wasm)]
             (.delete wasm 0 len)))
         (when-not (= output :clear)
           (.insert wasm (.-length wasm) new-output))
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] new-text)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (update-in [:shell :history] (fnil conj []) input))})))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize shell/eshell module and register commands."
  []
  ;; Shell commands
  (rf/dispatch [:register-command :shell
                {:docstring "Open a shell buffer"
                 :interactive nil
                 :handler [:shell/open]}])

  (rf/dispatch [:register-command :eshell
                {:docstring "Open an Emacs shell (eshell) buffer"
                 :interactive nil
                 :handler [:eshell/open]}])

  (rf/dispatch [:register-command :shell-command
                {:docstring "Execute a shell command and display output (M-!)"
                 :interactive nil
                 :handler [:shell-command]}])

  (rf/dispatch [:register-command :async-shell-command
                {:docstring "Execute an async shell command (M-&)"
                 :interactive nil
                 :handler [:async-shell-command]}])

  (rf/dispatch [:register-command :shell-command-on-region
                {:docstring "Pipe region to shell command (M-|)"
                 :interactive nil
                 :handler [:shell-command-on-region]}]))
