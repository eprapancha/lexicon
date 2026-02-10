(ns lexicon.packages.compile
  "Compilation mode - run commands and parse errors.

  M-x compile prompts for a command and creates a *compilation* buffer
  showing the output. Error messages are parsed and navigable with
  next-error (M-g n) and previous-error (M-g p).

  Based on Emacs lisp/progmodes/compile.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Error Regexp Patterns
;; =============================================================================

;; Simplified error patterns for common compilers/tools
;; Format: {:name :pattern :file-group :line-group :column-group :type-group}
(def compilation-error-regexps
  [{:name :gnu
    ;; GNU GCC: file:line:col: error/warning: message
    :pattern #"^([^:\s]+):(\d+):(\d+):\s*(error|warning|note):"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group 4}
   {:name :gnu-simple
    ;; GNU simple: file:line: error/warning: message
    :pattern #"^([^:\s]+):(\d+):\s*(error|warning|note):"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group 3}
   {:name :clojure
    ;; Clojure: Syntax error compiling at (file:line:col)
    :pattern #"at \(([^:]+):(\d+):(\d+)\)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :javascript
    ;; JavaScript/Node: file:line
    :pattern #"^\s+at .+ \(([^:]+):(\d+):(\d+)\)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :python
    ;; Python: File "file", line N
    :pattern #"File \"([^\"]+)\", line (\d+)"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group nil}
   {:name :rust
    ;; Rust: --> file:line:col
    :pattern #"^\s*--> ([^:]+):(\d+):(\d+)"
    :file-group 1
    :line-group 2
    :column-group 3
    :type-group nil}
   {:name :generic
    ;; Generic: file:line
    :pattern #"^([^:\s][^:]*):(\d+):"
    :file-group 1
    :line-group 2
    :column-group nil
    :type-group nil}])

;; =============================================================================
;; State
;; =============================================================================

;; Store compilation state
(defonce compilation-state
  (atom {:last-command "make -k"
         :errors []           ; Parsed errors [{:file :line :col :type :message :buffer-pos}]
         :error-index 0       ; Current error index for navigation
         :source-buffer nil})) ; Buffer we were in when compile was invoked

;; =============================================================================
;; Error Parsing
;; =============================================================================

(defn- parse-error-line
  "Try to parse a line as an error message.
   Returns {:file :line :col :type :message :buffer-pos} or nil."
  [line buffer-pos]
  (some (fn [{:keys [pattern file-group line-group column-group type-group]}]
          (when-let [match (re-find pattern line)]
            (let [groups (if (string? match) [match] (vec match))]
              {:file (get groups file-group)
               :line (when-let [l (get groups line-group)]
                       (js/parseInt l 10))
               :col (when (and column-group (get groups column-group))
                      (js/parseInt (get groups column-group) 10))
               :type (or (get groups type-group) "error")
               :message line
               :buffer-pos buffer-pos})))
        compilation-error-regexps))

(defn- parse-compilation-output
  "Parse compilation output and extract errors.
   Returns vector of error maps."
  [output]
  (let [lines (str/split-lines output)]
    (loop [remaining lines
           pos 0
           errors []]
      (if (empty? remaining)
        errors
        (let [line (first remaining)
              line-length (inc (count line)) ; +1 for newline
              error (parse-error-line line pos)]
          (recur (rest remaining)
                 (+ pos line-length)
                 (if error (conj errors error) errors)))))))

(defn- count-by-type
  "Count errors by type (error, warning, note)."
  [errors]
  (let [grouped (group-by #(str/lower-case (or (:type %) "error")) errors)]
    {:errors (count (get grouped "error" []))
     :warnings (count (get grouped "warning" []))
     :notes (count (get grouped "note" []))}))

;; =============================================================================
;; Buffer Creation
;; =============================================================================

(defn- format-compilation-header
  "Format the compilation buffer header."
  [command]
  (str "-*- mode: compilation; default-directory: \"" js/location.pathname "\" -*-\n"
       "Compilation started at " (.toLocaleTimeString (js/Date.)) "\n\n"
       command "\n\n"))

(defn- format-compilation-footer
  "Format the compilation buffer footer."
  [exit-code counts]
  (let [{:keys [errors warnings notes]} counts]
    (str "\n\nCompilation "
         (if (zero? exit-code) "finished" (str "exited abnormally with code " exit-code))
         " at " (.toLocaleTimeString (js/Date.))
         (when (pos? (+ errors warnings notes))
           (str " [" errors " error(s), " warnings " warning(s)]")))))

;; =============================================================================
;; Compilation Simulation (browser environment)
;; =============================================================================

(defn- simulate-compilation
  "Simulate compilation output since we can't run actual commands in browser.
   Returns simulated output with some errors for demonstration."
  [command]
  (cond
    ;; Simulate make output
    (str/starts-with? command "make")
    (str "make: Entering directory '/home/user/project'\n"
         "gcc -c -o main.o main.c\n"
         "main.c:10:5: warning: unused variable 'x' [-Wunused-variable]\n"
         "main.c:15:10: error: expected ';' before '}' token\n"
         "make: *** [Makefile:5: main.o] Error 1\n"
         "make: Leaving directory '/home/user/project'\n")

    ;; Simulate npm/node output
    (or (str/starts-with? command "npm")
        (str/starts-with? command "node"))
    (str "npm WARN deprecated\n"
         "> project@1.0.0 build\n"
         "> tsc\n"
         "src/index.ts:5:3: error TS2304: Cannot find name 'foo'.\n"
         "src/utils.ts:12:15: warning: Unused import 'bar'.\n")

    ;; Simulate clojure output
    (or (str/starts-with? command "clj")
        (str/starts-with? command "lein"))
    (str "Compiling project...\n"
         "Syntax error compiling at (src/core.clj:15:3)\n"
         "Unable to resolve symbol: undefined-fn\n")

    ;; Default: echo the command
    :else
    (str "Running: " command "\n"
         "[Simulated compilation - actual command execution not available in browser]\n"
         "Compilation complete.\n")))

;; =============================================================================
;; Commands
;; =============================================================================

(defn- goto-error!
  "Go to error location."
  [error]
  (let [{:keys [file line col]} error]
    ;; Display error info in echo area
    (lisp/message (str file ":" line
                       (when col (str ":" col))
                       " - " (or (:type error) "error")))))

(defn compile-run!
  "Run the compilation with the given command."
  [command]
  ;; Store the command for next time
  (swap! compilation-state assoc :last-command command)
  (swap! compilation-state assoc :source-buffer (lisp/current-buffer))
  (swap! compilation-state assoc :errors [])
  (swap! compilation-state assoc :error-index 0)

  ;; In browser, we can't actually run shell commands
  ;; So we simulate compilation output for demonstration
  (let [simulated-output (simulate-compilation command)
        errors (parse-compilation-output simulated-output)
        counts (count-by-type errors)
        header (format-compilation-header command)
        footer (format-compilation-footer 0 counts)
        content (str header simulated-output footer)]

    ;; Store parsed errors
    (swap! compilation-state assoc :errors errors)

    ;; Kill existing buffer if any
    (when-let [existing-id (lisp/get-buffer "*compilation*")]
      (lisp/kill-buffer existing-id))

    ;; Create the *compilation* buffer using lisp primitive
    (let [_buffer-id (lisp/create-special-buffer "*compilation*" content
                                                  {:major-mode :compilation-mode
                                                   :read-only true})]
      (lisp/switch-to-buffer "*compilation*")
      (lisp/message (str "Compilation finished: "
                         (:errors counts) " error(s), "
                         (:warnings counts) " warning(s)")))))

(defn compile-interactive
  "M-x compile - Prompt for command."
  []
  (lisp/read-from-minibuffer
   "Compile command: "
   compile-run!))

(defn recompile!
  "M-x recompile - rerun last compilation."
  []
  (let [last-cmd (:last-command @compilation-state)]
    (if last-cmd
      (compile-run! last-cmd)
      (compile-interactive))))

(defn next-error!
  "Visit next compilation error message and corresponding source code."
  []
  (let [errors (:errors @compilation-state)
        current-idx (:error-index @compilation-state)
        new-idx (min (dec (count errors)) (inc current-idx))]
    (if (empty? errors)
      (lisp/message "No compilation errors")
      (if (>= new-idx (count errors))
        (lisp/message "Moved past last error")
        (let [error (get errors new-idx)]
          (swap! compilation-state assoc :error-index new-idx)
          (goto-error! error))))))

(defn previous-error!
  "Visit previous compilation error message and corresponding source code."
  []
  (let [errors (:errors @compilation-state)
        current-idx (:error-index @compilation-state)
        new-idx (max 0 (dec current-idx))]
    (if (empty? errors)
      (lisp/message "No compilation errors")
      (if (< new-idx 0)
        (lisp/message "Moved past first error")
        (let [error (get errors new-idx)]
          (swap! compilation-state assoc :error-index new-idx)
          (goto-error! error))))))

(defn kill-compilation!
  "Kill the process made by the compile command."
  []
  ;; In real implementation, would kill the process
  (lisp/message "Compilation process killed"))

(defn goto-error-at-point!
  "Go to error at point in compilation buffer."
  []
  (let [cursor-pos (lisp/point)
        errors (:errors @compilation-state)]
    ;; Find error closest to cursor position
    (if-let [error (first (filter #(<= (:buffer-pos %) cursor-pos) (reverse errors)))]
      (goto-error! error)
      (lisp/message "No error at point"))))

(defn compilation-next-error!
  "Move to next error line in buffer."
  []
  (next-error!))

(defn compilation-previous-error!
  "Move to previous error line in buffer."
  []
  (previous-error!))

(defn quit-window!
  "Quit the current window, burying its buffer."
  []
  ;; Bury the current buffer (switch to previous buffer)
  ;; In Emacs, quit-window optionally kills the buffer for temp buffers
  (let [source-buffer (:source-buffer @compilation-state)
        ;; Find *scratch* buffer as fallback
        scratch-buffer-id (lisp/get-buffer "*scratch*")]
    (cond
      source-buffer
      (lisp/switch-to-buffer source-buffer)
      scratch-buffer-id
      (lisp/switch-to-buffer scratch-buffer-id)
      :else
      (lisp/message "No buffer to switch to"))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-compile!
  "Initialize compile module and register commands."
  []
  ;; Main commands
  (lisp/define-command 'compile
    compile-interactive
    "Compile the program including the current buffer")

  (lisp/define-command 'recompile
    recompile!
    "Re-execute the last compilation command")

  (lisp/define-command 'next-error
    next-error!
    "Visit next compilation error message and corresponding source code")

  (lisp/define-command 'previous-error
    previous-error!
    "Visit previous compilation error message and corresponding source code")

  (lisp/define-command 'kill-compilation
    kill-compilation!
    "Kill the process made by the compile command")

  (lisp/define-command 'quit-window
    quit-window!
    "Quit the current window, burying its buffer")

  ;; Compilation buffer specific commands
  (lisp/define-command 'compile-goto-error-at-point
    goto-error-at-point!
    "Go to error at point in compilation buffer")

  (lisp/define-command 'compilation-next-error
    compilation-next-error!
    "Move to next error in compilation buffer")

  (lisp/define-command 'compilation-previous-error
    compilation-previous-error!
    "Move to previous error in compilation buffer")

  ;; compilation-mode keybindings
  (lisp/define-key-for-mode :compilation-mode "g" :recompile)
  (lisp/define-key-for-mode :compilation-mode "RET" :compile-goto-error-at-point)
  (lisp/define-key-for-mode :compilation-mode "n" :compilation-next-error)
  (lisp/define-key-for-mode :compilation-mode "p" :compilation-previous-error)
  (lisp/define-key-for-mode :compilation-mode "q" :quit-window))
