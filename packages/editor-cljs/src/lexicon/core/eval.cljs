(ns lexicon.core.eval
  "Runtime evaluation using SCI (Small Clojure Interpreter).

  Provides safe evaluation of ClojureScript code at runtime with:
  - eval-string: Evaluate a string of code
  - eval-region: Evaluate text between positions
  - eval-last-sexp: Evaluate expression before point
  - eval-expression: Read from minibuffer and evaluate

  Security:
  - Only approved namespaces are accessible
  - No direct file system or network access
  - Sandboxed evaluation environment

  Phase 6.5 Week 7-8"
  (:require [re-frame.core :as rf]
            [sci.core :as sci]
            [lexicon.core.db :as db]
            [lexicon.core.api.message :as msg]
            [lexicon.core.api.buffer :as buf]
            [lexicon.core.minibuffer :as minibuffer]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; SCI Context Configuration
;; =============================================================================

(def ^:private lexicon-api-namespace
  "Lexicon API functions exposed to evaluated code."
  {'message msg/message
   'point buf/point
   'point-min buf/point-min
   'point-max buf/point-max
   'goto-char buf/goto-char
   'buffer-string buf/buffer-string
   'buffer-substring buf/buffer-substring
   'line-beginning-position buf/line-beginning-position
   'line-end-position buf/line-end-position
   'get-buffer buf/get-buffer
   'buffer-name buf/buffer-name
   'buffer-size buf/buffer-size
   'buffer-local-value buf/buffer-local-value})

;; Store for command function bodies
(defonce command-functions (atom {}))

(defn defcommand-impl
  "Implementation of defcommand function for SCI.

  Usage: (defcommand 'hello (fn [] (message \"hello\")) \"Doc\")

  Registers command that can be invoked via invoke-command."
  [cmd-name cmd-fn doc-str]
  (let [;; Convert symbol to keyword for command registry
        cmd-keyword (if (symbol? cmd-name) (keyword cmd-name) cmd-name)
        command-def {:name cmd-keyword
                     :doc (or doc-str "")
                     :handler [:sci/eval-command cmd-keyword]
                     :fn cmd-fn}]
    ;; Store function for execution
    (swap! command-functions assoc cmd-keyword cmd-fn)
    ;; Register command
    (rf/dispatch-sync [:register-command cmd-keyword command-def])
    cmd-keyword))

;; Atom to hold sci-context (allows setq-sci to access it)
(defonce sci-context-atom (atom nil))

(defn setq-impl
  "Runtime implementation of setq that interns variables into SCI context.

  This makes variables defined via setq available for subsequent evaluations.
  Called from the setq macro with quoted symbol and evaluated value."
  [var val]
  ;; Store in global-vars for compatibility
  (swap! lisp/global-vars assoc var val)
  ;; Intern into SCI 'user namespace so bare symbols resolve
  (when-let [ctx @sci-context-atom]
    (sci/intern ctx 'user var val))
  val)

(def sci-context
  ;; SCI evaluation context with allowed namespaces.
  ;;
  ;; Security:
  ;; - Only approved lexicon.* namespaces accessible
  ;; - SCI provides safe cljs.core by default
  ;; - Math and console allowed for convenience
  ;; - No eval, Function, network access, or timers
  (sci/init {:namespaces {'lexicon.core lexicon-api-namespace
                          'lexicon.api.buffer lexicon-api-namespace
                          'lexicon.api.message {'message msg/message}
                          ;; Use full lisp.cljs API as user namespace (includes markers)
                          'user (merge lisp/sci-namespace
                                       {'defcommand defcommand-impl
                                        ;; Override setq runtime implementation
                                        'setq-impl setq-impl
                                        ;; Emacs compatibility: t = true, nil already works
                                        't true})}
             :classes {'js {'Math js/Math         ; Allow Math for arithmetic
                            'Date js/Date          ; Allow Date for timestamps
                            'console js/console}   ; Allow console for debugging
                       'Math js/Math}
             :disable-arity-checks false}))

;; Initialize the atom so setq-impl can use it
(reset! sci-context-atom sci-context)

;; Define setq macro in the SCI context
;; This makes setq work like a special form (doesn't evaluate first arg)
(sci/eval-string* sci-context
  "(defmacro setq
     [& pairs]
     (let [pair-forms (partition 2 pairs)]
       `(do
          ~@(map (fn [[sym val]]
                   `(setq-impl '~sym ~val))
                 pair-forms))))")

;; Define save-excursion macro - saves point, mark, and buffer, restores on exit
(sci/eval-string* sci-context
  "(defmacro save-excursion
     [& body]
     `(let [saved-point# (point)
            saved-mark# (mark)
            saved-buffer# (current-buffer)]
        (try
          (do ~@body)
          (finally
            (when (not= (current-buffer) saved-buffer#)
              (set-buffer saved-buffer#))
            (goto-char saved-point#)
            (when saved-mark#
              (set-mark saved-mark#))))))")

;; Define while macro for loops
(sci/eval-string* sci-context
  "(defmacro while
     [test & body]
     `(loop []
        (when ~test
          ~@body
          (recur))))")

;; Define let* as alias for let (Emacs compatibility)
(sci/eval-string* sci-context
  "(defmacro let* [bindings & body]
     `(let ~bindings ~@body))")

;; Define progn as alias for do (Emacs compatibility)
(sci/eval-string* sci-context
  "(defmacro progn [& body]
     `(do ~@body))")

;; =============================================================================
;; Emacs Error Handling: condition-case, signal, error
;; =============================================================================

;; Define signal function - raises an error with Emacs-style error data
;; Error data is a vector: [error-symbol message & extra-data]
(sci/eval-string* sci-context
  "(defn signal
     \"Signal an error. ERROR-SYMBOL is a symbol identifying the error type.
      DATA is additional error information (typically a string message).

      Example: (signal 'file-error \\\"File not found\\\")\"
     [error-symbol & data]
     (throw (ex-info (str error-symbol \": \" (first data))
                     {:error-symbol error-symbol
                      :error-data (vec (cons error-symbol data))})))")

;; Define error function - convenience wrapper for signal with 'error symbol
(sci/eval-string* sci-context
  "(defn error
     \"Signal an error with symbol 'error and formatted message.

      Example: (error \\\"Something went wrong\\\")\"
     [& args]
     (signal 'error (apply str args)))")

;; Define condition-case macro - Emacs-style exception handling
;; Syntax: (condition-case var bodyform (error-sym handler...) ...)
;; NOTE: Non-local exits (throw*) propagate through - they're not errors
(sci/eval-string* sci-context
  "(defmacro condition-case
     \"Handle errors in BODYFORM with handlers for specific error types.

      VAR is bound to error data [error-symbol message ...] in handlers.
      Use nil for VAR if you don't need the error data.

      Each handler is (ERROR-SYMBOL HANDLER-FORMS...) where ERROR-SYMBOL
      matches the first element of error data. Use 'error to catch all errors.

      NOTE: Non-local exits via throw* are NOT caught - they propagate through.

      Example:
        (condition-case err
            (risky-operation)
          (file-error (message \\\"File error: %s\\\" (second err)))
          (error (message \\\"Error: %s\\\" (second err))))\"
     [var bodyform & handlers]
     (let [e-sym (gensym \"e\")
           data-sym (gensym \"data\")
           err-type-sym (gensym \"err-type\")]
       `(try
          ~bodyform
          (catch :default ~e-sym
            ;; Let non-local exits propagate through - they're not errors
            (if (:lexicon/non-local-exit (ex-data ~e-sym))
              (throw ~e-sym)
              (let [~data-sym (or (:error-data (ex-data ~e-sym))
                                  ['~'error (ex-message ~e-sym)])
                    ~err-type-sym (first ~data-sym)
                    ~@(when var [var data-sym])]
                (cond
                  ~@(mapcat
                      (fn [handler]
                        (let [error-type (first handler)
                              handler-body (rest handler)]
                          [(if (= error-type 'error)
                             true  ; 'error matches all errors
                             `(= ~err-type-sym '~error-type))
                           `(do ~@handler-body)]))
                      handlers)
                  :else (throw ~e-sym))))))))")

;; Define unwind-protect macro - guarantees cleanup forms run
(sci/eval-string* sci-context
  "(defmacro unwind-protect
     \"Execute BODYFORM, guaranteeing CLEANUP-FORMS will run afterward.

      CLEANUP-FORMS run whether BODYFORM completes normally, throws an error,
      or exits via non-local jump. Similar to try/finally.

      Example:
        (unwind-protect
            (progn
              (lock-resource)
              (use-resource))
          (unlock-resource))\"
     [bodyform & cleanup-forms]
     `(try
        ~bodyform
        (finally
          ~@cleanup-forms)))")

;; =============================================================================
;; Emacs Non-Local Exits: catch/throw
;; =============================================================================
;;
;; Unlike condition-case/signal which handle ERRORS, catch/throw is for
;; CONTROL FLOW - jumping out of nested computations to a named exit point.

;; Define throw* function for non-local exits (named throw* to avoid shadowing clojure throw)
(sci/eval-string* sci-context
  "(defn throw*
     \"Non-local exit to matching catch* point.
      Searches up the call stack for (catch* TAG ...) with matching TAG.
      Immediately exits to that catch*, which returns VALUE.\"
     [tag value]
     (throw (ex-info \"non-local-exit\"
                     {:lexicon/non-local-exit true
                      :tag tag
                      :value value})))")

;; Define catch* macro for non-local exit points (named catch* to avoid shadowing clojure catch)
(sci/eval-string* sci-context
  "(defmacro catch*
     \"Establish a non-local exit point with TAG.
      Executes BODY forms. If (throw* TAG VALUE) is called during execution,
      immediately returns VALUE. Otherwise returns the result of BODY.\"
     [tag & body]
     (let [e# (gensym \"e\")
           tag-val# (gensym \"tag\")]
       `(let [~tag-val# ~tag]
          (try
            (do ~@body)
            (catch :default ~e#
              (let [data# (ex-data ~e#)]
                (if (and (:lexicon/non-local-exit data#)
                         (= (:tag data#) ~tag-val#))
                  (:value data#)
                  (throw ~e#))))))))")

;; =============================================================================
;; Core Evaluation Functions
;; =============================================================================

(defn eval-string
  "Evaluate a string of ClojureScript code using SCI.

  Args:
    code-str - String of ClojureScript code to evaluate
    opts - Optional map with:
           :bindings - Additional bindings for evaluation
           :ns - Namespace to evaluate in (default: user)

  Returns:
    Result of evaluation, or error map if evaluation fails

  Example:
    (eval-string \"(+ 1 2 3)\")  ; => 6
    (eval-string \"(message \\\"Hello!\\\")\")  ; Displays message"
  ([code-str]
   (eval-string code-str {}))
  ([code-str opts]
   (try
     (let [result (sci/eval-string* sci-context code-str)]
       {:success true :result result})
     (catch :default e
       {:success false
        :error (ex-message e)
        :data (ex-data e)}))))

(defn eval-region
  "Evaluate text between START and END positions in current buffer.

  Args:
    db - Application database
    start - Start position (linear byte offset)
    end - End position (linear byte offset)

  Returns:
    Evaluation result map (see eval-string)"
  [db start end]
  (let [code-str (buf/buffer-substring db start end)]
    (eval-string code-str)))

(defn find-sexp-start
  "Find the start position of the s-expression before POS.

  This is a simplified implementation that looks backwards for:
  - Opening paren (
  - Opening bracket [
  - Opening brace {

  Args:
    text - Buffer text
    pos - Current position

  Returns:
    Start position of s-expression, or 0 if not found"
  [text pos]
  (if (zero? pos)
    0
    (loop [i (dec pos)
           paren-depth 0]
      (if (< i 0)
        0  ; Reached beginning of buffer
        (let [ch (nth text i)]
          (cond
            ;; Found opening delimiter at depth 0
            (and (zero? paren-depth)
                 (or (= ch \()
                     (= ch \[)
                     (= ch \{)))
            i

            ;; Closing delimiter - increase depth
            (or (= ch \))
                (= ch \])
                (= ch \}))
            (recur (dec i) (inc paren-depth))

            ;; Opening delimiter - decrease depth
            (or (= ch \()
                (= ch \[)
                (= ch \{))
            (recur (dec i) (dec paren-depth))

            ;; Other character - continue
            :else
            (recur (dec i) paren-depth)))))))

(defn eval-last-sexp
  "Evaluate the s-expression before point in current buffer.

  Args:
    db - Application database

  Returns:
    Evaluation result map (see eval-string)"
  [db]
  (let [cursor-pos (get-in db [:ui :cursor-position] 0)
        buffer-text (buf/buffer-string db)
        start (find-sexp-start buffer-text cursor-pos)
        end cursor-pos]
    (eval-region db start end)))

;; =============================================================================
;; Re-frame Event Handlers
;; =============================================================================

(rf/reg-event-fx
 :eval/string
 (fn [{:keys [db]} [_ code-str]]
   "Evaluate a string of code and display result in minibuffer.

   Args:
     code-str - String of ClojureScript code"
   (let [result (eval-string code-str)]
     (if (:success result)
       {:fx [[:dispatch [:message (str (:result result))]]]}
       {:fx [[:dispatch [:message (str "Error: " (:error result))]]]}))))

(rf/reg-event-fx
 :eval/last-sexp
 (fn [{:keys [db]} [_]]
   "Evaluate the s-expression before point (C-x C-e).

   Evaluates the expression and displays result in echo area."
   (let [result (eval-last-sexp db)]
     (if (:success result)
       {:fx [[:dispatch [:message (str (:result result))]]]}
       {:fx [[:dispatch [:message (str "Error: " (:error result))]]]}))))

(rf/reg-event-fx
 :eval/expression
 (fn [_ [_]]
   "Read an expression from minibuffer and evaluate it (M-:).

   Opens minibuffer with 'Eval: ' prompt."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Eval: "
                      :on-confirm (fn [input]
                                    (rf/dispatch [:eval/string input]))
                      :initial-input ""
                      :allow-empty? false}]]]}))

(rf/reg-event-db
  :sci/eval-command
  (fn [db [_ cmd-name]]
    "Execute a command defined via defcommand in SCI."
    (let [cmd-fn (get @command-functions cmd-name)]
      (when cmd-fn
        (try
          ;; Call the command function
          (cmd-fn)
          (catch :default e
            (.error js/console "Command error:" (ex-message e))))))
    ;; Return the current db state (which may have been modified by swap! in message)
    @re-frame.db/app-db))

;; =============================================================================
;; Command Registration
;; =============================================================================

(rf/dispatch-sync
 [:register-command
  :eval-last-sexp
  {:docstring "Evaluate sexp before point and print value in the echo area"
   :interactive-spec nil
   :handler [:eval/last-sexp]}])

(rf/dispatch-sync
 [:register-command
  :eval-expression
  {:docstring "Evaluate EXP and print value in the echo area"
   :interactive-spec nil
   :handler [:eval/expression]}])
