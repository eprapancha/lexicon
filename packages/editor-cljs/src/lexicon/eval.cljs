(ns lexicon.eval
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
            [lexicon.db :as db]
            [lexicon.api.message :as msg]
            [lexicon.api.buffer :as buf]
            [lexicon.minibuffer :as minibuffer]))

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

(defonce sci-context
  ;; SCI evaluation context with allowed namespaces.
  ;;
  ;; Security:
  ;; - Only approved lexicon.* namespaces accessible
  ;; - SCI provides safe cljs.core by default
  ;; - Math and console allowed for convenience
  ;; - No eval, Function, network access, or timers
  (sci/init {:namespaces {'lexicon.core lexicon-api-namespace
                          'lexicon.api.buffer lexicon-api-namespace
                          'lexicon.api.message {'message msg/message}}
             :classes {'js {'Math js/Math         ; Allow Math for arithmetic
                            'Date js/Date          ; Allow Date for timestamps
                            'console js/console}   ; Allow console for debugging
                       'Math js/Math}
             :disable-arity-checks false}))

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
