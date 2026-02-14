(ns lexicon.packages.kmacro
  "Keyboard Macro System - Record and replay key sequences.

  Emacs-compatible keyboard macro functionality:
  - F3: Start recording (or insert counter if already recording)
  - F4: Stop recording / execute last macro
  - C-x (: Start recording (alternative)
  - C-x ): Stop recording (alternative)
  - C-x e: Execute last macro

  Based on Emacs kmacro.el

  This package uses only lisp.cljs primitives."
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce recording? (atom false))
(defonce current-macro (atom []))
(defonce macro-ring (atom (lisp/make-ring 16)))
(defonce kmacro-counter (atom 0))
(defonce kmacro-counter-format (atom "%d"))
(defonce executing? (atom false))

;; =============================================================================
;; Core Functions
;; =============================================================================

(defn recording?-fn [] @recording?)
(defn executing?-fn [] @executing?)

(defn start-recording!
  ([] (start-recording! nil))
  ([prefix-arg]
   (when-not @recording?
     (reset! recording? true)
     (reset! current-macro [])
     (when (number? prefix-arg)
       (reset! kmacro-counter prefix-arg))
     (lisp/message "Defining keyboard macro..."))))

(defn stop-recording! []
  (when @recording?
    (reset! recording? false)
    (let [macro @current-macro]
      (if (seq macro)
        (do
          (swap! macro-ring #(lisp/ring-insert % macro))
          (lisp/message (str "Keyboard macro defined (" (count macro) " keys)")))
        (lisp/message "Empty keyboard macro not added to ring")))))

(defn- get-last-macro []
  (lisp/ring-ref @macro-ring 0))

(defn- insert-counter! [increment]
  (let [value @kmacro-counter
        formatted (try
                    (js/sprintf @kmacro-counter-format value)
                    (catch :default _ (str value)))]
    (swap! kmacro-counter + (or increment 1))
    (lisp/insert formatted)))

(defn- execute-macro-impl! [macro repeat-count]
  (reset! executing? true)
  (try
    (dotimes [_ repeat-count]
      (doseq [key-str macro]
        (lisp/simulate-key key-str)))
    (finally
      (reset! executing? false))))

(defn execute-macro!
  ([macro] (execute-macro! macro 1))
  ([macro repeat-count]
   (when (and (seq macro) (not @executing?) (not @recording?))
     (js/setTimeout #(execute-macro-impl! macro repeat-count) 0))))

(defn call-last-macro!
  ([] (call-last-macro! 1))
  ([repeat-count]
   (if-let [macro (get-last-macro)]
     (execute-macro! macro repeat-count)
     (lisp/message "No keyboard macro defined"))))

;; =============================================================================
;; Command Functions
;; =============================================================================

(defn kmacro-start-or-insert-counter! []
  (if @recording?
    (insert-counter! 1)
    (start-recording! nil)))

(defn kmacro-end-or-call! []
  (if @recording?
    (stop-recording!)
    (call-last-macro! 1)))

;; =============================================================================
;; Hook Handler
;; =============================================================================

(defn- on-key-pressed
  "Hook handler called when a key is pressed.
   Records the key if macro recording is active."
  [key-str]
  (when (and @recording? (not @executing?))
    ;; Don't record F3/F4 as they control the macro system
    (when-not (contains? #{"F3" "F4" "<f3>" "<f4>"} key-str)
      (swap! current-macro conj key-str))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-kmacro! []
  ;; Register hook handler for key recording
  (lisp/add-hook 'key-pressed-hook on-key-pressed)
  (lisp/define-command 'kmacro-start-macro kmacro-start-or-insert-counter!
    "Start defining a keyboard macro (F3)")

  (lisp/define-command 'kmacro-end-or-call-macro kmacro-end-or-call!
    "End macro definition or call last macro (F4)")

  (lisp/define-command 'start-kbd-macro start-recording!
    "Start defining a keyboard macro (C-x ()")

  (lisp/define-command 'end-kbd-macro stop-recording!
    "End keyboard macro definition (C-x ))")

  (lisp/define-command 'call-last-kbd-macro call-last-macro!
    "Execute the last keyboard macro (C-x e)")

  (lisp/global-set-key "F3" :kmacro-start-macro)
  (lisp/global-set-key "F4" :kmacro-end-or-call-macro)
  (lisp/global-set-key "C-x (" :start-kbd-macro)
  (lisp/global-set-key "C-x )" :end-kbd-macro)
  (lisp/global-set-key "C-x e" :call-last-kbd-macro))
