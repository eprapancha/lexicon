(ns lexicon.packages.eldoc
  "Eldoc mode - show function arglist or variable docstring in echo area.

  Implements Emacs eldoc.el functionality. Shows documentation for
  the symbol at point in the echo area after a brief idle delay.

  Key features:
  - eldoc-mode: Buffer-local minor mode
  - global-eldoc-mode: Enable eldoc in all supported buffers
  - eldoc-documentation-functions: Hook for documentation providers
  - Shows function signatures and variable docstrings

  Based on Emacs eldoc.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def eldoc-idle-delay
  "Number of seconds of idle time before displaying documentation.
   Default is 0.5 seconds."
  500)  ; milliseconds

(def eldoc-minor-mode-string " ElDoc")

;; =============================================================================
;; Documentation Providers
;; =============================================================================

;; eldoc-documentation-functions is a hook where documentation providers
;; register themselves. Each function takes no arguments and should return
;; documentation string or nil.

;; Atom holding registered documentation functions.
;; Each entry is {:name keyword :fn function :priority number}
(defonce documentation-functions (atom []))

(defn register-documentation-function!
  "Register a documentation provider function.
   Priority determines call order (higher = first)."
  [name doc-fn & {:keys [priority] :or {priority 0}}]
  (swap! documentation-functions
         (fn [fns]
           (conj (vec (remove #(= (:name %) name) fns))
                 {:name name :fn doc-fn :priority priority}))))

(defn unregister-documentation-function!
  "Unregister a documentation provider."
  [name]
  (swap! documentation-functions
         (fn [fns]
           (vec (remove #(= (:name %) name) fns)))))

;; =============================================================================
;; State
;; =============================================================================

;; Whether eldoc-mode is enabled per buffer
(defonce eldoc-mode-enabled (atom #{}))

;; Whether global-eldoc-mode is enabled
(defonce global-eldoc-mode-enabled? (atom false))

;; Timer for idle delay
(defonce eldoc-timer (atom nil))

;; =============================================================================
;; Symbol at Point Detection
;; =============================================================================

(defn- get-symbol-at-point
  "Extract the symbol at current point from buffer text.
   Returns {:symbol string :start pos :end pos} or nil."
  []
  (let [text (lisp/buffer-string)
        point (lisp/point)]
    (when (and text (< point (count text)))
      (let [;; Find word boundaries
            before-text (subs text 0 (min point (count text)))
            after-text (subs text (min point (count text)))
            ;; Match word chars before and after point
            start-match (re-find #"[\w-]*$" before-text)
            end-match (re-find #"^[\w-]*" after-text)
            symbol (str start-match end-match)
            start (- point (count start-match))
            end (+ point (count end-match))]
        (when (and (not (str/blank? symbol))
                   (> (count symbol) 1))  ; Ignore single chars
          {:symbol symbol
           :start start
           :end end})))))

;; =============================================================================
;; Documentation Lookup
;; =============================================================================

(defn- get-lisp-function-doc
  "Get documentation for a Lisp function.
   Checks known functions for arglist info."
  [symbol-name]
  ;; Simple built-in documentation for common Lisp functions
  (case symbol-name
    "if" "(if COND THEN ELSE...) - conditional"
    "let" "(let BINDINGS BODY...) - local bindings"
    "defn" "(defn NAME [ARGS] BODY...) - define function"
    "fn" "(fn [ARGS] BODY...) - anonymous function"
    "do" "(do FORMS...) - evaluate forms sequentially"
    "when" "(when COND BODY...) - conditional without else"
    "cond" "(cond CLAUSES...) - multi-branch conditional"
    "loop" "(loop BINDINGS BODY...) - loop with recur"
    "recur" "(recur ARGS...) - tail recursive call"
    "defmacro" "(defmacro NAME [ARGS] BODY...) - define macro"
    "quote" "(quote FORM) or 'FORM - prevent evaluation"
    "insert" "(insert STRING...) - insert text at point"
    "point" "(point) - return current position"
    "goto-char" "(goto-char POS) - move to position"
    "buffer-string" "(buffer-string) - return buffer contents"
    "buffer-name" "(buffer-name) - return buffer name"
    "current-buffer" "(current-buffer) - return current buffer"
    "set-buffer" "(set-buffer BUFFER) - switch to buffer"
    "save-excursion" "(save-excursion BODY...) - save point and mark"
    "with-current-buffer" "(with-current-buffer BUF BODY...) - execute in buffer"
    "message" "(message FMT ARGS...) - display in echo area"
    "format" "(format FMT ARGS...) - format string"
    "concat" "(concat STRINGS...) - concatenate strings"
    "substring" "(substring STRING START END) - extract substring"
    "string-match" "(string-match REGEXP STRING) - search string"
    "replace-regexp-in-string" "(replace-regexp-in-string REGEXP REP STRING) - replace"
    "search-forward" "(search-forward STRING &optional BOUND NOERROR) - search"
    "re-search-forward" "(re-search-forward REGEXP &optional BOUND NOERROR) - regexp search"
    "looking-at" "(looking-at REGEXP) - match at point"
    "match-string" "(match-string NUM) - return matched group"
    nil))

(defn- collect-documentation
  "Collect documentation from all registered providers.
   Returns first non-nil result."
  []
  (let [fns (sort-by :priority > @documentation-functions)]
    (loop [remaining fns]
      (if (empty? remaining)
        nil
        (let [{:keys [fn]} (first remaining)
              result (try (fn) (catch :default _ nil))]
          (if result
            result
            (recur (rest remaining))))))))

(defn- get-documentation-for-symbol
  "Get documentation for a symbol, checking various sources."
  [symbol-info]
  (when symbol-info
    (let [sym (:symbol symbol-info)]
      (or
       ;; Check registered documentation functions first
       (collect-documentation)
       ;; Check Lisp functions
       (get-lisp-function-doc sym)))))

;; =============================================================================
;; Timer Management
;; =============================================================================

(defn- cancel-eldoc-timer!
  "Cancel any pending eldoc timer."
  []
  (when-let [t @eldoc-timer]
    (js/clearTimeout t)
    (reset! eldoc-timer nil)))

(defn- display-documentation!
  "Display documentation for symbol at point in echo area."
  []
  (let [buffer-id (lisp/current-buffer)
        eldoc-enabled? (contains? @eldoc-mode-enabled buffer-id)
        global-eldoc? @global-eldoc-mode-enabled?]
    (when (or eldoc-enabled? global-eldoc?)
      (let [symbol-info (get-symbol-at-point)
            doc (get-documentation-for-symbol symbol-info)]
        (when doc
          (lisp/message doc))))))

(defn- schedule-eldoc-display!
  "Schedule eldoc display after idle delay."
  []
  (cancel-eldoc-timer!)
  (reset! eldoc-timer
          (js/setTimeout display-documentation! eldoc-idle-delay)))

;; =============================================================================
;; Eldoc Commands
;; =============================================================================

(defn eldoc-mode!
  "Toggle eldoc-mode for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        enabled? (contains? @eldoc-mode-enabled buffer-id)]
    (if enabled?
      (do
        (swap! eldoc-mode-enabled disj buffer-id)
        (lisp/message "Eldoc mode disabled"))
      (do
        (swap! eldoc-mode-enabled conj buffer-id)
        (lisp/message "Eldoc mode enabled")))))

(defn global-eldoc-mode!
  "Toggle global-eldoc-mode."
  []
  (let [new-state (not @global-eldoc-mode-enabled?)]
    (reset! global-eldoc-mode-enabled? new-state)
    (lisp/message (if new-state
                    "Global eldoc mode enabled"
                    "Global eldoc mode disabled"))))

;; =============================================================================
;; Post-Command Hook Integration
;; =============================================================================

(defn on-post-command
  "Called after each command to potentially update eldoc display.
   This should be called from the command execution system."
  []
  (schedule-eldoc-display!))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize eldoc module and register commands."
  []
  (lisp/define-command 'eldoc-mode
    eldoc-mode!
    "Toggle ElDoc mode - show documentation in echo area")

  (lisp/define-command 'global-eldoc-mode
    global-eldoc-mode!
    "Toggle global ElDoc mode for all supported buffers")

  ;; Register built-in documentation provider for Lisp
  (register-documentation-function!
   :elisp-basic
   (fn []
     (let [symbol-info (get-symbol-at-point)]
       (when symbol-info
         (get-lisp-function-doc (:symbol symbol-info)))))
   :priority 0))
