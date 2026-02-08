(ns lexicon.core.eldoc
  "Eldoc mode - show function arglist or variable docstring in echo area.

  Implements Emacs eldoc.el functionality. Shows documentation for
  the symbol at point in the echo area after a brief idle delay.

  Key features:
  - eldoc-mode: Buffer-local minor mode
  - global-eldoc-mode: Enable eldoc in all supported buffers
  - eldoc-documentation-functions: Hook for documentation providers
  - Shows function signatures and variable docstrings

  Based on Emacs eldoc.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
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
;; Symbol at Point Detection
;; =============================================================================

(defn- get-symbol-at-point
  "Extract the symbol at current point from buffer text.
   Returns {:symbol string :start pos :end pos} or nil."
  [_db]
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

(defn- get-command-doc
  "Get documentation for a command by name."
  [db cmd-name]
  (let [cmd-keyword (keyword cmd-name)
        cmd (get-in db [:commands cmd-keyword])]
    (when cmd
      (str cmd-name ": " (or (:docstring cmd) "No documentation")))))

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
  [db]
  (let [fns (sort-by :priority > @documentation-functions)]
    (loop [remaining fns]
      (if (empty? remaining)
        nil
        (let [{:keys [fn]} (first remaining)
              result (try (fn db) (catch :default _ nil))]
          (if result
            result
            (recur (rest remaining))))))))

(defn- get-documentation-for-symbol
  "Get documentation for a symbol, checking various sources."
  [db symbol-info]
  (when symbol-info
    (let [sym (:symbol symbol-info)]
      (or
       ;; Check registered documentation functions first
       (collect-documentation db)
       ;; Check commands
       (get-command-doc db sym)
       ;; Check Lisp functions
       (get-lisp-function-doc sym)))))

;; =============================================================================
;; Timer Management
;; =============================================================================

(defonce eldoc-timer (atom nil))

(defn- cancel-eldoc-timer!
  "Cancel any pending eldoc timer."
  []
  (when-let [t @eldoc-timer]
    (js/clearTimeout t)
    (reset! eldoc-timer nil)))

(defn- schedule-eldoc-display!
  "Schedule eldoc display after idle delay."
  []
  (cancel-eldoc-timer!)
  (reset! eldoc-timer
          (js/setTimeout
           (fn []
             (rf/dispatch [:eldoc/display-documentation]))
           eldoc-idle-delay)))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :eldoc/display-documentation
 (fn [{:keys [db]} [_]]
   "Display documentation for symbol at point in echo area."
   (let [buffer-id (lisp/current-buffer)
         buffer-info (lisp/buffer-info buffer-id)
         minor-modes (:minor-modes buffer-info)
         eldoc-enabled? (contains? minor-modes :eldoc-mode)
         global-eldoc? (get-in db [:settings :global-eldoc-mode] false)]
     (when (or eldoc-enabled? global-eldoc?)
       (let [symbol-info (get-symbol-at-point db)
             doc (get-documentation-for-symbol db symbol-info)]
         (when doc
           {:fx [[:dispatch [:echo/message doc]]]}))))))

(rf/reg-event-db
 :eldoc/toggle-mode
 (fn [db [_ buffer-id]]
   "Toggle eldoc-mode for a buffer."
   (let [buffer-id (or buffer-id (lisp/current-buffer))]
     (if buffer-id
       (update-in db [:buffers buffer-id :minor-modes]
                  (fn [modes]
                    (let [modes (or modes #{})]
                      (if (contains? modes :eldoc-mode)
                        (disj modes :eldoc-mode)
                        (conj modes :eldoc-mode)))))
       db))))

(rf/reg-event-db
 :eldoc/toggle-global-mode
 (fn [db [_]]
   "Toggle global-eldoc-mode."
   (update-in db [:settings :global-eldoc-mode] not)))

;; =============================================================================
;; Commands
;; =============================================================================

(rf/reg-event-fx
 :command/eldoc-mode
 (fn [{:keys [_db]} [_]]
   "Toggle eldoc-mode in current buffer."
   {:fx [[:dispatch [:eldoc/toggle-mode nil]]
         [:dispatch [:echo/message "eldoc-mode toggled"]]]}))

(rf/reg-event-fx
 :command/global-eldoc-mode
 (fn [{:keys [_db]} [_]]
   "Toggle global-eldoc-mode."
   {:fx [[:dispatch [:eldoc/toggle-global-mode]]
         [:dispatch [:echo/message "global-eldoc-mode toggled"]]]}))

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
  ;; Register eldoc-mode command
  (rf/dispatch [:register-command :eldoc-mode
                {:docstring "Toggle ElDoc mode - show documentation in echo area"
                 :interactive nil
                 :handler [:command/eldoc-mode]}])

  ;; Register global-eldoc-mode command
  (rf/dispatch [:register-command :global-eldoc-mode
                {:docstring "Toggle global ElDoc mode for all supported buffers"
                 :interactive nil
                 :handler [:command/global-eldoc-mode]}])

  ;; Register built-in documentation provider for Lisp
  (register-documentation-function!
   :elisp-basic
   (fn [db]
     (let [symbol-info (get-symbol-at-point db)]
       (when symbol-info
         (get-lisp-function-doc (:symbol symbol-info)))))
   :priority 0))
