(ns lexicon.packages.flymake
  "Flymake - on-the-fly syntax checking.

  flymake-mode runs syntax checkers asynchronously and displays
  diagnostics inline in the buffer. Supports multiple backends
  and provides navigation with flymake-goto-next-error/prev-error.

  Based on Emacs lisp/progmodes/flymake.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Diagnostic Types
;; =============================================================================

(def diagnostic-types
  "Diagnostic type definitions with severity and display properties."
  {:error   {:severity 100 :face "flymake-error"   :fringe "!" :prefix "E"}
   :warning {:severity 30  :face "flymake-warning" :fringe "?" :prefix "W"}
   :note    {:severity 10  :face "flymake-note"    :fringe "i" :prefix "I"}})

;; =============================================================================
;; State
;; =============================================================================

;; Per-buffer flymake state
;; {buffer-id {:enabled? bool
;;             :diagnostics [{:type :line :col :end-line :end-col :message :backend}]
;;             :backends #{:backend-name}
;;             :running? bool}}
(defonce flymake-state (atom {}))

;; Registry of backend functions
;; Each backend is a function: (fn [buffer-id buffer-text report-fn] ...)
;; report-fn is called with list of diagnostics
(defonce flymake-backends (atom {}))

;; Forward declarations
(declare flymake-start!)

;; =============================================================================
;; Diagnostic Management
;; =============================================================================

(defn make-diagnostic
  "Create a diagnostic object.

  Args:
  - type: :error, :warning, or :note
  - line: 1-indexed line number
  - col: 1-indexed column (optional)
  - message: diagnostic message
  - backend: keyword identifying the backend
  - end-line/end-col: end position (optional)"
  [{:keys [type line col message backend end-line end-col]}]
  {:type (or type :error)
   :line line
   :col (or col 1)
   :end-line (or end-line line)
   :end-col end-col
   :message message
   :backend backend})

(defn add-diagnostics!
  "Add diagnostics for a buffer from a backend."
  [buffer-id backend diagnostics]
  (swap! flymake-state update-in [buffer-id :diagnostics]
         (fn [existing]
           ;; Remove old diagnostics from this backend, add new ones
           (let [without-backend (remove #(= (:backend %) backend) existing)]
             (into (vec without-backend) diagnostics)))))

(defn clear-diagnostics!
  "Clear all diagnostics for a buffer."
  [buffer-id]
  (swap! flymake-state assoc-in [buffer-id :diagnostics] []))

(defn get-diagnostics
  "Get diagnostics for a buffer, optionally filtered by line range."
  ([buffer-id]
   (get-in @flymake-state [buffer-id :diagnostics] []))
  ([buffer-id start-line end-line]
   (filter #(and (>= (:line %) start-line)
                 (<= (:line %) end-line))
           (get-diagnostics buffer-id))))

(defn sort-diagnostics
  "Sort diagnostics by line, then column, then severity."
  [diagnostics]
  (sort-by (juxt :line :col #(- (get-in diagnostic-types [(:type %) :severity] 0)))
           diagnostics))

;; =============================================================================
;; Backend System
;; =============================================================================

(defn register-backend!
  "Register a flymake backend.

  Args:
  - name: keyword identifying the backend
  - backend-fn: (fn [buffer-id buffer-text report-fn] ...)
                Should call report-fn with diagnostics list when done"
  [name backend-fn]
  (swap! flymake-backends assoc name backend-fn))

(defn unregister-backend!
  "Unregister a flymake backend."
  [name]
  (swap! flymake-backends dissoc name))

;; =============================================================================
;; Built-in Backends
;; =============================================================================

(defn- clojure-syntax-backend
  "Simple Clojure syntax checker backend.
   Checks for unbalanced parens and basic issues."
  [buffer-id buffer-text report-fn]
  (let [diagnostics (atom [])
        lines (str/split-lines buffer-text)]

    ;; Check for unbalanced parens (simple check)
    (let [open-count (count (re-seq #"\(" buffer-text))
          close-count (count (re-seq #"\)" buffer-text))]
      (when (not= open-count close-count)
        (swap! diagnostics conj
               (make-diagnostic
                {:type :error
                 :line (count lines)
                 :message (str "Unbalanced parentheses: "
                               open-count " open, " close-count " close")
                 :backend :clojure-syntax}))))

    ;; Check for common issues
    (doseq [[idx line] (map-indexed vector lines)]
      ;; Check for trailing whitespace (as note)
      (when (re-find #"\s+$" line)
        (swap! diagnostics conj
               (make-diagnostic
                {:type :note
                 :line (inc idx)
                 :col (count (str/trimr line))
                 :message "Trailing whitespace"
                 :backend :clojure-syntax}))))

    ;; Report findings
    (report-fn @diagnostics)))

(defn- javascript-syntax-backend
  "Simple JavaScript syntax checker backend."
  [buffer-id buffer-text report-fn]
  (let [diagnostics (atom [])
        lines (str/split-lines buffer-text)]

    ;; Check for console.log (as warning in production)
    (doseq [[idx line] (map-indexed vector lines)]
      (when (re-find #"console\.log" line)
        (swap! diagnostics conj
               (make-diagnostic
                {:type :warning
                 :line (inc idx)
                 :col (inc (or (str/index-of line "console.log") 0))
                 :message "console.log statement"
                 :backend :javascript-syntax}))))

    (report-fn @diagnostics)))

;; =============================================================================
;; Mode-line Integration
;; =============================================================================

(defn format-mode-line
  "Format flymake status for mode line.
   Returns string like '[E:2 W:5 I:1]' or nil if no diagnostics."
  [buffer-id]
  (let [diagnostics (get-diagnostics buffer-id)]
    (when (seq diagnostics)
      (let [grouped (group-by :type diagnostics)
            errors (count (get grouped :error []))
            warnings (count (get grouped :warning []))
            notes (count (get grouped :note []))]
        (str "[Flymake:"
             (when (pos? errors) (str "E:" errors))
             (when (and (pos? errors) (pos? warnings)) " ")
             (when (pos? warnings) (str "W:" warnings))
             (when (and (pos? (+ errors warnings)) (pos? notes)) " ")
             (when (pos? notes) (str "I:" notes))
             "]")))))

;; =============================================================================
;; Flymake Commands
;; =============================================================================

(defn flymake-mode!
  "Toggle flymake-mode for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        currently-enabled? (get-in @flymake-state [buffer-id :enabled?] false)
        new-enabled? (not currently-enabled?)]
    (swap! flymake-state assoc-in [buffer-id :enabled?] new-enabled?)
    (if new-enabled?
      (do
        (flymake-start! buffer-id)
        (lisp/message "Flymake mode enabled"))
      (do
        (clear-diagnostics! buffer-id)
        (lisp/message "Flymake mode disabled")))))

(defn flymake-start!
  "Start flymake check for a buffer."
  ([] (flymake-start! (lisp/current-buffer)))
  ([buffer-id]
   (let [buffer-info (lisp/buffer-info buffer-id)
         text (lisp/buffer-text-of buffer-id)
         mode (:major-mode buffer-info)

         ;; Select backends based on mode
         backends (case mode
                    :clojure-mode [:clojure-syntax]
                    :javascript-mode [:javascript-syntax]
                    [:clojure-syntax])  ; Default

         report-fn (fn [diagnostics]
                     (swap! flymake-state assoc-in [buffer-id :running?] false)
                     (swap! flymake-state update-in [buffer-id :diagnostics]
                            (fn [existing]
                              (sort-diagnostics (into (vec (or existing [])) diagnostics)))))]

     ;; Mark as running
     (swap! flymake-state assoc-in [buffer-id :running?] true)

     ;; Run each backend
     (doseq [backend-name backends]
       (when-let [backend-fn (get @flymake-backends backend-name)]
         (try
           (backend-fn buffer-id text report-fn)
           (catch js/Error e
             (js/console.warn "Flymake backend error:" backend-name e))))))))

(defn flymake-goto-next-error!
  "Navigate to next diagnostic."
  []
  (let [buffer-id (lisp/current-buffer)
        diagnostics (sort-diagnostics (get-diagnostics buffer-id))
        cursor-line (lisp/current-line)
        ;; Find next diagnostic after current line
        next-diag (first (filter #(> (:line %) cursor-line) diagnostics))]
    (if next-diag
      (do
        (lisp/goto-line (:line next-diag))
        (lisp/message (:message next-diag)))
      (lisp/message "No more diagnostics"))))

(defn flymake-goto-prev-error!
  "Navigate to previous diagnostic."
  []
  (let [buffer-id (lisp/current-buffer)
        diagnostics (sort-diagnostics (get-diagnostics buffer-id))
        cursor-line (lisp/current-line)
        ;; Find previous diagnostic before current line
        prev-diag (last (filter #(< (:line %) cursor-line) diagnostics))]
    (if prev-diag
      (do
        (lisp/goto-line (:line prev-diag))
        (lisp/message (:message prev-diag)))
      (lisp/message "No more diagnostics"))))

(defn flymake-show-buffer-diagnostics!
  "Show buffer diagnostics in a special buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        diagnostics (sort-diagnostics (get-diagnostics buffer-id))]
    (if (empty? diagnostics)
      (lisp/message "No diagnostics in buffer")
      ;; Create diagnostics buffer
      (let [content (str "Flymake diagnostics:\n\n"
                         (str/join "\n"
                                   (map (fn [d]
                                          (str (:line d) ":"
                                               (:col d) " "
                                               (get-in diagnostic-types [(:type d) :prefix] "?")
                                               " " (:message d)))
                                        diagnostics)))]
        (lisp/create-special-buffer "*Flymake diagnostics*" content
                                     {:major-mode :special-mode
                                      :read-only true})
        (lisp/split-window-below)
        (lisp/switch-to-buffer "*Flymake diagnostics*")))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize flymake module."
  []
  ;; Register built-in backends
  (register-backend! :clojure-syntax clojure-syntax-backend)
  (register-backend! :javascript-syntax javascript-syntax-backend)

  ;; Register commands
  (lisp/define-command 'flymake-mode
    flymake-mode!
    "Toggle Flymake mode for on-the-fly syntax checking")

  (lisp/define-command 'flymake-start
    flymake-start!
    "Start a Flymake syntax check")

  (lisp/define-command 'flymake-goto-next-error
    flymake-goto-next-error!
    "Go to next Flymake diagnostic")

  (lisp/define-command 'flymake-goto-prev-error
    flymake-goto-prev-error!
    "Go to previous Flymake diagnostic")

  (lisp/define-command 'flymake-show-buffer-diagnostics
    flymake-show-buffer-diagnostics!
    "Show Flymake diagnostics for current buffer"))
