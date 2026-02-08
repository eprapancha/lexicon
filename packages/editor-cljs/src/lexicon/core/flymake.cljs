(ns lexicon.core.flymake
  "Flymake - on-the-fly syntax checking.

  flymake-mode runs syntax checkers asynchronously and displays
  diagnostics inline in the buffer. Supports multiple backends
  and provides navigation with flymake-goto-next-error/prev-error.

  Based on Emacs lisp/progmodes/flymake.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
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

(defn add-diagnostics
  "Add diagnostics for a buffer from a backend."
  [buffer-id backend diagnostics]
  (swap! flymake-state update-in [buffer-id :diagnostics]
         (fn [existing]
           ;; Remove old diagnostics from this backend, add new ones
           (let [without-backend (remove #(= (:backend %) backend) existing)]
             (into (vec without-backend) diagnostics)))))

(defn clear-diagnostics
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

;; Registry of backend functions
;; Each backend is a function: (fn [buffer-id buffer-text report-fn] ...)
;; report-fn is called with list of diagnostics
(defonce flymake-backends (atom {}))

(defn register-backend
  "Register a flymake backend.

  Args:
  - name: keyword identifying the backend
  - backend-fn: (fn [buffer-id buffer-text report-fn] ...)
                Should call report-fn with diagnostics list when done"
  [name backend-fn]
  (swap! flymake-backends assoc name backend-fn))

(defn unregister-backend
  "Unregister a flymake backend."
  [name]
  (swap! flymake-backends dissoc name))

;; =============================================================================
;; Built-in Backends
;; =============================================================================

(defn clojure-syntax-backend
  "Simple Clojure syntax checker backend.
   Checks for unbalanced parens and basic issues."
  [_buffer-id buffer-text report-fn]
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

(defn javascript-syntax-backend
  "Simple JavaScript syntax checker backend."
  [_buffer-id buffer-text report-fn]
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

;; Register built-in backends
(register-backend :clojure-syntax clojure-syntax-backend)
(register-backend :javascript-syntax javascript-syntax-backend)

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
;; Re-frame Events
;; =============================================================================

;; Enable flymake-mode for current buffer
(rf/reg-event-fx
 :flymake-mode
 (fn [{:keys [_db]} [_ enable?]]
   (let [buffer-id (lisp/current-buffer)
         currently-enabled? (get-in @flymake-state [buffer-id :enabled?] false)
         new-enabled? (if (nil? enable?)
                        (not currently-enabled?)
                        enable?)]
     (swap! flymake-state assoc-in [buffer-id :enabled?] new-enabled?)
     (if new-enabled?
       {:fx [[:dispatch [:flymake/start buffer-id]]
             [:dispatch [:echo/message "Flymake mode enabled"]]]}
       {:fx [[:dispatch [:flymake/clear buffer-id]]
             [:dispatch [:echo/message "Flymake mode disabled"]]]}))))

;; Start flymake check
(rf/reg-event-fx
 :flymake/start
 (fn [{:keys [_db]} [_ buffer-id]]
   (let [buffer-id (or buffer-id (lisp/current-buffer))
         buffer-info (lisp/buffer-info buffer-id)
         text (lisp/buffer-text-of buffer-id)
         mode (:major-mode buffer-info)

         ;; Select backends based on mode
         backends (case mode
                    :clojure-mode [:clojure-syntax]
                    :javascript-mode [:javascript-syntax]
                    [:clojure-syntax])  ; Default

         report-fn (fn [diagnostics]
                    (rf/dispatch [:flymake/report buffer-id diagnostics]))]

     ;; Mark as running
     (swap! flymake-state assoc-in [buffer-id :running?] true)

     ;; Run each backend
     (doseq [backend-name backends]
       (when-let [backend-fn (get @flymake-backends backend-name)]
         (try
           (backend-fn buffer-id text report-fn)
           (catch js/Error e
             (js/console.warn "Flymake backend error:" backend-name e)))))

     {})))

;; Report diagnostics from backend
(rf/reg-event-fx
 :flymake/report
 (fn [{:keys [_db]} [_ buffer-id diagnostics]]
   (swap! flymake-state assoc-in [buffer-id :running?] false)
   (swap! flymake-state update-in [buffer-id :diagnostics]
          (fn [existing]
            (sort-diagnostics (into (vec (or existing [])) diagnostics))))
   {}))

;; Clear diagnostics
(rf/reg-event-fx
 :flymake/clear
 (fn [{:keys [_db]} [_ buffer-id]]
   (let [buffer-id (or buffer-id (lisp/current-buffer))]
     (clear-diagnostics buffer-id)
     {})))

;; Navigate to next diagnostic
(rf/reg-event-fx
 :flymake-goto-next-error
 (fn [{:keys [_db]} [_ _n filter-type]]
   ;; TODO: Implement count argument (currently always moves by 1)
   (let [buffer-id (lisp/current-buffer)
         diagnostics (sort-diagnostics (get-diagnostics buffer-id))
         ;; Apply filter if specified
         diagnostics (if filter-type
                       (filter #(= (:type %) filter-type) diagnostics)
                       diagnostics)
         cursor-line (lisp/current-line)
         ;; Find next diagnostic after current line
         next-diag (first (filter #(> (:line %) cursor-line) diagnostics))]
     (if next-diag
       {:fx [[:dispatch [:goto-line (:line next-diag)]]
             [:dispatch [:echo/message (:message next-diag)]]]}
       {:fx [[:dispatch [:echo/message "No more diagnostics"]]]}))))

;; Navigate to previous diagnostic
(rf/reg-event-fx
 :flymake-goto-prev-error
 (fn [{:keys [_db]} [_ _n filter-type]]
   ;; TODO: Implement count argument (currently always moves by 1)
   (let [buffer-id (lisp/current-buffer)
         diagnostics (sort-diagnostics (get-diagnostics buffer-id))
         diagnostics (if filter-type
                       (filter #(= (:type %) filter-type) diagnostics)
                       diagnostics)
         cursor-line (lisp/current-line)
         ;; Find previous diagnostic before current line
         prev-diag (last (filter #(< (:line %) cursor-line) diagnostics))]
     (if prev-diag
       {:fx [[:dispatch [:goto-line (:line prev-diag)]]
             [:dispatch [:echo/message (:message prev-diag)]]]}
       {:fx [[:dispatch [:echo/message "No more diagnostics"]]]}))))

;; Show buffer diagnostics
(rf/reg-event-fx
 :flymake-show-buffer-diagnostics
 (fn [{:keys [_db]} [_]]
   (let [buffer-id (lisp/current-buffer)
         diagnostics (sort-diagnostics (get-diagnostics buffer-id))]
     (if (empty? diagnostics)
       {:fx [[:dispatch [:echo/message "No diagnostics in buffer"]]]}
       ;; Create diagnostics buffer
       (let [content (str "Flymake diagnostics:\n\n"
                          (str/join "\n"
                                    (map (fn [d]
                                           (str (:line d) ":"
                                                (:col d) " "
                                                (get-in diagnostic-types [(:type d) :prefix] "?")
                                                " " (:message d)))
                                         diagnostics)))]
         {:fx [[:dispatch [:create-special-buffer
                           {:name "*Flymake diagnostics*"
                            :content content
                            :mode :special-mode}]]]})))))

;; =============================================================================
;; Command Registration
;; =============================================================================

(defn register-flymake-commands! []
  (rf/dispatch-sync
   [:register-command :flymake-mode
    {:docstring "Toggle Flymake mode for on-the-fly syntax checking"
     :handler [:flymake-mode]}])

  (rf/dispatch-sync
   [:register-command :flymake-start
    {:docstring "Start a Flymake syntax check"
     :handler [:flymake/start]}])

  (rf/dispatch-sync
   [:register-command :flymake-goto-next-error
    {:docstring "Go to next Flymake diagnostic"
     :handler [:flymake-goto-next-error]}])

  (rf/dispatch-sync
   [:register-command :flymake-goto-prev-error
    {:docstring "Go to previous Flymake diagnostic"
     :handler [:flymake-goto-prev-error]}])

  (rf/dispatch-sync
   [:register-command :flymake-show-buffer-diagnostics
    {:docstring "Show Flymake diagnostics for current buffer"
     :handler [:flymake-show-buffer-diagnostics]}]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-flymake! []
  (register-flymake-commands!))
