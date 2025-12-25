(ns lexicon.buffer-local
  "Buffer-local variables subsystem.

  In Emacs, variables can have buffer-local bindings. Each buffer can have its own
  value for a variable, independent of the global value.

  Two types of buffer-local variables:
  1. Per-buffer local: Made local via (make-local-variable 'var) in specific buffer
  2. Automatically buffer-local: All buffers get local copy (make-variable-buffer-local)

  Implementation:
  - Global values stored in [:variables :global var-name]
  - Buffer-local values stored in [:buffers buffer-id :local-variables var-name]
  - Automatically buffer-local vars tracked in [:variables :auto-buffer-local]"
  (:require [re-frame.core :as rf]))

;; -- Variable Access --

(defn get-variable
  "Get value of VARIABLE in BUFFER-ID, falling back to global value.

  Args:
  - db: Application database
  - buffer-id: Buffer ID (nil for global only)
  - variable: Keyword variable name

  Returns: Variable value or nil"
  [db buffer-id variable]
  (if buffer-id
    ;; Try buffer-local first, then global
    (or (get-in db [:buffers buffer-id :local-variables variable])
        (get-in db [:variables :global variable]))
    ;; No buffer ID, return global
    (get-in db [:variables :global variable])))

(defn set-variable!
  "Set value of VARIABLE in BUFFER-ID or globally.

  If variable is buffer-local in buffer, sets buffer-local value.
  Otherwise sets global value."
  [buffer-id variable value]
  (rf/dispatch [:variables/set buffer-id variable value]))

(rf/reg-event-db
  :variables/set
  (fn [db [_ buffer-id variable value]]
    (if buffer-id
      ;; Check if variable is buffer-local in this buffer
      (let [is-local? (or (get-in db [:buffers buffer-id :local-variables-set variable])
                         (get-in db [:variables :auto-buffer-local variable]))]
        (if is-local?
          ;; Set buffer-local value
          (assoc-in db [:buffers buffer-id :local-variables variable] value)
          ;; Set global value
          (assoc-in db [:variables :global variable] value)))
      ;; No buffer ID, set global
      (assoc-in db [:variables :global variable] value))))

(defn get-variable-sub
  "Subscription for variable value in current buffer."
  [variable]
  @(rf/subscribe [:variables/get variable]))

(rf/reg-sub
  :variables/get
  (fn [db [_ variable]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])]
      (get-variable db buffer-id variable))))

;; -- Buffer-Local Variable Management --

(defn make-local-variable
  "Make VARIABLE buffer-local in BUFFER-ID.

  After this, any setq in this buffer will set the buffer-local value.
  The global value is unaffected.

  Returns: variable name"
  [buffer-id variable]
  (rf/dispatch [:variables/make-local buffer-id variable])
  variable)

(rf/reg-event-db
  :variables/make-local
  (fn [db [_ buffer-id variable]]
    (-> db
        ;; Mark variable as buffer-local in this buffer
        (assoc-in [:buffers buffer-id :local-variables-set variable] true)
        ;; Initialize buffer-local value from global if not already set
        (update-in [:buffers buffer-id :local-variables variable]
                   (fn [current]
                     (if (nil? current)
                       (get-in db [:variables :global variable])
                       current))))))

(defn make-variable-buffer-local
  "Make VARIABLE automatically buffer-local in ALL buffers.

  All future buffers will automatically have a local copy of this variable.

  Returns: variable name"
  [variable]
  (rf/dispatch [:variables/make-buffer-local variable])
  variable)

(rf/reg-event-db
  :variables/make-buffer-local
  (fn [db [_ variable]]
    ;; Mark variable as automatically buffer-local
    (assoc-in db [:variables :auto-buffer-local variable] true)))

(defn buffer-local-value
  "Get buffer-local value of VARIABLE in BUFFER-ID.
  Returns nil if variable is not buffer-local in buffer."
  [db buffer-id variable]
  (get-in db [:buffers buffer-id :local-variables variable]))

(defn kill-local-variable
  "Remove buffer-local binding of VARIABLE in BUFFER-ID.
  Variable returns to its global value in this buffer."
  [buffer-id variable]
  (rf/dispatch [:variables/kill-local buffer-id variable]))

(rf/reg-event-db
  :variables/kill-local
  (fn [db [_ buffer-id variable]]
    (-> db
        (update-in [:buffers buffer-id :local-variables] dissoc variable)
        (update-in [:buffers buffer-id :local-variables-set] dissoc variable))))

;; -- Helper Macros (as functions in ClojureScript) --

(defn setq-local
  "Set buffer-local value of VARIABLE in current buffer.
  Makes variable buffer-local if not already.

  Usage: (setq-local buffer-id :my-var 123)"
  [buffer-id variable value]
  (make-local-variable buffer-id variable)
  (set-variable! buffer-id variable value))

(defn defvar-local
  "Define a buffer-local variable with optional default value.

  Usage: (defvar-local :my-buffer-local-var default-value)"
  [variable & [default-value]]
  (make-variable-buffer-local variable)
  (when default-value
    (rf/dispatch [:variables/set nil variable default-value])))

;; -- Subscriptions --

(rf/reg-sub
  :variables/is-buffer-local?
  (fn [db [_ buffer-id variable]]
    (or (get-in db [:buffers buffer-id :local-variables-set variable])
        (get-in db [:variables :auto-buffer-local variable]))))

(rf/reg-sub
  :variables/auto-buffer-local?
  (fn [db [_ variable]]
    (get-in db [:variables :auto-buffer-local variable])))

(rf/reg-sub
  :variables/all-buffer-local
  (fn [db [_ buffer-id]]
    (get-in db [:buffers buffer-id :local-variables] {})))

;; -- Initialization --

(defn initialize-buffer-local-variables!
  "Initialize buffer-local variable system."
  []
  (rf/dispatch-sync [:variables/initialize]))

(rf/reg-event-db
  :variables/initialize
  (fn [db [_]]
    (-> db
        (assoc-in [:variables :global] {})
        (assoc-in [:variables :auto-buffer-local] {}))))

;; Auto-initialize on namespace load
(initialize-buffer-local-variables!)

;; -- Common Buffer-Local Variables --

;; Define standard buffer-local variables
(defn define-standard-buffer-local-vars! []
  ;; Major mode is buffer-local
  (defvar-local :major-mode :fundamental-mode)

  ;; Minor modes list is buffer-local
  (defvar-local :minor-modes #{})

  ;; Fill column (text wrapping) is buffer-local
  (defvar-local :fill-column 80)

  ;; Tab width is buffer-local
  (defvar-local :tab-width 8)

  ;; Indent style is buffer-local
  (defvar-local :indent-tabs-mode nil)

  ;; Comment syntax is buffer-local
  (defvar-local :comment-start nil)
  (defvar-local :comment-end nil)

  ;; Syntax table is buffer-local
  (defvar-local :syntax-table nil)

  ;; Completion-at-point functions are buffer-local
  (defvar-local :completion-at-point-functions []))

;; Initialize standard vars
(define-standard-buffer-local-vars!)
