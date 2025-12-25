(ns lexicon.hooks
  "Enhanced hook system for Lexicon.

  Hooks are lists of functions that are called at specific points:
  - pre-command-hook: Before every command
  - post-command-hook: After every command
  - window-configuration-change-hook: When windows change
  - kill-buffer-hook: Before killing a buffer
  - change-major-mode-hook: Before changing major mode

  Hooks can be buffer-local (each buffer has own list) or global.

  Usage:
    (add-hook! :post-command-hook my-fn)           ; Add globally
    (add-hook! :post-command-hook my-fn buffer-id) ; Add buffer-locally
    (remove-hook! :post-command-hook my-fn)        ; Remove globally
    (run-hooks! :post-command-hook)                ; Run all functions"
  (:require [re-frame.core :as rf]
            [lexicon.buffer-local :as buffer-local]))

;; -- Hook Management --

(defn add-hook!
  "Add FUNCTION to HOOK.

  Args:
  - hook: Hook keyword (:pre-command-hook, etc.)
  - function: Function to add
  - buffer-id: Optional buffer ID for buffer-local hook

  If buffer-id is provided, adds function to buffer-local hook.
  Otherwise adds to global hook."
  [hook function & [buffer-id]]
  (rf/dispatch [:hooks/add hook function buffer-id]))

(rf/reg-event-db
  :hooks/add
  (fn [db [_ hook function buffer-id]]
    (if buffer-id
      ;; Add to buffer-local hook
      (update-in db [:buffers buffer-id :hooks hook]
                 (fn [hooks]
                   (let [hooks (or hooks [])]
                     (if (some #{function} hooks)
                       hooks  ; Already in list
                       (conj hooks function)))))
      ;; Add to global hook
      (update-in db [:hooks hook]
                 (fn [hooks]
                   (let [hooks (or hooks [])]
                     (if (some #{function} hooks)
                       hooks
                       (conj hooks function))))))))

(defn remove-hook!
  "Remove FUNCTION from HOOK.

  Args:
  - hook: Hook keyword
  - function: Function to remove
  - buffer-id: Optional buffer ID for buffer-local hook"
  [hook function & [buffer-id]]
  (rf/dispatch [:hooks/remove hook function buffer-id]))

(rf/reg-event-db
  :hooks/remove
  (fn [db [_ hook function buffer-id]]
    (if buffer-id
      ;; Remove from buffer-local hook
      (update-in db [:buffers buffer-id :hooks hook]
                 (fn [hooks]
                   (vec (remove #{function} (or hooks [])))))
      ;; Remove from global hook
      (update-in db [:hooks hook]
                 (fn [hooks]
                   (vec (remove #{function} (or hooks []))))))))

(defn get-hook-functions
  "Get list of functions for HOOK in BUFFER-ID.

  Returns combined list of buffer-local + global functions."
  [db hook buffer-id]
  (let [global-hooks (get-in db [:hooks hook] [])
        buffer-local-hooks (when buffer-id
                            (get-in db [:buffers buffer-id :hooks hook] []))]
    ;; Buffer-local hooks run before global hooks
    (concat buffer-local-hooks global-hooks)))

(defn run-hook-functions
  "Run all functions in FUNCTIONS list.

  Args:
  - functions: Vector of functions
  - db: Current database (passed to each function)

  Each function receives the database and returns updated database.
  Functions are chained (output of one becomes input to next)."
  [functions db]
  (reduce (fn [acc-db f]
           (try
             (or (f acc-db) acc-db)
             (catch js/Error e
               (js/console.warn "Hook function error:" e)
               acc-db)))
         db
         functions))

;; -- Hook Running --

(defn run-hooks!
  "Run all functions in HOOK for current buffer.

  Args:
  - hook: Hook keyword"
  [hook]
  (rf/dispatch [:hooks/run hook]))

(rf/reg-event-db
  :hooks/run
  (fn [db [_ hook]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])
          functions (get-hook-functions db hook buffer-id)]
      (run-hook-functions functions db))))

;; -- Command Hooks Integration --

;; Interceptor to run pre/post-command hooks
(def command-hooks-interceptor
  (rf/->interceptor
   :id :command-hooks
   :before (fn [context]
            ;; Run pre-command-hook
            (let [db (get-in context [:coeffects :db])
                  buffer-id (get-in db [:editor :current-buffer-id])
                  functions (get-hook-functions db :pre-command-hook buffer-id)
                  updated-db (run-hook-functions functions db)]
              (assoc-in context [:coeffects :db] updated-db)))
   :after (fn [context]
           ;; Run post-command-hook
           (let [db (get-in context [:effects :db])
                 buffer-id (get-in db [:editor :current-buffer-id])
                 functions (get-hook-functions db :post-command-hook buffer-id)
                 updated-db (run-hook-functions functions db)]
             (assoc-in context [:effects :db] updated-db)))))

;; -- Specific Hooks --

;; window-configuration-change-hook
(defn run-window-configuration-change-hook! []
  (rf/dispatch [:hooks/run :window-configuration-change-hook]))

(rf/reg-event-db
  :hooks/run-window-change
  (fn [db [_]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])
          functions (get-hook-functions db :window-configuration-change-hook buffer-id)]
      (run-hook-functions functions db))))

;; kill-buffer-hook
(defn run-kill-buffer-hook!
  "Run kill-buffer-hook for BUFFER-ID before killing buffer."
  [buffer-id]
  (rf/dispatch [:hooks/run-kill-buffer buffer-id]))

(rf/reg-event-db
  :hooks/run-kill-buffer
  (fn [db [_ buffer-id]]
    (let [functions (get-hook-functions db :kill-buffer-hook buffer-id)]
      (run-hook-functions functions db))))

;; change-major-mode-hook
(defn run-change-major-mode-hook!
  "Run change-major-mode-hook for BUFFER-ID before changing mode."
  [buffer-id]
  (rf/dispatch [:hooks/run-change-major-mode buffer-id]))

(rf/reg-event-db
  :hooks/run-change-major-mode
  (fn [db [_ buffer-id]]
    (let [functions (get-hook-functions db :change-major-mode-hook buffer-id)]
      (run-hook-functions functions db))))

;; -- Subscriptions --

(rf/reg-sub
  :hooks/get
  (fn [db [_ hook buffer-id]]
    (get-hook-functions db hook buffer-id)))

(rf/reg-sub
  :hooks/global
  (fn [db [_ hook]]
    (get-in db [:hooks hook] [])))

(rf/reg-sub
  :hooks/buffer-local
  (fn [db [_ hook buffer-id]]
    (get-in db [:buffers buffer-id :hooks hook] [])))

;; -- Initialization --

(defn initialize-hooks!
  "Initialize hook system."
  []
  (rf/dispatch-sync [:hooks/initialize]))

(rf/reg-event-db
  :hooks/initialize
  (fn [db [_]]
    (assoc db :hooks {:pre-command-hook []
                     :post-command-hook []
                     :window-configuration-change-hook []
                     :kill-buffer-hook []
                     :change-major-mode-hook []})))

;; Auto-initialize on namespace load
(initialize-hooks!)

;; -- Hook Utilities --

(defn with-hooks
  "Wrap RE-FRAME-EVENT-ID to run pre/post-command hooks.

  This should be applied to all interactive commands."
  [re-frame-event-id]
  ;; In ClojureScript, we can't easily wrap event handlers
  ;; Instead, commands should manually call run-hooks! or use interceptor
  ;; For now, return event-id unchanged - interceptors will handle it
  re-frame-event-id)

;; -- Common Hook Functions (Examples) --

;; Example: Track last command
(defn track-last-command [db]
  (let [current-command (get-in db [:editor :current-command])]
    (assoc-in db [:editor :last-command] current-command)))

;; Example: Update mode line
(defn update-mode-line-on-command [db]
  ;; Trigger mode-line recalculation
  (assoc-in db [:ui :mode-line-dirty?] true))

;; Register some default hooks
(defn register-default-hooks! []
  ;; Track last command
  (add-hook! :post-command-hook track-last-command)

  ;; Update mode line after commands
  (add-hook! :post-command-hook update-mode-line-on-command))

;; Initialize default hooks
(register-default-hooks!)
