(ns lexicon.events.hooks
  "Event handlers for hook system.

  Hooks allow packages to extend editor behavior without modifying core code.
  Used by: all modes, directory-local variables, package initialization.

  See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Priority 4: Hooks System"
  (:require [re-frame.core :as rf]
            [lexicon.log :as log]))

;;; =============================================================================
;;; Hook Management
;;; =============================================================================

(rf/reg-event-db
 :hooks/add
 (fn [db [_ hook-name hook-fn]]
   "Add function to hook.

   Hook functions are called in order they were added.

   Args:
     hook-name - Keyword identifying the hook (e.g., :before-change-functions)
     hook-fn   - Function to add to hook

   Usage:
     (rf/dispatch [:hooks/add :before-change-functions my-fn])"
   (log/debug (str "Adding hook: " hook-name))
   (update-in db [:hooks hook-name] (fnil conj []) hook-fn)))

(rf/reg-event-db
 :hooks/remove
 (fn [db [_ hook-name hook-fn]]
   "Remove function from hook.

   Uses identical? for comparison, so you must pass the exact same function
   object that was added.

   Args:
     hook-name - Keyword identifying the hook
     hook-fn   - Function to remove

   Usage:
     (rf/dispatch [:hooks/remove :before-change-functions my-fn])"
   (log/debug (str "Removing hook: " hook-name))
   (update-in db [:hooks hook-name]
              (fn [hooks]
                (vec (remove #(identical? % hook-fn) hooks))))))

(rf/reg-event-db
 :hooks/clear
 (fn [db [_ hook-name]]
   "Clear all functions from hook.

   Args:
     hook-name - Keyword identifying the hook

   Usage:
     (rf/dispatch [:hooks/clear :before-change-functions])"
   (log/debug (str "Clearing hook: " hook-name))
   (assoc-in db [:hooks hook-name] [])))

;;; =============================================================================
;;; Running Hooks
;;; =============================================================================

(rf/reg-fx
 :hooks/run
 (fn [{:keys [hook-name args]}]
   "Effect to run all functions in a hook.

   Hooks are run synchronously in order they were added.
   Errors in one hook don't prevent subsequent hooks from running.

   Args:
     hook-name - Keyword identifying the hook
     args      - Vector of arguments to pass to each hook function

   Usage:
     {:fx [[:hooks/run {:hook-name :before-change-functions
                        :args [start end]}]]}"
   (let [hooks (get-in @re-frame.db/app-db [:hooks hook-name] [])]
     (log/debug (str "Running hook: " hook-name " (" (count hooks) " functions)"))
     (doseq [hook-fn hooks]
       (try
         (apply hook-fn args)
         (catch js/Error e
           (log/error (str "Error in hook " hook-name ": " (.-message e)))
           ;; Log but continue with other hooks
           (rf/dispatch [:echo/message (str "Hook error: " (.-message e))])))))))

(rf/reg-event-fx
 :hooks/run
 (fn [{:keys [db]} [_ hook-name & args]]
   "Run all functions in a hook (event version).

   This is the event version - dispatches the :hooks/run effect.

   Args:
     hook-name - Keyword identifying the hook
     args      - Arguments to pass to each hook function

   Usage:
     (rf/dispatch [:hooks/run :before-change-functions start end])"
   {:fx [[:hooks/run {:hook-name hook-name :args args}]]}))

;;; =============================================================================
;;; Standard Emacs Hooks
;;; =============================================================================

(rf/reg-event-fx
 :hooks/before-change
 (fn [{:keys [db]} [_ buffer-id start end]]
   "Run before-change-functions hook.

   Called before any buffer modification.

   Args:
     buffer-id - Buffer being modified
     start     - Start position of change
     end       - End position of change"
   {:fx [[:hooks/run {:hook-name :before-change-functions
                      :args [start end]}]]}))

(rf/reg-event-fx
 :hooks/after-change
 (fn [{:keys [db]} [_ buffer-id start end old-len]]
   "Run after-change-functions hook.

   Called after any buffer modification.

   Args:
     buffer-id - Buffer that was modified
     start     - Start position of change
     end       - End position after change
     old-len   - Length of deleted text (0 for insert)"
   {:fx [[:hooks/run {:hook-name :after-change-functions
                      :args [start end old-len]}]]}))

(rf/reg-event-fx
 :hooks/mode
 (fn [{:keys [db]} [_ mode-name]]
   "Run mode hook (e.g., dired-mode-hook).

   Called when a major mode is activated.

   Args:
     mode-name - Keyword identifying the mode (e.g., :dired-mode)"
   (let [hook-name (keyword (str (name mode-name) "-hook"))]
     {:fx [[:hooks/run {:hook-name hook-name :args []}]]})))

;;; =============================================================================
;;; Buffer-Local Hooks (Future)
;;; =============================================================================

(comment
  "Future: Buffer-local hooks

  Emacs supports buffer-local hooks via (add-hook 'hook-name fn nil t)
  where the final 't' argument makes it buffer-local.

  Implementation would store hooks in :buffers buffer-id :local-hooks.
  :hooks/run would check both global and buffer-local hooks.

  Deferred until basic hook system is working."

  (rf/reg-event-db
   :hooks/add-local
   (fn [db [_ buffer-id hook-name hook-fn]]
     "Add buffer-local hook"
     (update-in db [:buffers buffer-id :local-hooks hook-name]
                (fnil conj [])
                hook-fn))))

;;; =============================================================================
;;; Hook Variants (Future)
;;; =============================================================================

(comment
  "Future: run-hook-with-args-until-success and run-hook-with-args-until-failure

  These are hook variants that stop early based on return values.

  - until-success: Stops at first non-nil return, returns that value
  - until-failure: Stops at first nil return, returns nil

  Used for query/validation chains.

  Deferred until basic hook system is working."

  (rf/reg-fx
   :hooks/run-until-success
   (fn [{:keys [hook-name args]}]
     (let [hooks (get-in @re-frame.db/app-db [:hooks hook-name] [])]
       (loop [remaining hooks]
         (when (seq remaining)
           (let [result (try
                         (apply (first remaining) args)
                         (catch js/Error e
                           nil))]
             (if result
               result
               (recur (rest remaining))))))))))
