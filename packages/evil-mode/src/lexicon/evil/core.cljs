(ns lexicon.evil.core
  "Evil-mode package - Vim emulation for Lexicon"
  (:require [re-frame.core :as rf]
            [lexicon.packages :as packages]
            [lexicon.evil.fsm.events :as fsm-events]
            [lexicon.evil.fsm.interceptors :as fsm-interceptors]
            [lexicon.evil.command.dispatcher :as evil-dispatcher]
            [lexicon.evil.keymaps.registry :as evil-keymaps]))

;; -- FSM State Default --

(def default-fsm-state
  "Default FSM state structure for Evil-mode"
  {:current-state :normal
   :previous-state nil
   :state-context {}
   :operator-pending nil
   :motion-pending nil
   :count-register nil
   :register-name nil
   :active-keymap :normal-keymap
   :selection-mode :normal
   :selection-anchor nil
   :last-search {:pattern nil :direction :forward :case-sensitive false}
   :repeat-last-command nil
   :macro-recording nil
   :macro-registry {}
   :command-history []
   :transition-hooks {:enter {} :exit {}}})

;; -- Evil-mode Initialization --

(defn initialize!
  "Initialize Evil-mode package. Registers commands and keymaps."
  []
  (println "🔥 Initializing Evil-mode...")

  ;; Initialize FSM state in app-db
  (rf/dispatch-sync [:evil/initialize-fsm-state])

  ;; Initialize default Evil keymaps
  (evil-keymaps/initialize-default-keymaps!)

  ;; Register evil-mode toggle command
  (rf/dispatch [:register-command :evil-mode
                {:docstring "Toggle Vim emulation (Evil mode)"
                 :handler [:toggle-evil-mode]}])

  (println "✓ Evil-mode initialized"))

(defn cleanup!
  "Cleanup Evil-mode package. Unregisters commands and keymaps."
  []
  (println "🔥 Cleaning up Evil-mode...")

  ;; Remove FSM state from app-db
  (rf/dispatch-sync [:evil/cleanup-fsm-state])

  ;; Clear all Evil keymaps
  (reset! evil-keymaps/keymap-registry {})

  ;; Clear command registry
  (reset! evil-dispatcher/command-registry {})

  (println "✓ Evil-mode cleaned up"))

;; -- FSM State Management Events --

(rf/reg-event-db
 :evil/initialize-fsm-state
 (fn [db [_]]
   "Add FSM state to app-db when Evil-mode is loaded"
   (assoc db :fsm default-fsm-state)))

(rf/reg-event-db
 :evil/cleanup-fsm-state
 (fn [db [_]]
   "Remove FSM state from app-db when Evil-mode is unloaded"
   (dissoc db :fsm)))

;; -- Evil-mode Toggle Command --

(rf/reg-event-fx
 :toggle-evil-mode
 (fn [{:keys [db]} [_]]
   "Toggle Evil-mode on/off"
   (let [evil-active? (get-in db [:packages :evil-mode :active?] false)]
     (if evil-active?
       ;; Deactivate Evil-mode
       {:db (assoc-in db [:packages :evil-mode :active?] false)
        :fx [[:dispatch [:echo/message "Evil-mode disabled"]]]}
       ;; Activate Evil-mode
       {:db (assoc-in db [:packages :evil-mode :active?] true)
        :fx [[:dispatch [:echo/message "Evil-mode enabled (stub - not fully implemented)"]]]}))))

;; -- Package Registration --

(packages/register-package!
 {:name :evil-mode
  :version "0.1.0"
  :description "Vim emulation layer for Lexicon (FSM-based modal editing)"
  :initialize initialize!
  :cleanup cleanup!})

(println "📦 Evil-mode package registered")
