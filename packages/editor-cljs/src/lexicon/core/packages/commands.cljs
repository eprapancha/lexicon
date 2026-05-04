(ns lexicon.core.packages.commands
  "Interactive commands for package management.

  Provides M-x commands:
  - load-package - Load package from directory
  - unload-package - Unload loaded package
  - reload-package - Reload package (for development)
  - list-packages - Show all loaded packages
  - describe-package - Show info about a package"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- Event Handlers --

(rf/reg-event-fx
  :packages/load-interactive
  (fn [_ _]
    {:fx [[:dispatch [:echo/message
                      "load-package: Not yet fully implemented. Use (loader/load-package-from-dir \"path\") from REPL."]]]}))

(rf/reg-event-fx
  :packages/unload-interactive
  (fn [_ _]
    {:fx [[:dispatch [:echo/message
                      "unload-package: Not yet fully implemented. Use (loader/unload-package :package-name) from REPL."]]]}))

(rf/reg-event-fx
  :packages/reload-interactive
  (fn [_ _]
    {:fx [[:dispatch [:echo/message
                      "reload-package: Not yet fully implemented. Use (loader/reload-package \"path\") from REPL."]]]}))

(rf/reg-event-fx
  :packages/list-interactive
  (fn [_ _]
    (let [loaded-packages @(rf/subscribe [:packages/list-loaded])]
      (if (seq loaded-packages)
        {:fx [[:dispatch [:echo/message
                          (str "Loaded packages: "
                               (str/join ", " (map name loaded-packages)))]]]}
        {:fx [[:dispatch [:echo/message "No packages loaded"]]]}))))

(rf/reg-event-fx
  :packages/describe-interactive
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "describe-package: Not yet implemented"]]]}))

;; -- Command Registration --

(defn register-package-commands!
  "Register all package management commands."
  []
  (rf/dispatch
    [:register-command :load-package
     {:interactive true
      :doc "Load a package from local directory"
      :handler [:packages/load-interactive]}])

  (rf/dispatch
    [:register-command :unload-package
     {:interactive true
      :doc "Unload a currently loaded package"
      :handler [:packages/unload-interactive]}])

  (rf/dispatch
    [:register-command :reload-package
     {:interactive true
      :doc "Reload a package (unload + load)"
      :handler [:packages/reload-interactive]}])

  (rf/dispatch
    [:register-command :list-packages
     {:interactive true
      :doc "Show all loaded packages"
      :handler [:packages/list-interactive]}])

  (rf/dispatch
    [:register-command :describe-package
     {:interactive true
      :doc "Show information about a package"
      :handler [:packages/describe-interactive]}]))

;; Auto-register commands on namespace load
(register-package-commands!)
