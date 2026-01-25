(ns lexicon.core.packages.commands
  "Interactive commands for package management.

  Provides M-x commands:
  - load-package - Load package from directory
  - unload-package - Unload loaded package
  - reload-package - Reload package (for development)
  - list-packages - Show all loaded packages"
  (:require [lexicon.core.packages.loader :as loader]
            [re-frame.core :as rf]))

;; -- Command Registration --

(defn register-package-commands!
  "Register all package management commands.

  Called during editor initialization."
  []

  ;; load-package
  (rf/dispatch
    [:command/register
     :load-package
     {:interactive true
      :doc "Load a package from local directory"
      :handler (fn []
                 ;; TODO: Prompt for directory path via minibuffer
                 ;; For now, log that command was invoked
                 (rf/dispatch [:echo/message
                              "load-package: Not yet fully implemented. Use (loader/load-package-from-dir \"path\") from REPL."]))}])

  ;; unload-package
  (rf/dispatch
    [:command/register
     :unload-package
     {:interactive true
      :doc "Unload a currently loaded package"
      :handler (fn []
                 ;; TODO: Prompt for package name via completing-read
                 ;; For now, log that command was invoked
                 (rf/dispatch [:echo/message
                              "unload-package: Not yet fully implemented. Use (loader/unload-package :package-name) from REPL."]))}])

  ;; reload-package
  (rf/dispatch
    [:command/register
     :reload-package
     {:interactive true
      :doc "Reload a package (unload + load)"
      :handler (fn []
                 ;; TODO: Prompt for package path via minibuffer
                 (rf/dispatch [:echo/message
                              "reload-package: Not yet fully implemented. Use (loader/reload-package \"path\") from REPL."]))}])

  ;; list-packages
  (rf/dispatch
    [:command/register
     :list-packages
     {:interactive true
      :doc "Show all loaded packages"
      :handler (fn []
                 (let [loaded-packages @(rf/subscribe [:packages/list-loaded])]
                   (if (seq loaded-packages)
                     (rf/dispatch [:echo/message
                                  (str "Loaded packages: "
                                       (clojure.string/join ", " (map name loaded-packages)))])
                     (rf/dispatch [:echo/message "No packages loaded"]))))}])

  ;; package-info (future)
  (rf/dispatch
    [:command/register
     :describe-package
     {:interactive true
      :doc "Show information about a package"
      :handler (fn []
                 (rf/dispatch [:echo/message
                              "describe-package: Not yet implemented"]))}]))

(comment
  ;; Register commands during editor init
  (register-package-commands!)

  ;; Invoke commands
  (rf/dispatch [:load-package])
  (rf/dispatch [:list-packages]))
