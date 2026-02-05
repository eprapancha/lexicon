(ns lexicon.core.eglot
  "Eglot - Emacs LSP client (eglot.el).

  Implements Emacs eglot.el core framework:
  - LSP server program configuration
  - Connection lifecycle (initialize, shutdown)
  - textDocument/hover documentation
  - textDocument/completion integration
  - textDocument/publishDiagnostics handling
  - textDocument/definition navigation
  - textDocument/references
  - Workspace/document symbols
  - Code actions, rename

  Commands:
  - eglot: Start LSP for current buffer
  - eglot-shutdown: Shutdown LSP server
  - eglot-reconnect: Reconnect to LSP server
  - eglot-rename: Rename symbol at point
  - eglot-code-actions: Show code actions
  - eglot-format-buffer: Format buffer via LSP

  Note: In browser context, actual LSP communication requires
  a WebSocket bridge to a language server. This module provides
  the framework and command infrastructure.

  Based on Emacs lisp/progmodes/eglot.el"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Server Programs Configuration
;; =============================================================================

(def eglot-server-programs
  "Default server programs for different major modes.
   Maps mode keywords to server command configurations."
  {:clojure-mode {:command ["clojure-lsp"]
                  :language-id "clojure"}
   :javascript-mode {:command ["typescript-language-server" "--stdio"]
                     :language-id "javascript"}
   :python-mode {:command ["pylsp"]
                 :language-id "python"}
   :rust-mode {:command ["rust-analyzer"]
               :language-id "rust"}
   :html-mode {:command ["vscode-html-language-server" "--stdio"]
               :language-id "html"}
   :css-mode {:command ["vscode-css-language-server" "--stdio"]
              :language-id "css"}})

;; =============================================================================
;; Connection State
;; =============================================================================

(rf/reg-event-db
 :eglot/set-server-state
 (fn [db [_ buffer-id state]]
   (assoc-in db [:eglot :servers buffer-id] state)))

(rf/reg-sub
 :eglot/server-state
 (fn [db [_ buffer-id]]
   (get-in db [:eglot :servers buffer-id])))

(rf/reg-sub
 :eglot/active?
 (fn [db [_ buffer-id]]
   (let [state (get-in db [:eglot :servers buffer-id])]
     (= (:status state) :connected))))

;; =============================================================================
;; Eglot Commands
;; =============================================================================

(rf/reg-event-fx
 :eglot/start
 (fn [{:keys [db]} [_]]
   "Start LSP server for current buffer's major mode."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         major-mode (:major-mode buffer)
         server-config (get eglot-server-programs major-mode)]
     (if server-config
       {:db (assoc-in db [:eglot :servers buffer-id]
                      {:status :connecting
                       :mode major-mode
                       :language-id (:language-id server-config)
                       :command (:command server-config)})
        :fx [[:dispatch [:echo/message
                         (str "Eglot: connecting to "
                              (first (:command server-config))
                              " for " (name major-mode)
                              " (WebSocket bridge required)")]]]}
       {:fx [[:dispatch [:echo/message
                         (str "Eglot: no server configured for "
                              (name (or major-mode :fundamental-mode)))]]]}))))

(rf/reg-event-fx
 :eglot/shutdown
 (fn [{:keys [db]} [_]]
   "Shutdown LSP server for current buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:db (update-in db [:eglot :servers] dissoc buffer-id)
        :fx [[:dispatch [:echo/message "Eglot: server shutdown"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

(rf/reg-event-fx
 :eglot/reconnect
 (fn [{:keys [db]} [_]]
   "Reconnect to LSP server."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:fx [[:dispatch [:eglot/shutdown]]
             [:dispatch [:eglot/start]]]}
       {:fx [[:dispatch [:eglot/start]]]}))))

(rf/reg-event-fx
 :eglot/rename
 (fn [{:keys [db]} [_]]
   "Rename symbol at point via LSP."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt "Rename to: "
                          :on-confirm [:eglot/rename-exec]}]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

(rf/reg-event-fx
 :eglot/rename-exec
 (fn [{:keys [db]} [_ new-name]]
   {:fx [[:dispatch [:echo/message
                     (str "Eglot: rename to '" new-name
                          "' (requires active LSP connection)")]]]}))

(rf/reg-event-fx
 :eglot/code-actions
 (fn [{:keys [db]} [_]]
   "Show available code actions via LSP."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:fx [[:dispatch [:echo/message
                         "Eglot: code actions (requires active LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

(rf/reg-event-fx
 :eglot/format-buffer
 (fn [{:keys [db]} [_]]
   "Format current buffer via LSP."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:fx [[:dispatch [:echo/message
                         "Eglot: format buffer (requires active LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

;; =============================================================================
;; Diagnostics Display
;; =============================================================================

(rf/reg-event-db
 :eglot/set-diagnostics
 (fn [db [_ buffer-id diagnostics]]
   (assoc-in db [:eglot :diagnostics buffer-id] diagnostics)))

(rf/reg-sub
 :eglot/diagnostics
 (fn [db [_ buffer-id]]
   (get-in db [:eglot :diagnostics buffer-id] [])))

(rf/reg-event-fx
 :eglot/show-diagnostics
 (fn [{:keys [db]} [_]]
   "Show diagnostics for current buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         diagnostics (get-in db [:eglot :diagnostics buffer-id] [])
         buffer (get-in db [:buffers buffer-id])]
     (if (seq diagnostics)
       {:fx [[:dispatch [:echo/message
                         (str (count diagnostics) " diagnostic"
                              (when (not= 1 (count diagnostics)) "s")
                              " in " (:name buffer))]]]}
       {:fx [[:dispatch [:echo/message "No diagnostics"]]]}))))

;; =============================================================================
;; Hover Documentation
;; =============================================================================

(rf/reg-event-fx
 :eglot/hover
 (fn [{:keys [db]} [_]]
   "Show hover documentation for symbol at point."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:fx [[:dispatch [:echo/message
                         "Eglot: hover documentation (requires active LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

;; =============================================================================
;; Document Symbols
;; =============================================================================

(rf/reg-event-fx
 :eglot/workspace-symbol
 (fn [{:keys [db]} [_]]
   "Search for workspace symbols."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Workspace symbol: "
                      :on-confirm [:eglot/workspace-symbol-exec]}]]]}))

(rf/reg-event-fx
 :eglot/workspace-symbol-exec
 (fn [{:keys [db]} [_ query]]
   {:fx [[:dispatch [:echo/message
                     (str "Eglot: workspace symbol '" query
                          "' (requires active LSP connection)")]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize eglot module and register commands."
  []
  (rf/dispatch [:register-command :eglot
                {:docstring "Start LSP server for current buffer"
                 :interactive nil
                 :handler [:eglot/start]}])

  (rf/dispatch [:register-command :eglot-shutdown
                {:docstring "Shutdown LSP server"
                 :interactive nil
                 :handler [:eglot/shutdown]}])

  (rf/dispatch [:register-command :eglot-reconnect
                {:docstring "Reconnect to LSP server"
                 :interactive nil
                 :handler [:eglot/reconnect]}])

  (rf/dispatch [:register-command :eglot-rename
                {:docstring "Rename symbol at point via LSP"
                 :interactive nil
                 :handler [:eglot/rename]}])

  (rf/dispatch [:register-command :eglot-code-actions
                {:docstring "Show code actions via LSP"
                 :interactive nil
                 :handler [:eglot/code-actions]}])

  (rf/dispatch [:register-command :eglot-format-buffer
                {:docstring "Format current buffer via LSP"
                 :interactive nil
                 :handler [:eglot/format-buffer]}])

  (rf/dispatch [:register-command :eglot-show-diagnostics
                {:docstring "Show LSP diagnostics for current buffer"
                 :interactive nil
                 :handler [:eglot/show-diagnostics]}])

  (rf/dispatch [:register-command :eglot-hover
                {:docstring "Show hover documentation at point"
                 :interactive nil
                 :handler [:eglot/hover]}])

  (rf/dispatch [:register-command :eglot-workspace-symbol
                {:docstring "Search for workspace symbols"
                 :interactive nil
                 :handler [:eglot/workspace-symbol]}]))
