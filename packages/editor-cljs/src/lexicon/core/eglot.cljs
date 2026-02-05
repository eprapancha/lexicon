(ns lexicon.core.eglot
  "Eglot - Emacs LSP client (eglot.el).

  Implements Emacs eglot.el core framework:
  - LSP server program configuration per major mode
  - Connection lifecycle (initialize, shutdown, reconnect)
  - eglot-ensure: Auto-start eglot for configured modes
  - textDocument/hover documentation display
  - textDocument/publishDiagnostics handling
  - textDocument/definition navigation (delegates to xref)
  - textDocument/references (delegates to xref)
  - Workspace/document symbols
  - Code actions, rename, format-buffer
  - Events buffer (*EGLOT Events*) for protocol logging
  - Diagnostics buffer for viewing all diagnostics
  - Server capabilities tracking
  - eglot-shutdown-all for closing all servers

  Commands:
  - eglot: Start LSP server for current buffer
  - eglot-shutdown: Shutdown LSP server
  - eglot-shutdown-all: Shutdown all LSP servers
  - eglot-reconnect: Reconnect to LSP server
  - eglot-rename: Rename symbol at point
  - eglot-code-actions: Show code actions
  - eglot-format-buffer: Format buffer via LSP
  - eglot-show-diagnostics: Show diagnostics for buffer
  - eglot-hover: Show hover documentation at point
  - eglot-workspace-symbol: Search workspace symbols
  - eglot-find-declaration: Find declaration of symbol
  - eglot-find-implementation: Find implementation of symbol
  - eglot-events-buffer: Show protocol events log
  - eglot-ensure: Auto-start eglot when entering configured mode

  Note: In browser context, actual LSP communication requires
  a WebSocket bridge to a language server. This module provides
  the framework and command infrastructure.

  Based on Emacs lisp/progmodes/eglot.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

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
;; Server Capabilities
;; =============================================================================

(def default-capabilities
  "Default LSP server capabilities (simulated)."
  {:textDocumentSync {:openClose true :change 2}
   :completionProvider {:triggerCharacters ["." ":"]}
   :hoverProvider true
   :definitionProvider true
   :referencesProvider true
   :documentSymbolProvider true
   :workspaceSymbolProvider true
   :codeActionProvider true
   :documentFormattingProvider true
   :renameProvider true
   :diagnosticProvider {:interFileDependencies true}})

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

(rf/reg-sub
 :eglot/any-active?
 (fn [db _]
   (some (fn [[_id state]] (= (:status state) :connected))
         (get-in db [:eglot :servers] {}))))

;; =============================================================================
;; Events Log
;; =============================================================================

(rf/reg-event-db
 :eglot/log-event
 (fn [db [_ event-type message]]
   (let [events (get-in db [:eglot :events] [])
         timestamp (.toISOString (js/Date.))
         new-event {:timestamp timestamp
                    :type event-type
                    :message message}
         ;; Keep last 100 events
         new-events (if (> (count events) 100)
                      (conj (subvec events (- (count events) 99)) new-event)
                      (conj events new-event))]
     (assoc-in db [:eglot :events] new-events))))

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
       {:db (-> db
                (assoc-in [:eglot :servers buffer-id]
                          {:status :connecting
                           :mode major-mode
                           :language-id (:language-id server-config)
                           :command (:command server-config)
                           :capabilities default-capabilities
                           :started-at (.now js/Date)})
                (update-in [:eglot :events] (fnil conj [])
                           {:timestamp (.toISOString (js/Date.))
                            :type :lifecycle
                            :message (str "[eglot] Starting "
                                          (first (:command server-config))
                                          " for " (name major-mode))}))
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
       {:db (-> db
                (update-in [:eglot :servers] dissoc buffer-id)
                (update-in [:eglot :events] (fnil conj [])
                           {:timestamp (.toISOString (js/Date.))
                            :type :lifecycle
                            :message "[eglot] Server shutdown"}))
        :fx [[:dispatch [:echo/message "Eglot: server shutdown"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

(rf/reg-event-fx
 :eglot/shutdown-all
 (fn [{:keys [db]} [_]]
   "Shutdown all active LSP servers."
   (let [servers (get-in db [:eglot :servers] {})
         count (count servers)]
     {:db (-> db
              (assoc-in [:eglot :servers] {})
              (update-in [:eglot :events] (fnil conj [])
                         {:timestamp (.toISOString (js/Date.))
                          :type :lifecycle
                          :message (str "[eglot] Shutdown all (" count " servers)")}))
      :fx [[:dispatch [:echo/message
                       (if (pos? count)
                         (str "Eglot: shut down " count " server"
                              (when (not= count 1) "s"))
                         "Eglot: no active servers")]]]})))

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
 :eglot/ensure
 (fn [{:keys [db]} [_]]
   "Ensure eglot is active for the current buffer's mode.
    Auto-starts if the major mode has a configured server."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         buffer (get-in db [:buffers buffer-id])
         major-mode (:major-mode buffer)
         server (get-in db [:eglot :servers buffer-id])
         has-config? (get eglot-server-programs major-mode)]
     (if (and has-config? (not server))
       {:fx [[:dispatch [:eglot/start]]]}
       (if server
         {:fx [[:dispatch [:echo/message
                           (str "Eglot: already active for "
                                (name major-mode))]]]}
         {:fx [[:dispatch [:echo/message
                           (str "Eglot: no server for "
                                (name (or major-mode :fundamental-mode)))]]]})))))

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
   {:db (update-in db [:eglot :events] (fnil conj [])
                   {:timestamp (.toISOString (js/Date.))
                    :type :request
                    :message (str "[eglot] textDocument/rename -> " new-name)})
    :fx [[:dispatch [:echo/message
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
       {:db (update-in db [:eglot :events] (fnil conj [])
                       {:timestamp (.toISOString (js/Date.))
                        :type :request
                        :message "[eglot] textDocument/codeAction"})
        :fx [[:dispatch [:echo/message
                         "Eglot: no code actions available (requires LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

(rf/reg-event-fx
 :eglot/format-buffer
 (fn [{:keys [db]} [_]]
   "Format current buffer via LSP."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:db (update-in db [:eglot :events] (fnil conj [])
                       {:timestamp (.toISOString (js/Date.))
                        :type :request
                        :message "[eglot] textDocument/formatting"})
        :fx [[:dispatch [:echo/message
                         "Eglot: format buffer (requires active LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

;; =============================================================================
;; Find Declaration / Implementation (delegates to xref)
;; =============================================================================

(rf/reg-event-fx
 :eglot/find-declaration
 (fn [{:keys [db]} [_]]
   "Find declaration of symbol at point (delegates to xref)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:db (update-in db [:eglot :events] (fnil conj [])
                       {:timestamp (.toISOString (js/Date.))
                        :type :request
                        :message "[eglot] textDocument/declaration"})
        :fx [[:dispatch [:xref/find-definitions]]]}
       {:fx [[:dispatch [:xref/find-definitions]]]}))))

(rf/reg-event-fx
 :eglot/find-implementation
 (fn [{:keys [db]} [_]]
   "Find implementation of symbol at point (delegates to xref)."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         server (get-in db [:eglot :servers buffer-id])]
     (if server
       {:db (update-in db [:eglot :events] (fnil conj [])
                       {:timestamp (.toISOString (js/Date.))
                        :type :request
                        :message "[eglot] textDocument/implementation"})
        :fx [[:dispatch [:xref/find-references]]]}
       {:fx [[:dispatch [:xref/find-references]]]}))))

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
   "Show diagnostics for current buffer in a dedicated buffer."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         diagnostics (get-in db [:eglot :diagnostics buffer-id] [])
         buffer (get-in db [:buffers buffer-id])
         buffer-name (or (:name buffer) "unknown")]
     (if (seq diagnostics)
       (let [content (str "Diagnostics for: " buffer-name "\n"
                          (str/join "" (repeat 50 "=")) "\n\n"
                          (str/join "\n"
                                    (map-indexed
                                     (fn [i d]
                                       (str "  " (inc i) ". "
                                            (or (:severity d) "info") ": "
                                            (:message d)
                                            (when (:line d)
                                              (str " (line " (:line d) ")"))))
                                     diagnostics))
                          "\n\n"
                          (count diagnostics) " diagnostic"
                          (when (not= 1 (count diagnostics)) "s")
                          " total\n")
             buffers (:buffers db)
             new-buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
             lines (str/split content #"\n" -1)
             line-count (count lines)]
         (if wasm-instance
           {:db (assoc-in db [:buffers new-buffer-id]
                          {:id new-buffer-id
                           :name "*eglot diagnostics*"
                           :wasm-instance wasm-instance
                           :file-handle nil
                           :major-mode :special-mode
                           :is-read-only? true
                           :is-modified? false
                           :mark-position nil
                           :cursor-position {:line 0 :column 0}
                           :selection-range nil
                           :minor-modes #{}
                           :buffer-local-vars {}
                           :ast nil
                           :language :text
                           :diagnostics []
                           :undo-stack []
                           :undo-in-progress? false
                           :editor-version 0
                           :text-properties {}
                           :overlays {}
                           :next-overlay-id 1
                           :cache {:text content
                                   :line-count line-count}})
            :fx [[:dispatch [:switch-buffer new-buffer-id]]]}
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))
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
       {:db (update-in db [:eglot :events] (fnil conj [])
                       {:timestamp (.toISOString (js/Date.))
                        :type :request
                        :message "[eglot] textDocument/hover"})
        :fx [[:dispatch [:echo/message
                         "Eglot: hover documentation (requires active LSP connection)"]]]}
       {:fx [[:dispatch [:echo/message "Eglot: no active server"]]]}))))

;; =============================================================================
;; Document Symbols
;; =============================================================================

(rf/reg-event-fx
 :eglot/workspace-symbol
 (fn [_ [_]]
   "Search for workspace symbols."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Workspace symbol: "
                      :on-confirm [:eglot/workspace-symbol-exec]}]]]}))

(rf/reg-event-fx
 :eglot/workspace-symbol-exec
 (fn [{:keys [db]} [_ query]]
   {:db (update-in db [:eglot :events] (fnil conj [])
                   {:timestamp (.toISOString (js/Date.))
                    :type :request
                    :message (str "[eglot] workspace/symbol -> " query)})
    :fx [[:dispatch [:echo/message
                     (str "Eglot: workspace symbol '" query
                          "' (requires active LSP connection)")]]]}))

;; =============================================================================
;; Events Buffer
;; =============================================================================

(rf/reg-event-fx
 :eglot/events-buffer
 (fn [{:keys [db]} [_]]
   "Show *EGLOT Events* buffer with protocol log."
   (let [events (get-in db [:eglot :events] [])
         content (str "*EGLOT Events*\n"
                      (str/join "" (repeat 50 "=")) "\n\n"
                      (if (seq events)
                        (str/join "\n"
                                  (map (fn [e]
                                         (str "[" (:timestamp e) "] "
                                              (name (:type e)) ": "
                                              (:message e)))
                                       events))
                        "(no events recorded)")
                      "\n\n"
                      "Servers: " (count (get-in db [:eglot :servers] {})) " active\n"
                      "Configured modes: " (str/join ", " (map name (keys eglot-server-programs))) "\n")
         buffers (:buffers db)
         buffer-id (db/next-buffer-id buffers)
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
         lines (str/split content #"\n" -1)
         line-count (count lines)]
     (if wasm-instance
       {:db (assoc-in db [:buffers buffer-id]
                      {:id buffer-id
                       :name "*EGLOT Events*"
                       :wasm-instance wasm-instance
                       :file-handle nil
                       :major-mode :special-mode
                       :is-read-only? true
                       :is-modified? false
                       :mark-position nil
                       :cursor-position {:line 0 :column 0}
                       :selection-range nil
                       :minor-modes #{}
                       :buffer-local-vars {}
                       :ast nil
                       :language :text
                       :diagnostics []
                       :undo-stack []
                       :undo-in-progress? false
                       :editor-version 0
                       :text-properties {}
                       :overlays {}
                       :next-overlay-id 1
                       :cache {:text content
                               :line-count line-count}})
        :fx [[:dispatch [:switch-buffer buffer-id]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

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

  (rf/dispatch [:register-command :eglot-shutdown-all
                {:docstring "Shutdown all LSP servers"
                 :interactive nil
                 :handler [:eglot/shutdown-all]}])

  (rf/dispatch [:register-command :eglot-reconnect
                {:docstring "Reconnect to LSP server"
                 :interactive nil
                 :handler [:eglot/reconnect]}])

  (rf/dispatch [:register-command :eglot-ensure
                {:docstring "Ensure eglot is active for current buffer"
                 :interactive nil
                 :handler [:eglot/ensure]}])

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
                 :handler [:eglot/workspace-symbol]}])

  (rf/dispatch [:register-command :eglot-find-declaration
                {:docstring "Find declaration of symbol at point"
                 :interactive nil
                 :handler [:eglot/find-declaration]}])

  (rf/dispatch [:register-command :eglot-find-implementation
                {:docstring "Find implementation of symbol at point"
                 :interactive nil
                 :handler [:eglot/find-implementation]}])

  (rf/dispatch [:register-command :eglot-events-buffer
                {:docstring "Show *EGLOT Events* protocol log"
                 :interactive nil
                 :handler [:eglot/events-buffer]}]))
