(ns lexicon.packages.eglot
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

  Based on Emacs lisp/progmodes/eglot.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

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
;; State (Package-local)
;; =============================================================================

;; Eglot state: {:servers {buffer-id server-state}, :events [], :diagnostics {buffer-id []}}
(defonce eglot-state
  (atom {:servers {}
         :events []
         :diagnostics {}}))

;; =============================================================================
;; Events Log
;; =============================================================================

(defn- log-eglot-event!
  "Log an event to the eglot events buffer."
  [event-type message]
  (let [timestamp (.toISOString (js/Date.))
        new-event {:timestamp timestamp
                   :type event-type
                   :message message}]
    (swap! eglot-state update :events
           (fn [events]
             (let [events (or events [])]
               (if (> (count events) 100)
                 (conj (subvec events (- (count events) 99)) new-event)
                 (conj events new-event)))))))

;; =============================================================================
;; Eglot Core Functions
;; =============================================================================

(defn eglot-start!
  "Start LSP server for current buffer's major mode."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        major-mode (:mode buffer-info)
        server-config (get eglot-server-programs major-mode)]
    (if server-config
      (do
        (swap! eglot-state assoc-in [:servers buffer-id]
               {:status :connecting
                :mode major-mode
                :language-id (:language-id server-config)
                :command (:command server-config)
                :capabilities default-capabilities
                :started-at (.now js/Date)})
        (log-eglot-event! :lifecycle
                          (str "[eglot] Starting "
                               (first (:command server-config))
                               " for " (name major-mode)))
        (lisp/message (str "Eglot: connecting to "
                           (first (:command server-config))
                           " for " (name major-mode)
                           " (WebSocket bridge required)")))
      (lisp/message (str "Eglot: no server configured for "
                         (name (or major-mode :fundamental-mode)))))))

(defn eglot-shutdown!
  "Shutdown LSP server for current buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (if server
      (do
        (swap! eglot-state update :servers dissoc buffer-id)
        (log-eglot-event! :lifecycle "[eglot] Server shutdown")
        (lisp/message "Eglot: server shutdown"))
      (lisp/message "Eglot: no active server"))))

(defn eglot-shutdown-all!
  "Shutdown all active LSP servers."
  []
  (let [servers (get @eglot-state :servers {})
        cnt (count servers)]
    (swap! eglot-state assoc :servers {})
    (log-eglot-event! :lifecycle (str "[eglot] Shutdown all (" cnt " servers)"))
    (lisp/message (if (pos? cnt)
                    (str "Eglot: shut down " cnt " server"
                         (when (not= cnt 1) "s"))
                    "Eglot: no active servers"))))

(defn eglot-reconnect!
  "Reconnect to LSP server."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (when server
      (eglot-shutdown!))
    (eglot-start!)))

(defn eglot-ensure!
  "Ensure eglot is active for the current buffer's mode.
   Auto-starts if the major mode has a configured server."
  []
  (let [buffer-id (lisp/current-buffer)
        buffer-info (lisp/buffer-info buffer-id)
        major-mode (:mode buffer-info)
        server (get-in @eglot-state [:servers buffer-id])
        has-config? (get eglot-server-programs major-mode)]
    (cond
      (and has-config? (not server))
      (eglot-start!)

      server
      (lisp/message (str "Eglot: already active for " (name major-mode)))

      :else
      (lisp/message (str "Eglot: no server for "
                         (name (or major-mode :fundamental-mode)))))))

;; =============================================================================
;; Rename
;; =============================================================================

(defn- eglot-rename-exec!
  "Execute the rename with new-name."
  [new-name]
  (log-eglot-event! :request (str "[eglot] textDocument/rename -> " new-name))
  (lisp/message (str "Eglot: rename to '" new-name
                     "' (requires active LSP connection)")))

(defn eglot-rename!
  "Rename symbol at point via LSP."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (if server
      (lisp/read-from-minibuffer "Rename to: " eglot-rename-exec!)
      (lisp/message "Eglot: no active server"))))

;; =============================================================================
;; Code Actions
;; =============================================================================

(defn eglot-code-actions!
  "Show available code actions via LSP."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (if server
      (do
        (log-eglot-event! :request "[eglot] textDocument/codeAction")
        (lisp/message "Eglot: no code actions available (requires LSP connection)"))
      (lisp/message "Eglot: no active server"))))

;; =============================================================================
;; Format Buffer
;; =============================================================================

(defn eglot-format-buffer!
  "Format current buffer via LSP."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (if server
      (do
        (log-eglot-event! :request "[eglot] textDocument/formatting")
        (lisp/message "Eglot: format buffer (requires active LSP connection)"))
      (lisp/message "Eglot: no active server"))))

;; =============================================================================
;; Find Declaration / Implementation (delegates to xref)
;; =============================================================================

(defn eglot-find-declaration!
  "Find declaration of symbol at point (delegates to xref)."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (when server
      (log-eglot-event! :request "[eglot] textDocument/declaration"))
    ;; Always delegate to xref (it works without eglot too)
    (lisp/call-interactively 'xref-find-definitions)))

(defn eglot-find-implementation!
  "Find implementation of symbol at point (delegates to xref)."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (when server
      (log-eglot-event! :request "[eglot] textDocument/implementation"))
    ;; Always delegate to xref (it works without eglot too)
    (lisp/call-interactively 'xref-find-references)))

;; =============================================================================
;; Diagnostics Display
;; =============================================================================

(defn eglot-set-diagnostics!
  "Set diagnostics for a buffer."
  [buffer-id diagnostics]
  (swap! eglot-state assoc-in [:diagnostics buffer-id] diagnostics))

(defn eglot-get-diagnostics
  "Get diagnostics for a buffer."
  [buffer-id]
  (get-in @eglot-state [:diagnostics buffer-id] []))

(defn eglot-show-diagnostics!
  "Show diagnostics for current buffer in a dedicated buffer."
  []
  (let [buffer-id (lisp/current-buffer)
        diagnostics (eglot-get-diagnostics buffer-id)
        buffer-info (lisp/buffer-info buffer-id)
        buffer-name (or (:name buffer-info) "unknown")]
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
                         " total\n")]
        ;; Kill existing diagnostics buffer if any
        (when-let [existing-id (lisp/get-buffer "*eglot diagnostics*")]
          (lisp/kill-buffer existing-id))

        ;; Create new diagnostics buffer
        (let [new-buffer-id (lisp/create-special-buffer "*eglot diagnostics*" content
                                                        {:major-mode :special-mode
                                                         :read-only true})]
          (lisp/switch-to-buffer new-buffer-id)))
      (lisp/message "No diagnostics"))))

;; =============================================================================
;; Hover Documentation
;; =============================================================================

(defn eglot-hover!
  "Show hover documentation for symbol at point."
  []
  (let [buffer-id (lisp/current-buffer)
        server (get-in @eglot-state [:servers buffer-id])]
    (if server
      (do
        (log-eglot-event! :request "[eglot] textDocument/hover")
        (lisp/message "Eglot: hover documentation (requires active LSP connection)"))
      (lisp/message "Eglot: no active server"))))

;; =============================================================================
;; Document Symbols
;; =============================================================================

(defn- eglot-workspace-symbol-exec!
  "Execute workspace symbol search."
  [query]
  (log-eglot-event! :request (str "[eglot] workspace/symbol -> " query))
  (lisp/message (str "Eglot: workspace symbol '" query
                     "' (requires active LSP connection)")))

(defn eglot-workspace-symbol!
  "Search for workspace symbols."
  []
  (lisp/read-from-minibuffer "Workspace symbol: " eglot-workspace-symbol-exec!))

;; =============================================================================
;; Events Buffer
;; =============================================================================

(defn eglot-events-buffer!
  "Show *EGLOT Events* buffer with protocol log."
  []
  (let [events (get @eglot-state :events [])
        servers (get @eglot-state :servers {})
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
                     "Servers: " (count servers) " active\n"
                     "Configured modes: " (str/join ", " (map name (keys eglot-server-programs))) "\n")]
    ;; Kill existing events buffer if any
    (when-let [existing-id (lisp/get-buffer "*EGLOT Events*")]
      (lisp/kill-buffer existing-id))

    ;; Create new events buffer
    (let [new-buffer-id (lisp/create-special-buffer "*EGLOT Events*" content
                                                    {:major-mode :special-mode
                                                     :read-only true})]
      (lisp/switch-to-buffer new-buffer-id))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize eglot module and register commands."
  []
  (lisp/define-command 'eglot
    eglot-start!
    "Start LSP server for current buffer")

  (lisp/define-command 'eglot-shutdown
    eglot-shutdown!
    "Shutdown LSP server")

  (lisp/define-command 'eglot-shutdown-all
    eglot-shutdown-all!
    "Shutdown all LSP servers")

  (lisp/define-command 'eglot-reconnect
    eglot-reconnect!
    "Reconnect to LSP server")

  (lisp/define-command 'eglot-ensure
    eglot-ensure!
    "Ensure eglot is active for current buffer")

  (lisp/define-command 'eglot-rename
    eglot-rename!
    "Rename symbol at point via LSP")

  (lisp/define-command 'eglot-code-actions
    eglot-code-actions!
    "Show code actions via LSP")

  (lisp/define-command 'eglot-format-buffer
    eglot-format-buffer!
    "Format current buffer via LSP")

  (lisp/define-command 'eglot-show-diagnostics
    eglot-show-diagnostics!
    "Show LSP diagnostics for current buffer")

  (lisp/define-command 'eglot-hover
    eglot-hover!
    "Show hover documentation at point")

  (lisp/define-command 'eglot-workspace-symbol
    eglot-workspace-symbol!
    "Search for workspace symbols")

  (lisp/define-command 'eglot-find-declaration
    eglot-find-declaration!
    "Find declaration of symbol at point")

  (lisp/define-command 'eglot-find-implementation
    eglot-find-implementation!
    "Find implementation of symbol at point")

  (lisp/define-command 'eglot-events-buffer
    eglot-events-buffer!
    "Show *EGLOT Events* protocol log"))
