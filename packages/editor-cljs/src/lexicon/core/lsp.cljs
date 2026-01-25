(ns lexicon.core.lsp
  (:require [re-frame.core :as rf]
            [clojure.core.async :as async :refer [<!]]
            [clojure.core.async.interop :refer-macros [<p!]]
            [clojure.string])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; -- LSP Message ID Management --

(defonce next-id (atom 0))

(defn next-message-id []
  "Generate next unique message ID for LSP requests"
  (swap! next-id inc))

;; -- LSP Message Constructors --

(defn make-lsp-request
  "Create a standard LSP JSON-RPC request"
  [method params]
  {:jsonrpc "2.0"
   :id      (next-message-id)
   :method  method
   :params  params})

(defn make-lsp-notification
  "Create a standard LSP JSON-RPC notification (no response expected)"
  [method params]
  {:jsonrpc "2.0"
   :method  method
   :params  params})

(defn file-uri-from-path
  "Convert file path to file:// URI"
  [path]
  (str "file://" path))

;; -- LSP Lifecycle Events --

(rf/reg-event-fx
 :lsp/initialize
 (fn [{:keys [db]} [_ language]]
   "Send LSP initialize request for a language"
   (let [initialize-params {:processId             nil
                            :clientInfo            {:name "Lexicon Editor" :version "1.0.0"}
                            :capabilities          {:textDocument 
                                                    {:synchronization    {:dynamicRegistration false
                                                                          :willSave            false
                                                                          :willSaveWaitUntil   false
                                                                          :didSave             false}
                                                     :completion         {:dynamicRegistration false
                                                                          :completionItem      {:snippetSupport false}}
                                                     :hover              {:dynamicRegistration false}
                                                     :signatureHelp      {:dynamicRegistration false}
                                                     :references         {:dynamicRegistration false}
                                                     :documentHighlight  {:dynamicRegistration false}
                                                     :documentSymbol     {:dynamicRegistration false}
                                                     :formatting         {:dynamicRegistration false}
                                                     :rangeFormatting    {:dynamicRegistration false}
                                                     :onTypeFormatting   {:dynamicRegistration false}
                                                     :definition         {:dynamicRegistration false}
                                                     :codeAction         {:dynamicRegistration false}
                                                     :codeLens           {:dynamicRegistration false}
                                                     :documentLink       {:dynamicRegistration false}
                                                     :rename             {:dynamicRegistration false}
                                                     :publishDiagnostics {:relatedInformation false}}}
                            :initializationOptions {}
                            :trace                 "off"
                            :workspaceFolders      nil}
         message           (make-lsp-request "initialize" initialize-params)]
     {:fx [[:ws/send {:message {:type "lsp/start" :language (name language)}}]
           [:dispatch-later {:ms 1000 :dispatch [:lsp/send-message {:language language :message message}]}]]})))

(rf/reg-event-fx
 :lsp/initialized
 (fn [{:keys [db]} [_ language]]
   "Send initialized notification after initialize response"
   (let [message (make-lsp-notification "initialized" {})]
     {:fx [[:dispatch [:lsp/send-message {:language language :message message}]]]})))

(rf/reg-event-fx
 :lsp/did-open
 (fn [{:keys [db]} [_ buffer-id]]
   "Send textDocument/didOpen notification when a file is opened"
   (println "📄 LSP: ===== DID-OPEN EVENT TRIGGERED ===== buffer-id:" buffer-id)
   (let [buffer        (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         language      (:language buffer)
         file-name     (:name buffer)]
     (println "📄 :client LSP didOpen details - wasm:" (boolean wasm-instance) "language:" language "file:" file-name)
     (if (and wasm-instance (not= language :text))
       (let [already-open? (get-in db [:buffers buffer-id :lsp-open?] false)]
         (if already-open?
           (do
             (println "📄 :client LSP file already open, skipping didOpen for" file-name)
             {:db db})
           (let [text        (.getText wasm-instance)
                 language-id (case language
                               :javascript "javascript"
                               :typescript "typescript"
                               :python     "python"
                               :rust       "rust"
                               :clojure    "clojure"
                               "text")
                 file-uri    (file-uri-from-path file-name)
                 close-params {:textDocument {:uri file-uri}}
                 close-message (make-lsp-notification "textDocument/didClose" close-params)
                 open-params  {:textDocument {:uri        file-uri
                                              :languageId language-id
                                              :version    1
                                              :text       text}}
                 open-message (make-lsp-notification "textDocument/didOpen" open-params)]
             (println "📤 :client LSP sending didClose+didOpen - file:" file-name "-> URI:" file-uri "length:" (count text))
             {:db (assoc-in db [:buffers buffer-id :lsp-open?] true)
              :fx [[:dispatch [:lsp/initialize language]]
                   [:dispatch-later {:ms 2000 :dispatch [:lsp/send-message {:language language :message close-message}]}]
                   [:dispatch-later {:ms 2100 :dispatch [:lsp/send-message {:language language :message open-message}]}]]})))
       (do
         (println "❌ :client LSP cannot send didOpen - wasm:" (boolean wasm-instance) "language:" language)
         {:db db})))))

(rf/reg-event-fx
 :lsp/did-change
 (fn [{:keys [db]} [_ buffer-id]]
   "Send textDocument/didChange notification when buffer content changes"
   (let [buffer        (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         language      (:language buffer)
         file-name     (:name buffer)]
     (when (and wasm-instance (not= language :text))
       (let [text    (.getText wasm-instance)
             params  {:textDocument   {:uri     (file-uri-from-path file-name)
                                       :version (inc (get-in db [:buffers buffer-id :lsp-version] 1))}
                      :contentChanges [{:text text}]} ; Full document sync for simplicity
             message (make-lsp-notification "textDocument/didChange" params)]
         {:db (assoc-in db [:buffers buffer-id :lsp-version] 
                        (inc (get-in db [:buffers buffer-id :lsp-version] 1)))
          :fx [[:dispatch [:lsp/send-message {:language language :message message}]]]})))))

(rf/reg-event-fx
 :lsp/did-save
 (fn [{:keys [db]} [_ buffer-id]]
   "Send textDocument/didSave notification when buffer is saved"
   (let [buffer    (get-in db [:buffers buffer-id])
         language  (:language buffer)
         file-name (:name buffer)]
     (when (not= language :text)
       (let [params  {:textDocument {:uri (file-uri-from-path file-name)}}
             message (make-lsp-notification "textDocument/didSave" params)]
         {:fx [[:dispatch [:lsp/send-message {:language language :message message}]]]})))))

(rf/reg-event-fx
 :lsp/did-close
 (fn [{:keys [db]} [_ buffer-id]]
   "Send textDocument/didClose notification when buffer is closed"
   (let [buffer        (get-in db [:buffers buffer-id])
         language      (:language buffer)
         file-name     (:name buffer)
         already-open? (get-in db [:buffers buffer-id :lsp-open?] false)]
     (when (and already-open? (not= language :text))
       (let [params  {:textDocument {:uri (file-uri-from-path file-name)}}
             message (make-lsp-notification "textDocument/didClose" params)]
         (println "📤 LSP: Sending didClose for" file-name)
         {:db (assoc-in db [:buffers buffer-id :lsp-open?] false)
          :fx [[:dispatch [:lsp/send-message {:language language :message message}]]]})))))

;; -- LSP Effect Handlers --

(rf/reg-event-fx
 :lsp/send-message
 (fn [_ [_ {:keys [language message]}]]
   "Send LSP message through WebSocket bridge"
   (println "🌉 :client LSP send-message - language:" language "method:" (:method message))
   {:fx [[:ws/send {:message {:type     "lsp/message"
                              :language (name language)
                              :data     message}}]]}))

;; -- LSP Response Handlers --

(rf/reg-event-fx
 :lsp/server-started
 (fn [{:keys [db]} [_ {:keys [language status]}]]
   "Handle LSP server started notification"
   (println "🚀 :client LSP server started for" language "- status:" status)
   (if (= status "success")
     {:fx [[:dispatch [:lsp/initialize language]]]}
     {:db db})))

(rf/reg-event-db
 :lsp/server-stopped
 (fn [db [_ {:keys [language code]}]]
   "Handle LSP server stopped notification"
   (println "🛑 :client LSP server stopped for" language "- exit code:" code)
   db))

(rf/reg-event-fx
 :lsp/message-received
 (fn [{:keys [db]} [_ {:keys [language data]}]]
   "Handle incoming LSP message from server"
   (let [method (:method data)
         params (:params data)]
     (println "📨 :client LSP received message method:" method "from" language)
     (case method
       "textDocument/publishDiagnostics"
       (do
         (println "📋 :client LSP received diagnostics for:" (:uri params))
         {:fx [[:dispatch [:diagnostics/update 
                          {:uri         (:uri params)
                           :diagnostics (:diagnostics params)}]]]})
       
       "window/logMessage"
       (do
         (println "📝 :client LSP log message:" (:message params))
         {:db db})
       
       "window/showMessage"
       (do
         (println "💬 :client LSP show message:" (:message params))
         {:db db})
       
       ;; Response to initialize request or other responses
       (if (and (:result data) (:id data))
         (do
           (println "✅ :client LSP initialize response received")
           {:fx [[:dispatch [:lsp/initialized language]]]})
         
         ;; Default case - log unknown methods
         (do
           (println "📨 :client LSP unknown message method:" method "from" language)
           {:db db}))))))

;; -- Diagnostics Management --

(rf/reg-event-db
 :diagnostics/update
 (fn [db [_ {:keys [uri diagnostics]}]]
   "Update diagnostics for a specific file URI"
   (let [;; Find buffer by matching file name with URI
         buffer-id (->> (:buffers db)
                        (filter (fn [[id buffer]]
                                  (let [file-name (:name buffer)
                                        expected-uri (file-uri-from-path file-name)
                                        ;; Handle trailing slash by normalizing both URIs
                                        normalize-uri (fn [u] (clojure.string/replace u #"/$" ""))
                                        normalized-received (normalize-uri uri)
                                        normalized-expected (normalize-uri expected-uri)
                                        matches? (= normalized-received normalized-expected)]
                                    (println "🔍 Comparing URI:" uri "with expected:" expected-uri)
                                    (println "🔍 Normalized:" normalized-received "vs" normalized-expected "matches:" matches?)
                                    matches?)))
                        first
                        first)]
     (if buffer-id
       (do
         (println "📋 :client Updating diagnostics for buffer" buffer-id ":" (count diagnostics) "items")
         (assoc-in db [:buffers buffer-id :diagnostics] diagnostics))
       (do
         (println "⚠️ :client No buffer found for URI:" uri)
         db)))))

;; -- Debounced Change Notifications --

(defonce change-timeouts (atom {}))

(rf/reg-event-fx
 :lsp/did-change-debounced
 (fn [{:keys [db]} [_ buffer-id delay]]
   "Send did-change notification after a delay (debounced)"
   (let [timeout-key (str "change-" buffer-id)]
     ;; Clear existing timeout for this buffer
     (when-let [existing-timeout (get @change-timeouts timeout-key)]
       (js/clearTimeout existing-timeout))
     
     ;; Set new timeout
     (let [timeout (js/setTimeout 
                    #(rf/dispatch [:lsp/did-change buffer-id])
                    (or delay 500))]
       (swap! change-timeouts assoc timeout-key timeout))
     
     {:db db})))

;; -- Integration with Buffer Lifecycle --

(rf/reg-event-fx
 :lsp/on-buffer-opened
 (fn [{:keys [db]} [_ buffer-id]]
   "Handle buffer opened for LSP integration"
   (let [buffer   (get-in db [:buffers buffer-id])
         language (:language buffer)]
     (println "🔍 :client LSP buffer opened - ID:" buffer-id "Language:" language "Bridge status:" (get-in db [:bridge :status]))
     (if (not= language :text)
       (do
         (println "🚀 :client LSP starting for language:" language)
         {:fx [[:dispatch [:lsp/did-open buffer-id]]]})
       (do
         (println "⚠️ :client LSP skipping text files")
         {:db db})))))

(rf/reg-event-fx
 :lsp/on-buffer-changed
 (fn [{:keys [db]} [_ buffer-id]]
   "Handle buffer content changed for LSP integration"
   (let [buffer   (get-in db [:buffers buffer-id])
         language (:language buffer)]
     (when (not= language :text)
       {:fx [[:dispatch [:lsp/did-change-debounced buffer-id 750]]]}))))

(rf/reg-event-fx
 :lsp/on-buffer-saved
 (fn [{:keys [db]} [_ buffer-id]]
   "Handle buffer saved for LSP integration"
   (let [buffer   (get-in db [:buffers buffer-id])
         language (:language buffer)]
     (if (not= language :text)
       {:fx [[:dispatch [:lsp/did-save buffer-id]]]}
       {:db db}))))
