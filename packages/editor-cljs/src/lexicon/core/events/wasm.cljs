(ns lexicon.core.events.wasm
  "Event handlers for WASM module, transactions, and parser.

  Handles:
  - WASM module loading and initialization
  - Transaction queue management
  - Transaction processing (insert, delete, replace)
  - Undo/redo support
  - Parser worker management
  - AST updates and incremental parsing"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.core.db :as db]
            [lexicon.core.cache :as cache]
            [lexicon.core.constants :as const]
            [lexicon.core.advanced-undo :as undo]
            [lexicon.core.log :as log]
            [lexicon.core.api.message :refer [message]]
            [lexicon.core.dynamic :as dyn]))

;; -- Helper Functions --

(defn linear-pos-to-line-col
  "Convert linear position to line/column coordinates"
  [text linear-pos]
  (if (empty? text)
    {:line 0 :column 0}
    (let [lines (clojure.string/split text #"\n" -1)  ; -1 keeps trailing empty strings
          lines-with-lengths (map count lines)]
      (loop [pos 0
             line 0
             remaining-lengths lines-with-lengths]
        (if (empty? remaining-lengths)
          {:line (max 0 (dec line)) :column 0}
          (let [line-len (first remaining-lengths)
                line-end-pos (+ pos line-len)]
            (if (<= linear-pos line-end-pos)
              {:line line :column (- linear-pos pos)}
              (recur (+ line-end-pos 1) ; +1 for newline
                     (inc line)
                     (rest remaining-lengths)))))))))

(defn record-undo!
  "Record undo entry for an operation using advanced undo system.
  Returns undo entry or nil if recording disabled."
  [wasm-instance active-buffer-id current-cursor operation]
  (let [db @re-frame.db/app-db
        undo-in-progress? (get-in db [:buffers active-buffer-id :undo-in-progress?])
        recording-enabled? (get-in db [:undo :recording-enabled?] true)]
    (when (and recording-enabled? (not undo-in-progress?))
      (let [undo-entry (case (:op operation)
                         :insert
                         (let [text (:text operation)]
                           {:op :delete-range
                            :start current-cursor
                            :length (count text)
                            :text text})  ; Store the inserted text so invert can re-insert it

                         :delete-backward
                         (when (> current-cursor 0)
                           (let [deleted-char (.getRange ^js wasm-instance (dec current-cursor) current-cursor)]
                             {:op :insert
                              :text deleted-char
                              :position (dec current-cursor)}))

                         :delete-forward
                         (let [buffer-length (.length ^js wasm-instance)]
                           (when (< current-cursor buffer-length)
                             (let [deleted-char (.getRange ^js wasm-instance current-cursor (inc current-cursor))]
                               {:op :insert
                                :text deleted-char
                                :position current-cursor})))

                         :delete-range
                         (let [start (:start operation)
                               length (:length operation)
                               deleted-text (.getRange ^js wasm-instance start (+ start length))]
                           {:op :insert
                            :text deleted-text
                            :position start})

                         :replace
                         (let [start (:start operation)
                               length (:length operation)
                               old-text (.getRange ^js wasm-instance start (+ start length))
                               new-text (:text operation)]
                           {:op :replace
                            :start start
                            :length (count new-text)
                            :text old-text})

                         ;; Unknown operation - don't record
                         nil)]
        (when undo-entry
          ;; Push to advanced undo stack
          (undo/push-undo-entry! active-buffer-id {:type :edit :op (:op undo-entry) :position (:position undo-entry) :start (:start undo-entry) :length (:length undo-entry) :text (:text undo-entry)})

          ;; Record marker position change
          (let [old-pos current-cursor
                new-pos (case (:op operation)
                          :insert (+ current-cursor (count (:text operation)))
                          :delete-backward (dec current-cursor)
                          :delete-forward current-cursor
                          :delete-range (:start operation)
                          :replace (+ (:start operation) (count (:text operation)))
                          current-cursor)]
            (when (not= old-pos new-pos)
              (undo/push-undo-entry! active-buffer-id {:type :marker :marker-id :point :old-pos old-pos :new-pos new-pos})))

          ;; Clear redo stack on new edit
          (undo/clear-redo-stack! active-buffer-id)

          (println "📝 Advanced undo recorded:" undo-entry))
        undo-entry))))

;; -- WASM Module Loading --

(rf/reg-event-fx
 :wasm-module-loaded
 (fn [{:keys [db]} [_ {:keys [^js instance constructor]}]]
   "Store the loaded WASM module instance and constructor in the app state"
   ;; Log WASM module loading (before Messages buffer exists - tests log bus!)
   (log/info "WASM module loaded successfully")

   (let [initial-text (.getText ^js instance)
         initial-line-count (count (clojure.string/split initial-text #"\n" -1))
         ;; Create *Messages* buffer (Issue #47)
         messages-initial-text ";; *Messages* buffer - message history\n\n"
         messages-instance (new constructor messages-initial-text)
         messages-line-count (count (clojure.string/split messages-initial-text #"\n" -1))]
     (let [;; Detect test mode by checking port (8021 = test, 8080 = dev)
           test-mode? (= "8021" (.-port (.-location js/window)))
           ;; Skip WebSocket and parser worker in test mode
           production-fx (if test-mode?
                           []
                           [[:parser/start-worker {:worker-path "/parser-worker.js"}]
                            [:dispatch [:ws/connect]]])]
       {:db (-> db
                (assoc :initialized? true)
                (assoc-in [:system :wasm-constructor] constructor)
                (assoc-in [:buffers 1 :wasm-instance] instance)
                ;; Initialize cache with initial text and line count
                (assoc-in [:buffers 1 :cache :text] initial-text)
                (assoc-in [:buffers 1 :cache :line-count] initial-line-count)
                ;; Create *Messages* buffer (Issue #47)
                (assoc-in [:buffers 2 :id] 2)
                (assoc-in [:buffers 2 :wasm-instance] messages-instance)
                (assoc-in [:buffers 2 :name] "*Messages*")
                (assoc-in [:buffers 2 :is-modified?] false)
                (assoc-in [:buffers 2 :is-read-only?] true)
                (assoc-in [:buffers 2 :major-mode] :fundamental-mode)
                (assoc-in [:buffers 2 :minor-modes] #{})
                (assoc-in [:buffers 2 :buffer-local-vars] {})
                (assoc-in [:buffers 2 :file-handle] nil)
                (assoc-in [:buffers 2 :ast] nil)
                (assoc-in [:buffers 2 :language] :text)
                (assoc-in [:buffers 2 :diagnostics] [])
                (assoc-in [:buffers 2 :undo-stack] [])
                (assoc-in [:buffers 2 :undo-in-progress?] false)
                (assoc-in [:buffers 2 :editor-version] 0)
                (assoc-in [:buffers 2 :cache :text] messages-initial-text)
                (assoc-in [:buffers 2 :cache :line-count] messages-line-count)
                (assoc-in [:buffers 2 :text-properties] {})
                (assoc-in [:buffers 2 :overlays] {})
                (assoc-in [:buffers 2 :next-overlay-id] 1))
        :fx (concat [[:dispatch [:initialize-buffer-cursor 1]]]
                    production-fx
                    [;; Attach Messages buffer to log bus (Issue #73)
                     [:log/attach-messages-buffer nil]
                     ;; Log after Messages buffer attached (should appear in buffer!)
                     [:dispatch [:log-startup-complete]]])}))))

(rf/reg-event-fx
 :log-startup-complete
 (fn [_ [_]]
   "Log startup completion"
   ;; Can't use log/info or message from within event handler (dispatch-sync forbidden)
   (js/console.log "Editor initialized - buffers ready")
   (js/console.log "Log bus attached to *Messages* buffer")
   {}))

(rf/reg-event-db
 :wasm-load-failed
 (fn [db [_ error]]
   "Handle WASM loading failure"
   (println "WASM load failed, showing error to user:" error)
   (assoc-in db [:system :wasm-error] (str error))))

;; -- Serialized Transaction Queue System --

(rf/reg-event-db
 :editor/set-flight-status
 (fn [db [_ status]]
   "Set transaction in-flight status"
   (assoc db :transaction-in-flight? status)))

(rf/reg-event-fx
 :editor/queue-transaction
 (fn [{:keys [db]} [_ operation]]
   "Add transaction to queue and trigger processing"
   (println "🔄 Queueing operation:" operation)
   (let [updated-db (update db :transaction-queue conj operation)
         queue-size (count (:transaction-queue updated-db))]
     (println "📋 Queue size after adding:" queue-size)
     {:db updated-db
      :fx [[:editor/process-queue updated-db]]})))

(rf/reg-event-fx
 :handle-text-input
 (fn [{:keys [db]} [_ {:keys [input-type data dom-cursor-pos]}]]
   "Handle text input events by queueing operations"
   (let [prefix-arg (get-in db [:ui :prefix-argument])
         prefix-active? (get-in db [:ui :prefix-argument-active?])
         repeat-count (if (and prefix-active? prefix-arg) prefix-arg 1)]
     (println "🔢 handle-text-input - prefix-arg:" prefix-arg "active?:" prefix-active? "repeat:" repeat-count "data:" data)
     (let [operation (case input-type
                       "insertText"
                       (when data
                         ;; Repeat the text based on prefix-argument
                         (let [text (apply str (repeat repeat-count data))]
                           (println "📝 Inserting text:" text "(repeat" repeat-count "times)")
                           {:op :insert :text text}))

                       "insertCompositionText"
                       (when data
                         ;; Handle IME composition
                         (rf/dispatch [:ime-composition-update data])
                         nil)

                       "deleteContentBackward"
                       {:op :delete-backward}

                       "deleteContentForward"
                       {:op :delete-forward}

                       "insertFromPaste"
                       (when data
                         {:op :insert :text data})

                       ;; Log unhandled input types
                       (do (println "Unhandled input type:" input-type) nil))]

       (if operation
         {:fx [[:dispatch [:editor/queue-transaction operation]]
               ;; Clear prefix argument after using it for text insertion
               (when (and prefix-active? (= input-type "insertText"))
                 [:dispatch [:clear-prefix-argument]])]}
         {:db db})))))

;; Core queue processor effect - ensures serialized execution
(rf/reg-fx
 :editor/process-queue
 (fn [db-state]
   "Process the next transaction in the queue if not already processing"
   (println "🚀 Queue processor called")
   (let [db (or db-state @re-frame.db/app-db)
         in-flight? (:transaction-in-flight? db)
         queue (:transaction-queue db)
         queue-size (count queue)]
     (println "📊 Queue status - In flight:" in-flight? "Queue size:" queue-size)
     (when (and (not in-flight?) (seq queue))
       (let [operation (first (:transaction-queue db))
             active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
             active-buffer-id (:buffer-id active-window)
             active-buffer (get (:buffers db) active-buffer-id)
             wasm-instance (:wasm-instance active-buffer)
             current-cursor (get-in db [:ui :cursor-position] 0)
             is-read-only? (:is-read-only? active-buffer false)]

         ;; Phase 6B Week 3: Read-only buffer enforcement
         ;; Check if buffer is read-only AND inhibit-read-only is not set
         (if (and is-read-only? (not (dyn/inhibit-read-only?)))
           (do
             (println "🚫 Buffer is read-only, rejecting operation:" (:op operation))
             ;; Use message function to both show in echo area AND log to *Messages*
             (message "Buffer is read-only")
             ;; Remove transaction from queue without processing
             (swap! re-frame.db/app-db update :transaction-queue rest)
             ;; Continue processing queue in case there are other operations
             (rf/dispatch [:editor/process-queue]))
           (if wasm-instance
           (do
             (println "🔧 Processing operation:" (:op operation) "with WASM instance")
             ;; Immediately set the in-flight flag in the database to prevent concurrent operations
             (swap! re-frame.db/app-db assoc :transaction-in-flight? true)

             ;; Record undo information BEFORE executing operation
             (record-undo! wasm-instance active-buffer-id current-cursor operation)

             ;; Process the operation based on its type (using Gap Buffer API)
             (case (:op operation)
             :insert
             (let [text (:text operation)
                   ;; Use explicit position if provided (e.g., from yank), otherwise current cursor
                   insert-pos (or (:position operation) current-cursor)]
               (println "🔧 INSERT - text:" (pr-str text) "position:" insert-pos)
               (try
                 ;; Use gap buffer's direct insert API
                 (.insert ^js wasm-instance insert-pos text)
                 (rf/dispatch [:editor/transaction-success
                              {:operation operation
                               :new-cursor (+ insert-pos (count text))}])
                 (catch js/Error error
                   (println "❌ Insert error:" error)
                   (rf/dispatch [:editor/transaction-failure
                                {:error (str error)
                                 :operation operation}]))))

             :delete-backward
             (if (> current-cursor 0)
               (do
                 (println "🔧 DELETE BACKWARD at position:" (dec current-cursor))
                 (try
                   ;; Use gap buffer's direct delete API
                   (.delete ^js wasm-instance (dec current-cursor) 1)
                   (rf/dispatch [:editor/transaction-success
                                {:operation operation
                                 :new-cursor (dec current-cursor)}])
                   (catch js/Error error
                     (println "❌ Delete backward error:" error)
                     (rf/dispatch [:editor/transaction-failure
                                  {:error (str error)
                                   :operation operation}]))))
               ;; At beginning of buffer - no-op but still signal success
               (do
                 (println "⚠️ DELETE BACKWARD at position 0 (no-op)")
                 (rf/dispatch [:editor/transaction-success
                              {:operation operation
                               :new-cursor current-cursor}])))

             :delete-forward
             (do
               (println "🔧 DELETE FORWARD at position:" current-cursor)
               (try
                 ;; Use gap buffer's direct delete API
                 (.delete ^js wasm-instance current-cursor 1)
                 (rf/dispatch [:editor/transaction-success
                              {:operation operation
                               :new-cursor current-cursor}])  ; Cursor stays same after delete forward
                 (catch js/Error error
                   (println "❌ Delete forward error:" error)
                   (rf/dispatch [:editor/transaction-failure
                                {:error (str error)
                                 :operation operation}]))))

             :delete-range
             (let [start (:start operation)
                   length (:length operation)]
               (println "🔧 DELETE RANGE at position:" start "length:" length)
               (try
                 ;; Use gap buffer's direct delete API
                 (.delete ^js wasm-instance start length)
                 (rf/dispatch [:editor/transaction-success
                              {:operation operation
                               :new-cursor start}])  ; Cursor moves to start of deleted range
                 (catch js/Error error
                   (println "❌ Delete range error:" error)
                   (rf/dispatch [:editor/transaction-failure
                                {:error (str error)
                                 :operation operation}]))))

             :replace
             (let [start (:start operation)
                   length (:length operation)
                   text (:text operation)]
               (println "🔧 REPLACE at position:" start "length:" length "with:" (pr-str text))
               (try
                 ;; Use gap buffer's atomic replace API
                 (.replace ^js wasm-instance start length text)
                 (let [new-cursor (+ start (count text))]
                   (rf/dispatch [:editor/transaction-success
                                {:operation operation
                                 :new-cursor new-cursor}]))  ; Cursor moves to end of replacement
                 (catch js/Error error
                   (println "❌ Replace error:" error)
                   (rf/dispatch [:editor/transaction-failure
                                {:error (str error)
                                 :operation operation}]))))

             ;; Unknown operation type
             (do
               (println "Unknown operation type:" (:op operation))
               (rf/dispatch [:editor/transaction-failure
                            {:error "Unknown operation type"
                             :operation operation}])))))))))))

;; Transaction success handler - updates state and continues processing
(rf/reg-event-fx
 :editor/transaction-success
 (fn [{:keys [db]} [_ {:keys [new-cursor operation]}]]
   "Handle successful transaction completion"
   (println "✅ Queue: Transaction success. New cursor:" new-cursor)
   (try
     (let [active-window    (db/find-window-in-tree (:window-tree db) (:active-window-id db))
           active-buffer-id (:buffer-id active-window)
           ;; Apply cursor adjustment (e.g., for electric-pair mode)
           cursor-adjust    (:cursor-adjust operation 0)
           final-cursor     (max 0 (+ new-cursor cursor-adjust))

           _ (println "🎯 Transaction result - operation:" (:op operation) "final cursor:" final-cursor)

           ;; Calculate line/column coordinates for the new cursor position
           ^js wasm-instance (get-in db [:buffers active-buffer-id :wasm-instance])
           text              (when wasm-instance (.getText wasm-instance))
           line-col          (when text (linear-pos-to-line-col text final-cursor))

           ;; Invalidate cache by incrementing version and updating cached values
           lines (when text (clojure.string/split text #"\n" -1))
           line-count (if lines (count lines) 1)

           ;; Update window tree with new cursor position (Phase 3)
           window-tree (:window-tree db)
           new-window-tree (db/update-window-in-tree window-tree (:active-window-id db)
                                                     #(assoc % :cursor-position (or line-col {:line 0 :column 0})))

           updated-db (-> db
                          ;; Remove completed operation from queue
                          (update :transaction-queue rest)
                          ;; Clear in-flight flag
                          (assoc :transaction-in-flight? false)
                          (assoc :window-tree new-window-tree)
                          ;; Mark buffer as modified
                          (assoc-in [:buffers active-buffer-id :is-modified?] true)
                          ;; Update cache with fresh values
                          (assoc-in [:buffers active-buffer-id :cache :text] (or text ""))
                          (assoc-in [:buffers active-buffer-id :cache :line-count] line-count)
                          ;; Update system state
                          (update-in [:system :last-transaction-id] inc)
                          (assoc-in [:ui :view-needs-update?] true))]

       ;; Continue processing the queue and trigger incremental parsing
       {:db updated-db
        :fx [[:dispatch [:cursor/set-position final-cursor]]
             [:dispatch [:buffer/increment-version active-buffer-id]]
             [:editor/process-queue updated-db]
             [:dispatch [:parser/request-incremental-parse
                         {:op       (:op operation)
                          :position final-cursor
                          :text     (:text operation)}]]
             [:dispatch [:lsp/on-buffer-changed active-buffer-id]]
             ;; Ensure hidden input stays focused after DOM updates
             [:focus-editor true]]})
     (catch js/Error error
       (println "❌ Error processing transaction success:" error)
       ;; Clear in-flight flag and continue processing
       (let [updated-db (-> db
                            (update :transaction-queue rest)
                            (assoc :transaction-in-flight? false))]
         {:db updated-db
          :fx [[:editor/process-queue updated-db]]})))))

;; Transaction failure handler - continues processing queue
(rf/reg-event-fx
 :editor/transaction-failure
 (fn [{:keys [db]} [_ {:keys [error operation]}]]
   "Handle transaction failure"
   (println "❌ Queue: Transaction failed:" error "for operation:" operation)
   (let [updated-db (-> db
                        ;; Remove failed operation from queue
                        (update :transaction-queue rest)
                        ;; Clear in-flight flag
                        (assoc :transaction-in-flight? false)
                        ;; Store error for debugging
                        (assoc-in [:system :last-error] error))]
     ;; Continue processing the queue
     {:db updated-db
      :fx [[:editor/process-queue updated-db]]})))

;; -- Transaction Events (Legacy) --

(rf/reg-event-fx
 :dispatch-transaction
 (fn [{:keys [db]} [_ transaction]]
   "Apply a transaction to the WASM kernel with before/after-change hooks"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]

     (if (and wasm-instance active-buffer-id)
       (let [transaction-id (inc (get-in db [:system :last-transaction-id]))
             {:keys [type pos text length]} transaction

             ;; Get old text for before-change hook context
             old-text (when (and (#{:delete :replace} type) wasm-instance)
                        (.substring ^js wasm-instance pos (+ pos (or length 0))))

             ;; Context for before-change hook
             before-context {:buffer-id active-buffer-id
                            :start pos
                            :end (+ pos (or length 0))
                            :old-text (or old-text "")
                            :new-text (or text "")
                            :type type
                            :timestamp (js/Date.now)}

             ;; Convert to WASM transaction format using constants
             wasm-transaction (case type
                               :insert {:type const/TRANSACTION_INSERT :position pos :text text}
                               :delete {:type const/TRANSACTION_DELETE :position pos :length length}
                               :replace {:type const/TRANSACTION_REPLACE :position pos :length length :text text}
                               {:type const/TRANSACTION_INSERT :position 0 :text ""}) ; fallback

             ;; Convert to JSON string for WASM
             transaction-json (js/JSON.stringify (clj->js wasm-transaction))]

         ;; Dispatch before-change hook
         (rf/dispatch [:hook/run :before-change-hook before-context])

         ;; Apply transaction and handle result synchronously
         (try
           (let [patch-json (.applyTransaction ^js wasm-instance transaction-json)]
             (println "🔧 Transaction applied. Patch JSON:" patch-json)

             ;; Context for after-change hook
             (let [after-context (assoc before-context
                                   :end (+ pos (count (or text "")))
                                   :old-length (or length 0)
                                   :new-length (count (or text "")))]
               (rf/dispatch [:hook/run :after-change-hook after-context]))

             ;; Transaction successful - dispatch result event
             (rf/dispatch [:apply-transaction-result
                          {:patch-json patch-json
                           :transaction transaction
                           :transaction-id transaction-id
                           :buffer-id active-buffer-id}]))
           (catch js/Error error
             ;; Transaction failed - dispatch error event
             (println "❌ Transaction failed:" error)
             (rf/dispatch [:transaction-failed {:error (str error)}])))

         ;; Return no immediate db changes - handlers will update state
         {:db db})

       ;; WASM not ready or no active buffer
       {:db db}))))

(rf/reg-event-fx
 :apply-transaction-result
 (fn [{:keys [db]} [_ {:keys [patch-json transaction transaction-id buffer-id]}]]
   "Apply the result of a successful transaction - pure state update"
   (println "📥 Applying transaction result. Patch JSON:" patch-json)
   (try
     (let [patch (js->clj (js/JSON.parse patch-json) :keywordize-keys true)
           {:keys [type pos text length]} transaction
           new-cursor (:cursor-position patch)
           new-length (:length patch)

           ;; Invalidate affected cache ranges
           cache-start (case type
                         :insert pos
                         :delete pos
                         :replace pos
                         0)
           cache-end (case type
                       :insert (+ pos (count text))
                       :delete (+ pos length)
                       :replace (+ pos (count text))
                       cache-start)

           updated-cache (cache/invalidate-cache-range
                         (get-in db [:ui :text-cache])
                         cache-start cache-end)

           ;; Set cursor position - if not provided by WASM, calculate it
           final-cursor (or new-cursor
                           (case type
                             :insert (+ pos (count text))
                             :delete pos
                             :replace (+ pos (count text))
                             pos))]

       (println "✅ Final cursor position:" final-cursor "Patch:" patch)

       ;; Calculate line/column coordinates for the new cursor position
       (let [^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
             text (when wasm-instance (.getText wasm-instance))
             line-col (when text
                        (let [result (linear-pos-to-line-col text final-cursor)]
                          (println "🔄 Converting cursor pos" final-cursor "to line/col:" result "from text:" (pr-str text))
                          result))]

         (let [updated-db (-> db
                              (assoc-in [:system :last-transaction-id] transaction-id)
                              (assoc-in [:system :last-patch] patch)
                              (assoc-in [:ui :view-needs-update?] true)
                              (assoc-in [:ui :text-cache] updated-cache)
                              (assoc-in [:buffers buffer-id :is-modified?] true)
                              ;; Update the new buffer-based cursor position
                              (assoc-in [:buffers buffer-id :cursor-position] (or line-col {:line 0 :column 0})))]
           {:db updated-db
            :fx [[:dispatch [:cursor/set-position final-cursor]]]})))
     (catch js/Error error
       (println "❌ Error processing transaction result:" error)
       {:db db}))))

(rf/reg-event-db
 :transaction-failed
 (fn [db [_ {:keys [error]}]]
   "Handle transaction failure"
   (println "Transaction failed:" error)
   (assoc-in db [:system :last-error] error)))

(rf/reg-event-fx
 :compound-transaction
 (fn [{:keys [db]} [_ operations]]
   "Apply multiple operations as a compound transaction"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (if wasm-instance
       (let [compound-transaction {:type const/TRANSACTION_COMPOUND :operations operations}
             transaction-json (js/JSON.stringify (clj->js compound-transaction))
             error-code (.applyTransaction ^js wasm-instance transaction-json)]

         (if (= error-code 0)
           ;; Success - update state similar to single transaction
           (rf/dispatch [:transaction-completed])
           ;; Error - handle appropriately
           (rf/dispatch [:transaction-failed (.getLastErrorMessage ^js wasm-instance)])))

       {:db db}))))

;; -- Parser Worker Management --

(rf/reg-fx
 :parser/start-worker
 (fn [{:keys [worker-path]}]
   "Create and initialize the parser Web Worker"
   (let [worker (js/Worker. (or worker-path "/parser-worker.js"))]
     ;; Set up message handler
     (.addEventListener worker "message"
       (fn [^js event]
         (let [data (.-data event)
               msg-type (.-type data)
               payload (js->clj (.-payload data) :keywordize-keys true)]
           (println "Parser worker message:" msg-type)
           (case msg-type
             "init-success"
             (rf/dispatch [:parser/worker-ready payload])

             "init-error"
             (rf/dispatch [:parser/worker-error payload])

             "parse-success"
             (rf/dispatch [:parser/ast-updated payload])

             "parse-error"
             (rf/dispatch [:parser/parse-error payload])

             "edit-success"
             (rf/dispatch [:parser/ast-updated payload])

             "edit-error"
             (rf/dispatch [:parser/parse-error payload])

             "error"
             (rf/dispatch [:parser/worker-error payload])

             (println "Unknown worker message type:" msg-type)))))

     ;; Store worker instance and initialize
     (rf/dispatch [:parser/worker-created worker])

     ;; Initialize the worker with a default language
     (.postMessage worker
       (clj->js {:type "init" :languageId "clojure"})))))

(rf/reg-event-db
 :parser/worker-created
 (fn [db [_ worker]]
   "Store the created worker instance"
   (assoc-in db [:system :parser-worker] worker)))

(rf/reg-event-db
 :parser/worker-ready
 (fn [db [_ payload]]
   "Mark parser worker as ready"
   (println "✅ Parser worker ready for language:" (:languageName payload))
   (assoc-in db [:system :parser-ready?] true)))

(rf/reg-event-db
 :parser/worker-error
 (fn [db [_ payload]]
   "Handle parser worker errors"
   (println "Parser worker error:" (:error payload))
   (assoc-in db [:system :parser-error] (:error payload))))

(rf/reg-event-db
 :parser/ast-updated
 (fn [db [_ payload]]
   "Update the AST for the active buffer"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)]
     (assoc-in db [:buffers active-buffer-id :ast] (:ast payload)))))

(rf/reg-event-db
 :parser/parse-error
 (fn [db [_ payload]]
   "Handle parsing errors"
   (println "Parse error:" (:error payload))
   db))

(rf/reg-event-fx
 :parser/request-parse
 (fn [{:keys [db]} [_ buffer-id]]
   "Request full parse of buffer content"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         parser-worker (get-in db [:system :parser-worker])
         language (:language buffer :clojure)]

     (if (and parser-worker wasm-instance)
       (do
         (let [text (.getText wasm-instance)]
           (println "Requesting full parse for buffer" buffer-id "with language" language)
           (.postMessage parser-worker
             (clj->js {:type "parse"
                      :languageId (name language)
                      :code text}))
           {:db db}))
       (do
         (println "Parser worker or WASM not ready, skipping parse")
         {:db db})))))

(rf/reg-event-fx
 :parser/request-incremental-parse
 (fn [{:keys [db]} [_ edit-details]]
   "Request incremental reparse after edit"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers active-buffer-id])
         parser-worker (get-in db [:system :parser-worker])
         language (:language buffer :clojure)]

     (if parser-worker
       (do
         (println "Requesting incremental parse for buffer" active-buffer-id)
         ;; For now, just do a full reparse
         ;; TODO: Implement true incremental parsing with edit metadata
         {:db db})
       ;; Fall back to full parse
       {:fx [[:dispatch [:parser/request-parse active-buffer-id]]]}))))
