(ns lexicon.events
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.cache :as cache]
            [lexicon.constants :as const]))

;; -- Initialization Events --

(rf/reg-event-db
 :initialize-db
 (fn [_ _]
   "Initialize the application database with default state"
   db/default-db))

(rf/reg-event-db
 :wasm-module-loaded
 (fn [db [_ {:keys [instance constructor]}]]
   "Store the loaded WASM module instance and constructor in the app state"
   (-> db
       (assoc :initialized? true)
       (assoc-in [:system :wasm-constructor] constructor)
       (assoc-in [:buffers 1 :wasm-instance] instance))))

;; -- Buffer Management Events --

(rf/reg-event-db
 :create-buffer
 (fn [db [_ name wasm-instance]]
   "Create a new buffer with the given name and WASM instance"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         new-buffer (db/create-buffer buffer-id name wasm-instance)]
     (assoc-in db [:buffers buffer-id] new-buffer))))

(rf/reg-event-db
 :switch-buffer
 (fn [db [_ buffer-id]]
   "Switch to the specified buffer by updating the active window"
   (if (get-in db [:buffers buffer-id])
     (assoc-in db [:windows (:active-window-id db) :buffer-id] buffer-id)
     db))) ; Ignore if buffer doesn't exist

;; -- Input Handling Events --

(rf/reg-event-fx
 :handle-text-input
 (fn [{:keys [db]} [_ {:keys [input-type data dom-cursor-pos]}]]
   "Handle text input events with proper cursor position from app state"
   (let [current-cursor (get-in db [:ui :cursor-position] 0)
         insert-pos current-cursor  ; Use app state cursor position
         
         
         transaction (case input-type
                       "insertText"
                       (when data
                         {:type :insert :pos insert-pos :text data})
                       
                       "insertCompositionText"
                       (when data
                         ;; Handle IME composition
                         (rf/dispatch [:ime-composition-update data])
                         nil)
                       
                       "deleteContentBackward"
                       (when (> insert-pos 0)
                         {:type :delete :pos (dec insert-pos) :length 1})
                       
                       "deleteContentForward"
                       {:type :delete :pos insert-pos :length 1}
                       
                       "insertFromPaste"
                       (when data
                         {:type :insert :pos insert-pos :text data})
                       
                       ;; Log unhandled input types
                       (do (println "Unhandled input type:" input-type) nil))]
     
     (if transaction
       {:fx [[:dispatch [:dispatch-transaction transaction]]]}
       {:db db}))))

;; -- Transaction Events --

(rf/reg-event-fx
 :dispatch-transaction
 (fn [{:keys [db]} [_ transaction]]
   "Apply a transaction to the WASM kernel - side effect only"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     
     (if (and wasm-instance active-buffer-id)
       (let [transaction-id (inc (get-in db [:system :last-transaction-id]))
             {:keys [type pos text length]} transaction
             
             ;; Convert to WASM transaction format using constants
             wasm-transaction (case type
                               :insert {:type const/TRANSACTION_INSERT :position pos :text text}
                               :delete {:type const/TRANSACTION_DELETE :position pos :length length}
                               :replace {:type const/TRANSACTION_REPLACE :position pos :length length :text text}
                               {:type const/TRANSACTION_INSERT :position 0 :text ""}) ; fallback
             
             ;; Convert to JSON string for WASM
             transaction-json (js/JSON.stringify (clj->js wasm-transaction))]
         
         ;; Apply transaction and handle result synchronously
         (try
           (let [patch-json (.applyTransaction ^js wasm-instance transaction-json)]
             (println "🔧 Transaction applied. Patch JSON:" patch-json)
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

;; New event handlers for async transaction results

(rf/reg-event-db
 :apply-transaction-result
 (fn [db [_ {:keys [patch-json transaction transaction-id buffer-id]}]]
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
       (-> db
           (assoc-in [:system :last-transaction-id] transaction-id)
           (assoc-in [:system :last-patch] patch)
           (assoc-in [:ui :cursor-position] final-cursor)
           (assoc-in [:ui :view-needs-update?] true)
           (assoc-in [:ui :text-cache] updated-cache)
           (assoc-in [:buffers buffer-id :is-modified?] true)))
     (catch js/Error error
       (println "❌ Error processing transaction result:" error)
       db))))

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
   (let [active-window (get (:windows db) (:active-window-id db))
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

;; -- UI State Events --

(rf/reg-event-db
 :set-cursor-position
 (fn [db [_ position]]
   "Update cursor position"
   (assoc-in db [:ui :cursor-position] position)))

(rf/reg-event-db
 :set-selection
 (fn [db [_ start end]]
   "Update selection range"
   (assoc-in db [:ui :selection] {:start start :end end})))

(rf/reg-event-db
 :ime-composition-start
 (fn [db [_]]
   "Start IME composition"
   (assoc-in db [:ui :ime-composing?] true)))

(rf/reg-event-db
 :ime-composition-update
 (fn [db [_ text]]
   "Update IME composition text"
   (assoc-in db [:ui :ime-composition-text] text)))

(rf/reg-event-db
 :ime-composition-end
 (fn [db [_ final-text]]
   "End IME composition and commit text"
   (-> db
       (assoc-in [:ui :ime-composing?] false)
       (assoc-in [:ui :ime-composition-text] ""))))

(rf/reg-event-fx
 :ime-commit-composition
 (fn [coeffects [_ text position]]
   "Commit IME composition as a transaction"
   (rf/dispatch [:dispatch-transaction {:type :insert
                                        :pos position
                                        :text text}])
   coeffects))

;; -- View Reconciliation Events --

(rf/reg-event-db
 :view-updated
 (fn [db [_]]
   "Mark that view has been updated"
   (assoc-in db [:ui :view-needs-update?] false)))

(rf/reg-event-fx
 :reconcile-dom-state
 (fn [{:keys [db]} [_ dom-content expected-content]]
   "Reconcile DOM state with WASM state using diff algorithm"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (when wasm-instance
       ;; TODO: Implement text diffing algorithm
       ;; For now, replace entire content
       (.deleteText ^js wasm-instance 0 (.getLength ^js wasm-instance))
       (.insertText ^js wasm-instance 0 dom-content))
     
     {:db (assoc-in db [:ui :view-needs-update?] true)})))

;; -- System Events --

(rf/reg-event-db
 :set-mutation-observer
 (fn [db [_ observer]]
   "Store MutationObserver instance"
   (assoc-in db [:system :mutation-observer] observer)))

(rf/reg-event-db
 :set-reconciliation-active
 (fn [db [_ active?]]
   "Set reconciliation active flag"
   (assoc-in db [:system :reconciliation-active?] active?)))

;; -- File System Access Events --

(rf/reg-event-fx
 :save-buffer
 (fn [{:keys [db]} [_]]
   "Save the active buffer to disk"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         file-handle (:file-handle active-buffer)]
     
     (if wasm-instance
       (let [content (.getText ^js wasm-instance)]
         (if file-handle
           ;; Save to existing file
           {:fx [[:save-to-file-handle {:file-handle file-handle
                                        :content content
                                        :buffer-id active-buffer-id}]]}
           ;; Prompt for save location
           {:fx [[:save-file-picker {:content content
                                     :buffer-id active-buffer-id}]]}))
       {:db db}))))

(rf/reg-fx
 :save-file-picker
 (fn [{:keys [content buffer-id]}]
   (-> (js/window.showSaveFilePicker)
       (.then (fn [file-handle]
                (-> (.createWritable file-handle)
                    (.then (fn [writable]
                             (-> (.write writable content)
                                 (.then (fn []
                                          (.close writable)
                                          (rf/dispatch [:buffer-saved 
                                                       {:buffer-id buffer-id
                                                        :file-handle file-handle}])))
                                 (.catch (fn [error]
                                           (println "Write failed:" error))))))
                    (.catch (fn [error]
                              (println "Failed to create writable stream:" error))))))
       (.catch (fn [error]
                 (println "Save cancelled or failed:" error))))))

(rf/reg-fx
 :save-to-file-handle
 (fn [{:keys [file-handle content buffer-id]}]
   (-> (.createWritable file-handle)
       (.then (fn [writable]
                (-> (.write writable content)
                    (.then (fn []
                             (.close writable)
                             (rf/dispatch [:buffer-saved 
                                          {:buffer-id buffer-id
                                           :file-handle file-handle}])))
                    (.catch (fn [error]
                              (println "Write failed:" error))))))
       (.catch (fn [error]
                 (println "Failed to create writable stream:" error))))))

(rf/reg-event-db
 :buffer-saved
 (fn [db [_ {:keys [buffer-id file-handle]}]]
   "Mark buffer as saved and update file handle"
   (let [file-name (.-name file-handle)]
     (-> db
         (assoc-in [:buffers buffer-id :is-modified?] false)
         (assoc-in [:buffers buffer-id :file-handle] file-handle)
         (assoc-in [:buffers buffer-id :name] file-name)))))

(rf/reg-event-fx
 :find-file
 (fn [{:keys [db]} [_]]
   "Open a file from disk"
   {:fx [[:open-file-picker]]}))

(rf/reg-fx
 :open-file-picker
 (fn [_]
   "Handle file picker interaction and dispatch appropriate events"
   (-> (js/window.showOpenFilePicker)
       (.then (fn [file-handles]
                (let [file-handle (first file-handles)]
                  (-> (.getFile file-handle)
                      (.then (fn [file]
                               (-> (.text file)
                                   (.then (fn [content]
                                            (rf/dispatch [:file-read-success 
                                                         {:file-handle file-handle
                                                          :content content
                                                          :name (.-name file)}])))
                                   (.catch (fn [error]
                                             (rf/dispatch [:file-read-failure 
                                                          {:error error
                                                           :message "Failed to read file content"}]))))))
                      (.catch (fn [error]
                                (rf/dispatch [:file-read-failure 
                                             {:error error
                                              :message "Failed to access file"}])))))))
       (.catch (fn [error]
                 ;; Don't dispatch error for user cancellation
                 (when (not= (.-name error) "AbortError")
                   (rf/dispatch [:file-read-failure 
                                {:error error
                                 :message "File picker failed"}])))))))

(rf/reg-event-fx
 :file-read-success
 (fn [{:keys [db]} [_ {:keys [file-handle content name]}]]
   "Handle successful file read - create new buffer and switch to it"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         WasmEditorCore (get-in db [:system :wasm-constructor])
         wasm-instance (WasmEditorCore.)]
     ;; Initialize WASM instance with file content
     (.init wasm-instance content)
     
     ;; Create new buffer and update app state
     (let [new-buffer {:id buffer-id
                       :wasm-instance wasm-instance
                       :file-handle file-handle
                       :name name
                       :is-modified? false
                       :mark-position nil}]
       {:db (-> db
                (assoc-in [:buffers buffer-id] new-buffer)
                (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))))

(rf/reg-event-db
 :file-read-failure
 (fn [db [_ {:keys [error message]}]]
   "Handle file read failure"
   (println "File read failed:" message error)
   ;; Could add user notification here in the future
   db))

;; -- Buffer Lifecycle Events --

(rf/reg-event-fx
 :close-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Close a buffer and free its WASM memory, with defensive logic"
   (let [buffers (:buffers db)
         buffer-to-close (get buffers buffer-id)
         active-window-id (:active-window-id db)
         active-window (get (:windows db) active-window-id)
         currently-active-buffer-id (:buffer-id active-window)]
     
     (if buffer-to-close
       (let [wasm-instance (:wasm-instance buffer-to-close)
             remaining-buffers (dissoc buffers buffer-id)
             remaining-buffer-ids (keys remaining-buffers)]
         
         ;; Free the WASM instance memory
         (when wasm-instance
           (.free ^js wasm-instance))
         
         (if (empty? remaining-buffer-ids)
           ;; No buffers left - create a new default *scratch* buffer
           (let [new-buffer-id (inc (apply max 0 (keys buffers)))
                 WasmEditorCore (get-in db [:system :wasm-constructor])
                 new-wasm-instance (WasmEditorCore.)]
             (.init new-wasm-instance "")
             {:db (-> db
                      (assoc :buffers {new-buffer-id {:id new-buffer-id
                                                      :wasm-instance new-wasm-instance
                                                      :file-handle nil
                                                      :name "*scratch*"
                                                      :is-modified? false
                                                      :mark-position nil}})
                      (assoc-in [:windows active-window-id :buffer-id] new-buffer-id))})
           
           ;; Other buffers exist - switch to another buffer if necessary
           (let [new-active-buffer-id (if (= buffer-id currently-active-buffer-id)
                                        ;; Need to switch to a different buffer
                                        (first remaining-buffer-ids)
                                        ;; Keep current active buffer
                                        currently-active-buffer-id)]
             {:db (-> db
                      (assoc :buffers remaining-buffers)
                      (assoc-in [:windows active-window-id :buffer-id] new-active-buffer-id))})))
       
       ;; Buffer not found - no action needed
       {:db db}))))

;; -- WASM Loading Error Events --

(rf/reg-event-db
 :wasm-load-failed
 (fn [db [_ error]]
   "Handle WASM loading failure"
   (println "WASM load failed, showing error to user:" error)
   (assoc-in db [:system :wasm-error] (str error))))

(rf/reg-event-fx
 :reconcile-dom-if-needed
 (fn [{:keys [db]} [_ dom-content]]
   "Check if DOM content differs from WASM content and reconcile if needed"
   (let [reconciliation-active? (get-in db [:system :reconciliation-active?])
         active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     ;; Only process if reconciliation is not currently active
     (when (and (not reconciliation-active?) wasm-instance)
       (let [expected-content (.getText ^js wasm-instance)]
         (when (not= dom-content expected-content)
           (println "🔄 Reconciling DOM state")
           {:fx [[:dispatch [:reconcile-dom-state dom-content expected-content]]]})))
     {:db db})))

;; -- Viewport Events for Virtualized Rendering --

(rf/reg-event-db
 :update-viewport
 (fn [db [_ start-line end-line]]
   "Update the viewport for the active window"
   (let [active-window-id (:active-window-id db)]
     (-> db
         (assoc-in [:windows active-window-id :viewport :start-line] start-line)
         (assoc-in [:windows active-window-id :viewport :end-line] end-line)))))

;; -- Region Selection and Kill Ring Events --

(rf/reg-event-db
 :set-mark
 (fn [db [_]]
   "Set the mark at the current cursor position"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         cursor-pos (get-in db [:ui :cursor-position])]
     (assoc-in db [:buffers active-buffer-id :mark-position] cursor-pos))))

(rf/reg-event-fx
 :kill-region
 (fn [{:keys [db]} [_]]
   "Kill (cut) the region between point and mark"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         mark-position (:mark-position active-buffer)
         cursor-pos (get-in db [:ui :cursor-position])]
     
     (if (and wasm-instance mark-position)
       (let [start (min cursor-pos mark-position)
             end (max cursor-pos mark-position)
             length (- end start)]
         (if (> length 0)
           (let [killed-text (.getTextInRange ^js wasm-instance start end)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc-in [:buffers active-buffer-id :mark-position] nil))
              :fx [[:dispatch [:dispatch-transaction 
                              {:type :delete
                               :pos start
                               :length length}]]]})
           {:db db}))
       (do
         (println "Mark not set")
         {:db db})))))

(rf/reg-event-fx
 :yank
 (fn [{:keys [db]} [_]]
   "Yank (paste) the most recent kill"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         kill-ring (:kill-ring db)
         cursor-pos (get-in db [:ui :cursor-position])]
     
     (if (and wasm-instance (seq kill-ring))
       (let [text-to-yank (first kill-ring)]
         {:fx [[:dispatch [:dispatch-transaction 
                          {:type :insert
                           :pos cursor-pos
                           :text text-to-yank}]]]})
       {:db db}))))
