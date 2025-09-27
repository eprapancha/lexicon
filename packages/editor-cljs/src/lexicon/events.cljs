(ns lexicon.events
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.cache :as cache]))

;; -- Initialization Events --

(rf/reg-event-db
 :initialize-db
 (fn [_ _]
   "Initialize the application database with default state"
   db/default-db))

(rf/reg-event-db
 :wasm-module-loaded
 (fn [db [_ wasm-instance]]
   "Store the loaded WASM module instance and mark as initialized"
   (-> db
       (assoc :wasm-handle wasm-instance)
       (assoc :initialized? true)
       (assoc :active-buffer-id :default)
       (assoc-in [:buffers :default] (db/default-buffer)))))

;; -- Buffer Management Events --

(rf/reg-event-db
 :create-buffer
 (fn [db [_ buffer-id content]]
   "Create a new buffer with the given ID and content"
   (assoc-in db [:buffers buffer-id] (db/create-buffer buffer-id content))))

(rf/reg-event-db
 :switch-buffer
 (fn [db [_ buffer-id]]
   "Switch to the specified buffer"
   (if (get-in db [:buffers buffer-id])
     (assoc db :active-buffer-id buffer-id)
     db))) ; Ignore if buffer doesn't exist

;; -- Transaction Events --

(rf/reg-event-fx
 :dispatch-transaction
 (fn [{:keys [db]} [_ transaction]]
   "Apply a transaction to the WASM kernel using new applyTransaction API"
   (let [wasm-handle (:wasm-handle db)
         active-buffer-id (:active-buffer-id db)]
     
     (if (and wasm-handle active-buffer-id)
       (let [transaction-id (inc (get-in db [:system :last-transaction-id]))
             {:keys [type position text length]} transaction
             
             ;; Convert to WASM transaction format
             wasm-transaction (case type
                               :insert {:type 0 :position position :text text}
                               :delete {:type 1 :position position :length length}
                               :replace {:type 2 :position position :length length :text text}
                               {:type 0 :position 0 :text ""}) ; fallback
             
             ;; Convert to JSON string for WASM
             transaction-json (js/JSON.stringify (clj->js wasm-transaction))
             
             ;; Apply transaction via WASM
             error-code (.applyTransaction wasm-handle transaction-json)]
         
         (if (= error-code 0) ; SUCCESS
           ;; Transaction successful
           (let [result-json (.getLastResult wasm-handle)
                 result (js->clj (js/JSON.parse result-json) :keywordize-keys true)
                 new-cursor (:cursorPosition result)
                 new-length (:length result)
                 
                 ;; Invalidate affected cache ranges
                 cache-start (case type
                               :insert position
                               :delete position
                               :replace position
                               0)
                 cache-end (case type
                             :insert (+ position (count text))
                             :delete (+ position length)
                             :replace (+ position (count text))
                             cache-start)
                 
                 updated-cache (cache/invalidate-cache-range 
                               (get-in db [:ui :text-cache])
                               cache-start cache-end)]
             
             {:db (-> db
                      (assoc-in [:system :last-transaction-id] transaction-id)
                      (assoc-in [:ui :cursor-position] new-cursor)
                      (assoc-in [:ui :view-needs-update?] true)
                      (assoc-in [:ui :text-cache] updated-cache)
                      (assoc-in [:buffers active-buffer-id :modified?] true)
                      (assoc-in [:buffers active-buffer-id :last-modified] (js/Date.now))
                      (assoc-in [:buffers active-buffer-id :length] new-length))})
           
           ;; Transaction failed
           (let [error-message (.getLastErrorMessage wasm-handle)]
             (println "Transaction failed:" error-message)
             {:db (assoc-in db [:system :last-error] error-message)
              :fx [[:dispatch [:show-error error-message]]]})))
       
       ;; WASM not ready or no active buffer
       {:db db}))))

(rf/reg-event-fx
 :compound-transaction
 (fn [{:keys [db]} [_ operations]]
   "Apply multiple operations as a compound transaction"
   (let [wasm-handle (:wasm-handle db)]
     (if wasm-handle
       (let [compound-transaction {:type 3 :operations operations}
             transaction-json (js/JSON.stringify (clj->js compound-transaction))
             error-code (.applyTransaction wasm-handle transaction-json)]
         
         (if (= error-code 0)
           ;; Success - update state similar to single transaction
           (rf/dispatch [:transaction-completed])
           ;; Error - handle appropriately
           (rf/dispatch [:transaction-failed (.getLastErrorMessage wasm-handle)])))
       
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
   (let [wasm-handle (:wasm-handle db)]
     (when wasm-handle
       ;; TODO: Implement text diffing algorithm
       ;; For now, replace entire content
       (.deleteText wasm-handle 0 (.getLength wasm-handle))
       (.insertText wasm-handle 0 dom-content))
     
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