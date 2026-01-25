(ns lexicon.core.events.ui
  "Event handlers for UI state, window management, and user interactions.

  Handles:
  - Window management (split, delete, switch)
  - UI state (selection, IME)
  - View reconciliation
  - System state (mutation observer, reconciliation)
  - Minibuffer activation and interaction
  - Echo area messages
  - WebSocket bridge communication
  - Focus management effects"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.core.db :as db]
            [lexicon.core.minibuffer :as minibuffer]
            [lexicon.core.completion.styles :as completion-styles]
            [lexicon.core.completion.metadata :as completion-metadata]))

;; =============================================================================
;; Window Management Events
;; =============================================================================

(rf/reg-event-fx
 :split-window-below
 (fn [{:keys [db]} [_]]
   "Split current window horizontally (C-x 2)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)

         ;; Create new window with same buffer
         new-window-id (db/next-window-id db)
         new-window (db/create-leaf-window new-window-id (:buffer-id active-window))

         ;; Create split node
         split-id (inc new-window-id)
         split-node (db/create-split-window :hsplit split-id active-window new-window)

         ;; Function to replace the active window with the split
         replace-window (fn replace-window [tree]
                         (if (= (:id tree) active-window-id)
                           split-node
                           (cond
                             (= (:type tree) :leaf) tree
                             (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                             (assoc tree
                                    :first (replace-window (:first tree))
                                    :second (replace-window (:second tree)))
                             :else tree)))

         new-tree (replace-window window-tree)]

     {:db (-> db
              (assoc :window-tree new-tree)
              (assoc :active-window-id new-window-id)
              (assoc :cursor-owner new-window-id)  ; Issue #62: Transfer cursor to new window
              (assoc :next-window-id (+ new-window-id 2)))})))

(rf/reg-event-fx
 :split-window-right
 (fn [{:keys [db]} [_]]
   "Split current window vertically (C-x 3)"
   (let [active-window-id (:active-window-id db)
         window-tree (:window-tree db)
         active-window (db/find-window-in-tree window-tree active-window-id)

         ;; Create new window with same buffer
         new-window-id (db/next-window-id db)
         new-window (db/create-leaf-window new-window-id (:buffer-id active-window))

         ;; Create split node (vertical this time)
         split-id (inc new-window-id)
         split-node (db/create-split-window :vsplit split-id active-window new-window)

         ;; Function to replace the active window with the split
         replace-window (fn replace-window [tree]
                         (if (= (:id tree) active-window-id)
                           split-node
                           (cond
                             (= (:type tree) :leaf) tree
                             (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                             (assoc tree
                                    :first (replace-window (:first tree))
                                    :second (replace-window (:second tree)))
                             :else tree)))

         new-tree (replace-window window-tree)]

     {:db (-> db
              (assoc :window-tree new-tree)
              (assoc :active-window-id new-window-id)
              (assoc :cursor-owner new-window-id)  ; Issue #62: Transfer cursor to new window
              (assoc :next-window-id (+ new-window-id 2)))})))

(rf/reg-event-fx
 :other-window
 (fn [{:keys [db]} [_]]
   "Switch to next window (C-x o)"
   (let [window-tree (:window-tree db)
         all-windows (db/get-all-leaf-windows window-tree)
         active-window-id (:active-window-id db)

         ;; Find current window index
         current-index (first (keep-indexed
                               (fn [idx window]
                                 (when (= (:id window) active-window-id) idx))
                               all-windows))

         ;; Get next window (wrap around)
         next-index (mod (inc (or current-index 0)) (count all-windows))
         next-window (nth all-windows next-index)
         next-window-id (:id next-window)]

     {:db (-> db
              (assoc :active-window-id next-window-id)
              (assoc :cursor-owner next-window-id))})))  ; Issue #62: Transfer cursor ownership

(rf/reg-event-db
 :set-active-window
 (fn [db [_ window-id]]
   "Set the active window by ID (used when clicking on a window)"
   (-> db
       (assoc :active-window-id window-id)
       (assoc :cursor-owner window-id))))  ; Issue #62: Transfer cursor ownership

(rf/reg-event-fx
 :delete-window
 (fn [{:keys [db]} [_]]
   "Delete current window (C-x 0)"
   (let [window-tree (:window-tree db)
         all-windows (db/get-all-leaf-windows window-tree)
         active-window-id (:active-window-id db)]

     (if (<= (count all-windows) 1)
       ;; Can't delete the last window
       {:db db}
       ;; TODO: Implement window deletion logic
       ;; This requires removing the window from the tree and rebalancing
       {:fx [[:dispatch [:echo/message "delete-window not yet implemented"]]]}))))

(rf/reg-event-fx
 :delete-other-windows
 (fn [{:keys [db]} [_]]
   "Delete all windows except current (C-x 1)"
   (let [window-tree (:window-tree db)
         active-window-id (:active-window-id db)
         active-window (db/find-window-in-tree window-tree active-window-id)]

     {:db (assoc db :window-tree active-window)})))

;; =============================================================================
;; Window State Management Events (Issue #60: State Ownership)
;; =============================================================================
;; These events provide the canonical API for updating window state.
;; Other modules MUST use these events instead of directly manipulating :window-tree.

(rf/reg-event-db
 :window/set-mark
 (fn [db [_ window-id mark-position]]
   "Set mark position for a specific window.

   This is the ONLY event that should update window mark-position.
   Other modules must dispatch this instead of direct manipulation.

   Args:
     window-id - ID of window to update (defaults to active window if nil)
     mark-position - Linear position for mark, or nil to clear"
   (let [target-window-id (or window-id (:active-window-id db))
         window-tree (:window-tree db)
         new-tree (db/update-window-in-tree window-tree target-window-id
                                            #(assoc % :mark-position mark-position))]
     (assoc db :window-tree new-tree))))

(rf/reg-event-db
 :window/set-cursor
 (fn [db [_ window-id cursor-position]]
   "Set cursor position for a specific window.

   This is the ONLY event that should update window cursor-position.
   Other modules must dispatch this instead of direct manipulation.

   Args:
     window-id - ID of window to update (defaults to active window if nil)
     cursor-position - {:line N :column N} map"
   (let [target-window-id (or window-id (:active-window-id db))
         window-tree (:window-tree db)
         new-tree (db/update-window-in-tree window-tree target-window-id
                                            #(assoc % :cursor-position cursor-position))]
     (assoc db :window-tree new-tree))))

;; =============================================================================
;; UI State Events
;; =============================================================================

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
   (rf/dispatch [:editor/queue-transaction {:op :insert :text text}])
   coeffects))

;; =============================================================================
;; View Reconciliation Events
;; =============================================================================

(rf/reg-event-db
 :view-updated
 (fn [db [_]]
   "Mark that view has been updated"
   (assoc-in db [:ui :view-needs-update?] false)))

(rf/reg-event-fx
 :reconcile-dom-state
 (fn [{:keys [db]} [_ dom-content expected-content]]
   "Reconcile DOM state with WASM state using diff algorithm"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (when wasm-instance
       ;; TODO: Implement text diffing algorithm
       ;; For now, replace entire content
       (let [current-length (.length ^js wasm-instance)]
         (.delete ^js wasm-instance 0 current-length)
         (.insert ^js wasm-instance 0 dom-content)))

     {:db (assoc-in db [:ui :view-needs-update?] true)})))

;; =============================================================================
;; System Events
;; =============================================================================

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

;; =============================================================================
;; Minibuffer Events
;; =============================================================================

(rf/reg-event-fx
 :minibuffer/activate
 (fn [{:keys [db]} [_ config]]
   "Activate the minibuffer with given configuration (Phase 6.5 Week 3-4).
   Now uses stack-based architecture to support recursive minibuffer and fix Issue #72."
   (let [already-active? (minibuffer/minibuffer-active? db)
         recursive-allowed? (:enable-recursive-minibuffers db)
         replace-mode? (:replace? config false)
         should-replace? (and already-active? replace-mode?)]
     (cond
       ;; Minibuffer already active, recursive not allowed, not replacing
       (and already-active? (not recursive-allowed?) (not replace-mode?))
       {:fx [[:dispatch [:echo/message "Command attempted to use minibuffer while in minibuffer"]]]}
       ;; Replace current frame (for M-x to command transition)
       should-replace?
       {:db (-> db
                (minibuffer/replace-current-frame config)
                (minibuffer/sync-to-legacy)
                (assoc :cursor-owner :minibuffer))}
       ;; Push new frame (normal activation or recursive)
       :else
       {:db (-> db
                (minibuffer/push-frame config)
                (minibuffer/sync-to-legacy)
                (assoc :cursor-owner :minibuffer))}))))

(rf/reg-event-fx
 :minibuffer/deactivate
 (fn [{:keys [db]} [_]]
   "Deactivate the minibuffer (Phase 6.5 Week 3-4).
   Pops current frame from stack. If stack becomes empty, restores cursor to window."
   (let [active-window-id (:active-window-id db)
         db' (-> db
                 minibuffer/pop-frame
                 minibuffer/sync-to-legacy)
         still-active? (minibuffer/minibuffer-active? db')]
     {:db (cond-> db'
            ;; If stack is now empty, restore cursor to active window
            (not still-active?)
            (assoc :cursor-owner active-window-id))
      :fx (cond-> []
            ;; Only focus editor if fully deactivated
            (not still-active?)
            (conj [:focus-editor]))})))

(rf/reg-event-fx
 :minibuffer/set-input
 (fn [{:keys [db]} [_ input-text]]
   "Update the minibuffer input text (Phase 6.5 Week 3-4 - stack-based)"
   (let [on-change (get-in db [:minibuffer :on-change])]
     (if on-change
       ;; Custom on-change handler exists (e.g., for isearch)
       {:fx [[:dispatch (conj on-change input-text)]]}
       ;; Standard minibuffer - update current frame input
       {:db (-> db
                (minibuffer/update-current-frame {:input input-text})
                minibuffer/sync-to-legacy)}))))

(rf/reg-event-db
 :minibuffer/complete
 (fn [db [_]]
   "TAB completion in minibuffer - Phase 7.8.1 with visual completion list"
   (let [minibuffer (:minibuffer db)
         input (:input minibuffer)
         completions (:completions minibuffer)
         ;; Get effective styles for current completion
         styles (or (get-in db [:completion :styles]) [:basic :substring :flex])
         ;; Filter completions using styles
         matches (if (clojure.string/blank? input)
                  completions
                  (completion-styles/filter-candidates
                   input
                   completions
                   :styles-list styles))]
     (cond
       ;; No completions available
       (empty? completions)
       db

       ;; Single match - complete it
       (= (count matches) 1)
       (-> db
           (assoc-in [:minibuffer :input] (first matches))
           (assoc-in [:minibuffer :show-completions?] false)
           (assoc-in [:minibuffer :filtered-completions] [])
           (assoc-in [:minibuffer :height-lines] 1))

       ;; Multiple matches - show completion list
       (> (count matches) 1)
       (let [common-prefix (reduce (fn [prefix candidate]
                                    (loop [i 0]
                                      (if (and (< i (count prefix))
                                              (< i (count candidate))
                                              (= (nth prefix i) (nth candidate i)))
                                        (recur (inc i))
                                        (subs prefix 0 i))))
                                  (first matches)
                                  (rest matches))
             num-candidates-to-show (min 10 (count matches))
             height-lines (inc num-candidates-to-show)]  ; +1 for input line
         (-> db
             (assoc-in [:minibuffer :input] (if (> (count common-prefix) (count input))
                                              common-prefix
                                              input))
             (assoc-in [:minibuffer :filtered-completions] matches)
             (assoc-in [:minibuffer :show-completions?] true)
             (assoc-in [:minibuffer :completion-index] 0)
             (assoc-in [:minibuffer :height-lines] height-lines)))

       ;; No matches
       :else
       db))))

(rf/reg-event-fx
 :minibuffer/confirm
 (fn [{:keys [db]} [_]]
   "Confirm minibuffer input (Phase 6.5 Week 3-4 - stack-based).
   IMPORTANT: Does NOT auto-deactivate anymore! Commands must deactivate explicitly
   or use :replace? true to take over the minibuffer slot. This fixes Issue #72."
   (let [input (minibuffer/get-input db)
         on-confirm (minibuffer/get-on-confirm db)
         persist? (minibuffer/get-persist? db)]
     (if on-confirm
       (if persist?
         ;; Multi-step prompt - dispatch without deactivating
         {:fx [[:dispatch (conj on-confirm input)]]}
         ;; Single-step prompt - dispatch then deactivate
         {:fx [[:dispatch (conj on-confirm input)]
               [:dispatch [:minibuffer/deactivate]]]})
       ;; No handler - just deactivate
       {:fx [[:dispatch [:minibuffer/deactivate]]]}))))

;; Phase 7.8.1: Completion navigation events
(rf/reg-event-db
 :minibuffer/completion-next
 (fn [db [_]]
   "Move to next completion candidate (Arrow Down)"
   (let [completion-index (get-in db [:minibuffer :completion-index] 0)
         completions (get-in db [:minibuffer :filtered-completions] [])
         num-completions (count completions)
         new-index (if (< completion-index (dec num-completions))
                     (inc completion-index)
                     0)]  ; Wrap around to start
     (assoc-in db [:minibuffer :completion-index] new-index))))

(rf/reg-event-db
 :minibuffer/completion-prev
 (fn [db [_]]
   "Move to previous completion candidate (Arrow Up)"
   (let [completion-index (get-in db [:minibuffer :completion-index] 0)
         completions (get-in db [:minibuffer :filtered-completions] [])
         num-completions (count completions)
         new-index (if (> completion-index 0)
                     (dec completion-index)
                     (dec num-completions))]  ; Wrap around to end
     (assoc-in db [:minibuffer :completion-index] new-index))))

(rf/reg-event-db
 :minibuffer/select-completion
 (fn [db [_ index]]
   "Select a completion candidate (click or Enter on highlighted)"
   (let [completions (get-in db [:minibuffer :filtered-completions] [])
         selected-completion (nth completions index nil)]
     (if selected-completion
       (-> db
           (assoc-in [:minibuffer :input] selected-completion)
           (assoc-in [:minibuffer :show-completions?] false)
           (assoc-in [:minibuffer :filtered-completions] [])
           (assoc-in [:minibuffer :height-lines] 1))
       db))))

;; =============================================================================
;; Echo Area Events
;; =============================================================================

(rf/reg-event-fx
 :echo/message
 (fn [{:keys [db]} [_ message & [persist?]]]
   "Display a message in the minibuffer (unified echo area - Phase 7.8.1).

   By default, auto-clears after 3 seconds unless persist? is true.
   During query-replace, messages should persist until user responds."
   (let [old-timeout-id (get-in db [:minibuffer :message-timeout-id])
         query-replace-active? (get-in db [:ui :query-replace :active?])]
     ;; Clear previous timeout if any
     (when old-timeout-id
       (js/clearTimeout old-timeout-id))
     ;; Set new message and create new timeout (unless persisting or query-replace active)
     (if (or persist? query-replace-active?)
       {:db (-> db
                (assoc-in [:minibuffer :message] message)
                (assoc-in [:minibuffer :message-timeout-id] nil)
                ;; Also update deprecated echo-area for backward compatibility
                (assoc-in [:echo-area :message] message)
                (assoc-in [:echo-area :timeout-id] nil))}
       (let [timeout-id (js/setTimeout
                          #(rf/dispatch [:echo/clear])
                          3000)]
         {:db (-> db
                  (assoc-in [:minibuffer :message] message)
                  (assoc-in [:minibuffer :message-timeout-id] timeout-id)
                  ;; Also update deprecated echo-area for backward compatibility
                  (assoc-in [:echo-area :message] message)
                  (assoc-in [:echo-area :timeout-id] timeout-id))})))))

(rf/reg-event-db
 :echo/clear
 (fn [db [_]]
   "Clear the echo area message (minibuffer unified - Phase 7.8.1)"
   (let [timeout-id (get-in db [:minibuffer :message-timeout-id])]
     (when timeout-id
       (js/clearTimeout timeout-id))
     (-> db
         (assoc-in [:minibuffer :message] "")
         (assoc-in [:minibuffer :message-timeout-id] nil)
         ;; Also clear deprecated echo-area for backward compatibility
         (assoc-in [:echo-area :message] "")
         (assoc-in [:echo-area :timeout-id] nil)))))

;; =============================================================================
;; WebSocket Bridge Communication
;; =============================================================================

(rf/reg-fx
 :ws/connect
 (fn [{:keys [url on-open on-message on-close on-error]}]
   "Create WebSocket connection to lexicon-bridge server with ticket authentication"
   (when (and url (not (get-in @re-frame.db/app-db [:bridge :ws])))
     (try
       ;; First, get a ticket from the HTTP server
       (-> (js/fetch "http://localhost:30304/api/ws-ticket"
                     (clj->js {:method "POST"
                               :headers {"Content-Type" "application/json"}}))
           (.then (fn [response]
                    (if (.-ok response)
                      (.json response)
                      (throw (js/Error. (str "Failed to get ticket: " (.-status response)))))))
           (.then (fn [ticket-data]
                    (let [ticket (.-ticket ^js ticket-data)
                          ws-url (str url "?ticket=" ticket)
                          ws (js/WebSocket. ws-url)]
                      (println "🎫 :client Got WebSocket ticket:" (subs ticket 0 8) "...")

                      ;; Store the WebSocket immediately to prevent duplicate connections
                      (rf/dispatch [:ws/connecting ws])

                      (set! (.-onopen ws)
                            (fn [event]
                              (println "✅ :client WebSocket connected with valid ticket")
                              (rf/dispatch [:ws/opened event])))

                      (set! (.-onmessage ws)
                            (fn [event]
                              (let [data (js/JSON.parse (.-data event))]
                                (println "📨 :client Received message:" (.-type data))
                                (rf/dispatch [:ws/message-received (js->clj data :keywordize-keys true)]))))

                      (set! (.-onclose ws)
                            (fn [event]
                              (println "🔌 :client WebSocket closed. Code:" (.-code event) "Reason:" (.-reason event))
                              (rf/dispatch [:ws/closed {:code (.-code event)
                                                        :reason (.-reason event)
                                                        :was-clean (.-wasClean event)}])))

                      (set! (.-onerror ws)
                            (fn [event]
                              (println "❌ :client WebSocket error")
                              (rf/dispatch [:ws/error {:error "WebSocket connection error"}])))
                      )))
           (.catch (fn [error]
                     (println "❌ :client Failed to get WebSocket ticket:" error)
                     (rf/dispatch [:ws/error {:error (str "Ticket acquisition failed: " error)}]))))
       (catch js/Error error
         (println "❌ :client WebSocket creation error:" error)
         (rf/dispatch [:ws/error {:error (str "Failed to create WebSocket: " error)}]))))))

(rf/reg-fx
 :ws/send
 (fn [{:keys [message]}]
   "Send message over active WebSocket connection"
   (println "🌐 WS: send effect called with message type:" (:type message))
   (let [ws (get-in @re-frame.db/app-db [:bridge :ws])
         status (get-in @re-frame.db/app-db [:bridge :status])]
     (println "🌐 WS: Connection status:" status "WebSocket exists:" (boolean ws))
     (println "🌐 WS: WebSocket readyState:" (when ws (.-readyState ws)))
     (println "🌐 WS: WebSocket URL:" (when ws (.-url ws)))
     (if (and ws (= status :connected))
       (try
         (println "🌐 WS: Sending message:" message)
         (.send ws (js/JSON.stringify (clj->js message)))
         (println "🌐 WS: Message sent successfully to:" (when ws (.-url ws)))
         (catch js/Error error
           (println "❌ WS: Failed to send WebSocket message:" error)
           (rf/dispatch [:ws/error {:error (str "Send failed: " error)}])))
       (println "❌ WS: Cannot send message - WebSocket not connected. Status:" status)))))

;; WebSocket Event Handlers

(rf/reg-event-db
 :ws/connecting
 (fn [db [_ ws]]
   "Set WebSocket connection status to connecting"
   (-> db
       (assoc-in [:bridge :ws] ws)
       (assoc-in [:bridge :status] :connecting))))

(rf/reg-event-db
 :ws/opened
 (fn [db [_ event]]
   "Handle WebSocket connection opened"
   (println "✅ Connected to lexicon-bridge")
   (-> db
       (assoc-in [:bridge :status] :connected)
       (assoc-in [:bridge :retry-count] 0))))

(rf/reg-event-fx
 :ws/closed
 (fn [{:keys [db]} [_ {:keys [code reason was-clean]}]]
   "Handle WebSocket connection closed"
   (println "🔌 WebSocket connection closed. Code:" code "Reason:" reason "Clean:" was-clean)
   (let [retry-count (get-in db [:bridge :retry-count] 0)
         max-retries (get-in db [:bridge :max-retries] 5)]
     {:db (-> db
              (assoc-in [:bridge :ws] nil)
              (assoc-in [:bridge :status] :disconnected))
      :fx (if (< retry-count max-retries)
            [[:dispatch-later {:ms 2000 :dispatch [:ws/connect]}]]
            [])})))

(rf/reg-event-fx
 :ws/error
 (fn [{:keys [db]} [_ {:keys [error]}]]
   "Handle WebSocket error"
   (println "❌ WebSocket error:" error)
   (let [retry-count (get-in db [:bridge :retry-count] 0)
         max-retries (get-in db [:bridge :max-retries] 5)]
     {:db (-> db
              (assoc-in [:bridge :status] :disconnected)
              (update-in [:bridge :retry-count] inc))
      :fx (if (< retry-count max-retries)
            [[:dispatch-later {:ms 3000 :dispatch [:ws/connect]}]]
            [])})))

(rf/reg-event-fx
 :ws/connect
 (fn [{:keys [db]} [_]]
   "Initiate WebSocket connection to bridge with ticket authentication"
   (let [url (get-in db [:bridge :url] "ws://localhost:30303")]
     (println "🌉 :client Initiating WebSocket connection to:" url)
     {:fx [[:ws/connect {:url url}]]})))

(rf/reg-event-fx
 :ws/message-received
 (fn [{:keys [db]} [_ message]]
   "Handle incoming WebSocket message from bridge"
   (let [msg-type (:type message)]
     (case msg-type
       "lsp/started"
       {:fx [[:dispatch [:lsp/server-started message]]]}

       "lsp/stopped"
       {:fx [[:dispatch [:lsp/server-stopped message]]]}

       "lsp/message"
       {:fx [[:dispatch [:lsp/message-received message]]]}

       "error"
       (do
         (println "Bridge error:" (:message message))
         {:db db})

       ;; Unknown message type
       (do
         (println "Unknown bridge message type:" msg-type)
         {:db db})))))

;; =============================================================================
;; Viewport Management
;; =============================================================================

(rf/reg-event-db
 :update-viewport
 (fn [db [_ start-line end-line]]
   "Update the viewport for the active window to show lines from start-line to end-line"
   (let [active-window-id (:active-window-id db)
         update-fn (fn update-viewport-in-tree [tree]
                    (cond
                      (= (:type tree) :leaf)
                      (if (= (:id tree) active-window-id)
                        (assoc tree :viewport {:start-line start-line :end-line end-line})
                        tree)

                      (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                      (assoc tree
                             :first (update-viewport-in-tree (:first tree))
                             :second (update-viewport-in-tree (:second tree)))

                      :else tree))]
     (assoc db :window-tree (update-fn (:window-tree db))))))

;; =============================================================================
;; Effects
;; =============================================================================

(rf/reg-fx
 :focus-editor
 (fn [_]
   "Focus the hidden input element to enable keyboard input"
   (when-let [hidden-input (js/document.querySelector ".hidden-input")]
     (.focus hidden-input))))

;; TODO: Issue #62 - :blur-editor removed - wrong architectural approach
;; Proper fix: Implement cursor singleton with :cursor-owner field
