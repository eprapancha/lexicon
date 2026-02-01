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
  - Focus management effects
  - Completion help (*Completions* buffer)"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.core.db :as db]
            [lexicon.core.minibuffer :as minibuffer]
            [lexicon.core.completion.styles :as completion-styles]
            [lexicon.core.events.buffer :as buffer-events]
            [lexicon.core.modes.special-mode :as special-mode]))

;; Forward declarations for functions used in event handlers
(declare delete-completions-window)

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
         next-window-id (:id next-window)
         next-buffer-id (:buffer-id next-window)

         ;; Get cursor position from the target WINDOW (not buffer)
         ;; Each window has its own cursor even when showing the same buffer
         window-cursor (:cursor-position next-window {:line 0 :column 0})

         ;; Convert window's line/col to linear position using buffer text
         ^js wasm-instance (get-in db [:buffers next-buffer-id :wasm-instance])
         buffer-text (when wasm-instance (.getText wasm-instance))
         linear-pos (if buffer-text
                      (buffer-events/line-col-to-linear-pos
                       buffer-text
                       (:line window-cursor 0)
                       (:column window-cursor 0))
                      0)]

     {:db (-> db
              (assoc :active-window-id next-window-id)
              (assoc :cursor-owner next-window-id)
              ;; Restore cursor position from the target window's cursor
              (assoc-in [:ui :cursor-position] linear-pos))})))  ; Issue #62: Transfer cursor ownership

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
       {:fx [[:dispatch [:message "Attempt to delete sole window"]]]}
       ;; Delete the window and switch to another
       (let [new-tree (db/delete-window-from-tree window-tree active-window-id)
             remaining-windows (db/get-all-leaf-windows new-tree)
             new-active-window (first remaining-windows)
             new-active-id (:id new-active-window)]
         {:db (-> db
                  (assoc :window-tree new-tree)
                  (assoc :active-window-id new-active-id)
                  (assoc :cursor-owner new-active-id))})))))

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
   Pops current frame from stack. If stack becomes empty, restores cursor to window.
   Also closes *Completions* buffer when fully deactivating (Emacs behavior)."
   (let [active-window-id (:active-window-id db)
         db' (-> db
                 minibuffer/pop-frame
                 minibuffer/sync-to-legacy)
         still-active? (minibuffer/minibuffer-active? db')
         ;; Close *Completions* buffer when fully deactivating
         db'' (if still-active?
                db'
                (delete-completions-window db'))]
     {:db (cond-> db''
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
   "Update the minibuffer input text.
   Also resets cycling state when user types (not during arrow cycling).
   For file completion, also updates completions based on new input."
   (let [on-change (get-in db [:minibuffer :on-change])
         category (get-in db [:minibuffer :metadata :category])]
     (cond
       ;; Custom on-change handler exists (e.g., for isearch)
       on-change
       {:fx [[:dispatch (conj on-change input-text)]]}

       ;; File completion - update completions dynamically
       (= category :file)
       {:db (-> db
                (assoc-in [:minibuffer :input] input-text)
                (assoc-in [:minibuffer :cycling?] false)
                (assoc-in [:minibuffer :completion-index] -1)
                (assoc-in [:minibuffer :original-input] ""))
        :fx [[:dispatch [:find-file/update-completions input-text]]]}

       ;; Standard minibuffer - update input and reset cycling state
       :else
       {:db (-> db
                (assoc-in [:minibuffer :input] input-text)
                ;; Reset cycling state when user types
                (assoc-in [:minibuffer :cycling?] false)
                (assoc-in [:minibuffer :completion-index] -1)
                (assoc-in [:minibuffer :original-input] ""))}))))

(rf/reg-event-fx
 :minibuffer/complete
 (fn [{:keys [db]} [_]]
   "TAB completion in minibuffer - vanilla Emacs behavior.

   First TAB: Complete as far as possible (common prefix).
   Second TAB: Show *Completions* buffer with all matching candidates.

   For file completion, TAB on a complete directory path (ending with /)
   descends into that directory and shows its children. If the subdirectory
   isn't cached yet, triggers async caching.

   This matches Emacs minibuffer-complete behavior:
   - TAB once completes common prefix, no UI change if ambiguous
   - TAB twice opens *Completions* buffer

   See Issue #136 for details."
   (let [input (get-in db [:minibuffer :input] "")
         category (get-in db [:minibuffer :completion-metadata :category])
         last-tab-input (get-in db [:minibuffer :last-tab-input])
         ;; For file completion, check if we need to descend into a directory
         is-file-completion? (= category :file)
         input-is-directory? (and is-file-completion?
                                  (clojure.string/ends-with? input "/"))
         ;; Check if directory is cached (for file completion)
         dir-cache (get-in db [:fs-access :directory-cache] {})
         granted-dirs (get-in db [:fs-access :granted-directories] {})
         ;; For directory input, check if we need to trigger caching
         cache-key (when input-is-directory?
                     ;; Remove trailing slash for cache lookup
                     (subs input 0 (dec (count input))))
         dir-is-cached? (or (not input-is-directory?)
                            (contains? dir-cache cache-key))
         ;; Find the grant that contains this path (for triggering cache)
         matching-grant (when (and input-is-directory? (not dir-is-cached?))
                          (first
                           (keep (fn [[grant-path {:keys [handle]}]]
                                   (when (clojure.string/starts-with? input grant-path)
                                     {:grant-path grant-path
                                      :handle handle}))
                                 granted-dirs)))
         ;; For file completion with directory input, refresh completions first
         db' (if input-is-directory?
               ;; Update completions for this directory
               (let [new-completions (buffer-events/file-completions-for-input
                                      dir-cache granted-dirs input)]
                 (assoc-in db [:minibuffer :completions] (vec new-completions)))
               db)
         completions (get-in db' [:minibuffer :completions] [])
         ;; Get effective styles for current completion
         styles (or (get-in db' [:completion :styles]) [:basic :substring :flex])
         ;; Filter completions by input (standard prefix/substring matching)
         matches (if (clojure.string/blank? input)
                   completions
                   (completion-styles/filter-candidates
                    input
                    completions
                    :styles-list styles))
         ;; Detect consecutive TAB press (input unchanged since last TAB)
         consecutive-tab? (= input last-tab-input)]
     (cond
       ;; Directory not cached - trigger async caching
       (and input-is-directory? (not dir-is-cached?) matching-grant)
       {:db (assoc-in db' [:minibuffer :last-tab-input] input)
        :fx [[:fs-access/list-subdirectory-for-cache
              {:grant-handle (:handle matching-grant)
               :grant-path (:grant-path matching-grant)
               :target-path cache-key}]
             [:dispatch [:message/display "Loading directory..."]]]}

       ;; No completions available
       (empty? completions)
       {:db db'}

       ;; Consecutive TAB - show *Completions* buffer
       consecutive-tab?
       {:db (assoc-in db' [:minibuffer :last-tab-input] nil)  ; Reset for next cycle
        :fx [[:dispatch [:minibuffer/completion-help matches]]]}

       ;; Single match - complete it fully
       (= (count matches) 1)
       {:db (-> db'
                (assoc-in [:minibuffer :input] (first matches))
                (assoc-in [:minibuffer :last-tab-input] (first matches))
                (assoc-in [:minibuffer :show-completions?] false)
                (assoc-in [:minibuffer :filtered-completions] [])
                (assoc-in [:minibuffer :height-lines] 1))}

       ;; Multiple matches - complete common prefix only (first TAB)
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
             new-input (if (> (count common-prefix) (count input))
                         common-prefix
                         input)]
         {:db (-> db'
                  (assoc-in [:minibuffer :input] new-input)
                  (assoc-in [:minibuffer :last-tab-input] new-input)
                  ;; Don't show inline completions on first TAB (vanilla Emacs)
                  (assoc-in [:minibuffer :show-completions?] false)
                  (assoc-in [:minibuffer :filtered-completions] [])
                  (assoc-in [:minibuffer :height-lines] 1))})

       ;; No matches
       :else
       {:db (assoc-in db' [:minibuffer :last-tab-input] input)}))))

(rf/reg-event-fx
 :minibuffer/confirm
 (fn [{:keys [db]} [_]]
   "Confirm minibuffer input.
   IMPORTANT: Does NOT auto-deactivate anymore! Commands must deactivate explicitly
   or use :replace? true to take over the minibuffer slot. This fixes Issue #72."
   ;; Read directly from :minibuffer map (the single source of truth)
   (let [input (get-in db [:minibuffer :input] "")
         on-confirm (get-in db [:minibuffer :on-confirm])
         persist? (get-in db [:minibuffer :persist?])]
     (if on-confirm
       (if persist?
         ;; Multi-step prompt - dispatch without deactivating
         {:fx [[:dispatch (conj on-confirm input)]]}
         ;; Single-step prompt - dispatch then deactivate
         {:fx [[:dispatch (conj on-confirm input)]
               [:dispatch [:minibuffer/deactivate]]]})
       ;; No handler - just deactivate
       {:fx [[:dispatch [:minibuffer/deactivate]]]}))))

;; Completion navigation events
(rf/reg-event-db
 :minibuffer/completion-next
 (fn [db [_]]
   "Move to next completion candidate (Arrow Down)"
   (let [completion-index (get-in db [:minibuffer :completion-index] 0)
         completions (get-in db [:minibuffer :filtered-completions] [])
         num-completions (count completions)
         new-index (if (< completion-index (dec num-completions))
                     (inc completion-index)
                     0)]
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
                     (dec num-completions))]
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
;; Completion Buffer Formatting (Issue #137)
;; =============================================================================

(defn- format-completions-buffer
  "Format completions for display in *Completions* buffer.
   Shows one candidate per line with a marker for the selected item.
   If selected-index is nil or -1, no item is highlighted.
   If annotation-fn is provided, appends annotations to each line.
   If strip-prefix is provided, strips it from display (but not value)."
  ([candidates] (format-completions-buffer candidates -1 nil nil))
  ([candidates selected-index] (format-completions-buffer candidates selected-index nil nil))
  ([candidates selected-index annotation-fn] (format-completions-buffer candidates selected-index annotation-fn nil))
  ([candidates selected-index annotation-fn strip-prefix]
   (if (empty? candidates)
     "No completions available."
     (let [;; For display, optionally strip common prefix (e.g., directory path)
           display-candidates (if strip-prefix
                                (map #(if (clojure.string/starts-with? % strip-prefix)
                                        (subs % (count strip-prefix))
                                        %)
                                     candidates)
                                candidates)
           ;; Find max display length for alignment
           max-len (apply max (map count display-candidates))
           ;; Pad to align annotations
           pad-to (+ max-len 2)
           format-line (fn [idx [_cand display-cand]]
                         (let [prefix (if (= idx selected-index) "> " "  ")
                               ;; Pad candidate for annotation alignment
                               padded (str display-cand (apply str (repeat (- pad-to (count display-cand)) " ")))
                               ;; Get annotation if function provided (using original candidate)
                               annotation (when annotation-fn
                                            (try
                                              (annotation-fn _cand)
                                              (catch :default _ nil)))]
                           (str prefix padded (or annotation ""))))]
       (str "Possible completions:\n"
            (clojure.string/join "\n"
                                 (map-indexed format-line (map vector candidates display-candidates))))))))

;; =============================================================================
;; Arrow Key Cycling - Vanilla Emacs Style (Issue #137)
;; =============================================================================
;; These handlers implement icomplete-style cycling through completions.
;; Up/Down arrows cycle through matching completions, inserting the current
;; selection into the minibuffer input. This provides visual feedback as
;; users navigate without requiring a separate *Completions* buffer.
;;
;; NOTE: The *Completions* buffer is STATIC - it does NOT update when cycling.
;; This matches Emacs behavior. The buffer shows the initial set of completions
;; and users navigate using the minibuffer display, not by watching *Completions*.

(rf/reg-event-db
 :minibuffer/cycle-next
 (fn [db [_]]
   "Cycle to next completion candidate (Down arrow).

   - First press: saves current input as original-input, starts cycling
   - Subsequent: moves to next completion (wraps around)
   - Updates minibuffer input to show selected completion
   - Does NOT update *Completions* buffer (Emacs behavior: buffer is static)"
   (let [input (get-in db [:minibuffer :input] "")
         completions (get-in db [:minibuffer :completions] [])
         ;; Filter completions by current input (case-insensitive substring match)
         matches (if (clojure.string/blank? input)
                   completions
                   (filter #(clojure.string/includes?
                             (clojure.string/lower-case %)
                             (clojure.string/lower-case input))
                           completions))
         matches-vec (vec matches)
         num-matches (count matches-vec)]
     (if (zero? num-matches)
       db  ; No matches to cycle through
       (let [cycling? (get-in db [:minibuffer :cycling?] false)
             original-input (if cycling?
                              (get-in db [:minibuffer :original-input] "")
                              input)
             current-index (get-in db [:minibuffer :completion-index] -1)
             ;; Next index: -1 -> 0, then increment with wrap
             new-index (if (< current-index (dec num-matches))
                         (inc current-index)
                         0)
             selected (nth matches-vec new-index)]
         (-> db
             (assoc-in [:minibuffer :cycling?] true)
             (assoc-in [:minibuffer :original-input] original-input)
             (assoc-in [:minibuffer :completion-index] new-index)
             (assoc-in [:minibuffer :input] selected)))))))

(rf/reg-event-db
 :minibuffer/cycle-prev
 (fn [db [_]]
   "Cycle to previous completion candidate (Up arrow).

   - First press: saves current input as original-input, starts cycling
   - When at index 0 and pressing up: returns to original-input (index -1)
   - Otherwise: moves to previous completion (wraps around)
   - Updates minibuffer input to show selected completion"
   (let [input (get-in db [:minibuffer :input] "")
         completions (get-in db [:minibuffer :completions] [])
         ;; Filter completions by current input (case-insensitive substring match)
         matches (if (clojure.string/blank? input)
                   completions
                   (filter #(clojure.string/includes?
                             (clojure.string/lower-case %)
                             (clojure.string/lower-case input))
                           completions))
         matches-vec (vec matches)
         num-matches (count matches-vec)]
     (if (zero? num-matches)
       db  ; No matches to cycle through
       (let [cycling? (get-in db [:minibuffer :cycling?] false)
             original-input (if cycling?
                              (get-in db [:minibuffer :original-input] "")
                              input)
             current-index (get-in db [:minibuffer :completion-index] -1)]
         (cond
           ;; Not yet cycling, Up goes to last match
           (not cycling?)
           (let [new-index (dec num-matches)
                 selected (nth matches-vec new-index)]
             (-> db
                 (assoc-in [:minibuffer :cycling?] true)
                 (assoc-in [:minibuffer :original-input] original-input)
                 (assoc-in [:minibuffer :completion-index] new-index)
                 (assoc-in [:minibuffer :input] selected)))

           ;; At index 0, go back to original input
           (= current-index 0)
           (-> db
               (assoc-in [:minibuffer :cycling?] false)
               (assoc-in [:minibuffer :completion-index] -1)
               (assoc-in [:minibuffer :input] original-input))

           ;; At index -1 (showing original), wrap to last match
           (= current-index -1)
           (let [new-index (dec num-matches)
                 selected (nth matches-vec new-index)]
             (-> db
                 (assoc-in [:minibuffer :cycling?] true)
                 (assoc-in [:minibuffer :completion-index] new-index)
                 (assoc-in [:minibuffer :input] selected)))

           ;; Otherwise, decrement index
           :else
           (let [new-index (dec current-index)
                 selected (nth matches-vec new-index)]
             (-> db
                 (assoc-in [:minibuffer :completion-index] new-index)
                 (assoc-in [:minibuffer :input] selected)))))))))

;; =============================================================================
;; Completion Help - *Completions* Buffer (Issue #136)
;; =============================================================================

(def ^:private completion-list-mode-map
  "Keymap for completion-list-mode. Extends special-mode-map.
   Defined here to avoid circular dependencies with modes namespace."
  (merge special-mode/special-mode-map
         {:RET   [:completion-list/choose-at-point]
          :n     [:completion-list/next-completion]
          :p     [:completion-list/previous-completion]
          :down  [:completion-list/next-completion]
          :up    [:completion-list/previous-completion]
          :q     [:completion-list/quit]}))

(defn- get-or-create-completions-buffer
  "Get existing *Completions* buffer or create new one.
   Returns [db buffer-id]."
  [db]
  (let [buffers (:buffers db)
        existing (some (fn [[id buf]]
                         (when (= (:name buf) "*Completions*") id))
                       buffers)]
    (if existing
      [db existing]
      ;; Create new buffer
      (let [WasmGapBuffer (get-in db [:system :wasm-constructor])
            wasm-instance (when WasmGapBuffer (new WasmGapBuffer ""))
            buffer-id (db/next-buffer-id buffers)
            new-buffer (-> (db/create-buffer buffer-id "*Completions*" wasm-instance)
                           (assoc :is-read-only? true)
                           (assoc :major-mode :completion-list-mode)
                           (assoc :mode-keymap completion-list-mode-map)
                           ;; Enable hl-line-mode to highlight current selection
                           (assoc :minor-modes #{:hl-line-mode}))]
        [(assoc-in db [:buffers buffer-id] new-buffer) buffer-id]))))

(defn- find-completions-window
  "Find window showing *Completions* buffer, if any."
  [db buffer-id]
  (let [window-tree (:window-tree db)]
    (letfn [(find-in-tree [tree]
              (cond
                (nil? tree) nil
                (= (:type tree) :leaf)
                (when (= (:buffer-id tree) buffer-id)
                  (:id tree))
                (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                (or (find-in-tree (:first tree))
                    (find-in-tree (:second tree)))
                :else nil))]
      (find-in-tree window-tree))))

(defn- create-bottom-split-for-buffer
  "Create a horizontal split at the ROOT level with buffer-id in the bottom window.
   Returns updated db with new window tree.

   IMPORTANT: Always creates the split at the root (frame level), not within
   existing splits. This ensures *Completions* is always a single window at
   the bottom of the frame, not nested within other splits.

   The bottom window has a fixed height of ~6 lines for *Completions*."
  [db buffer-id]
  (let [window-tree (:window-tree db)
        new-window-id (db/next-window-id db)
        ;; Create new window for completions with fixed height
        ;; 6 lines * 20px line-height + 24px mode-line = ~144px
        new-window (-> (db/create-leaf-window new-window-id buffer-id)
                       (assoc :viewport {:start-line 0 :end-line 6})
                       (assoc :fixed-height 168))  ; 6 lines + mode-line + padding
        ;; Create split at ROOT level - entire existing tree goes on top
        split-id (inc new-window-id)
        new-tree {:type :hsplit
                  :id split-id
                  :first window-tree      ; Entire existing tree on top
                  :second new-window}]    ; *Completions* at bottom
    (-> db
        (assoc :window-tree new-tree)
        (assoc :next-window-id (+ new-window-id 2))
        ;; Store the completions window id for later use
        (assoc-in [:completion-help :window-id] new-window-id))))

(rf/reg-event-fx
 :minibuffer/completion-help
 (fn [{:keys [db]} [_ candidates]]
   "Display *Completions* buffer with all matching candidates.

   This is triggered by pressing TAB twice in the minibuffer (vanilla Emacs behavior).
   Creates or updates the *Completions* buffer and displays it in a bottom split.
   Focus remains on the minibuffer.

   For file completion, strips the directory prefix from display (it's already
   visible in the minibuffer input, so showing it again is redundant).

   See Issue #136, #137."
   (let [;; Get or create *Completions* buffer
         [db' buffer-id] (get-or-create-completions-buffer db)
         ;; Get annotation function from minibuffer state
         annotation-fn (get-in db [:minibuffer :annotation-fn])
         ;; For file completion, strip directory prefix from display
         category (get-in db [:minibuffer :completion-metadata :category])
         input (get-in db [:minibuffer :input] "")
         strip-prefix (when (= category :file)
                        ;; Get directory part of input (up to and including last /)
                        (let [last-slash (clojure.string/last-index-of input "/")]
                          (when last-slash
                            (subs input 0 (inc last-slash)))))
         ;; Format content with annotations and optional prefix stripping
         content (format-completions-buffer candidates -1 annotation-fn strip-prefix)
         ;; Get WASM instance and update content
         ^js wasm-instance (get-in db' [:buffers buffer-id :wasm-instance])]
     (when wasm-instance
       ;; Clear existing content and insert new
       (let [current-length (.-length wasm-instance)]
         (when (pos? current-length)
           (.delete wasm-instance 0 current-length))
         (.insert wasm-instance 0 content)))
     ;; Check if *Completions* window already exists
     (let [existing-window (find-completions-window db' buffer-id)
           db'' (-> db'
                    (assoc-in [:buffers buffer-id :cache :text] content)
                    (assoc-in [:buffers buffer-id :cache :line-count]
                              (count (clojure.string/split content #"\n" -1)))
                    (assoc-in [:completion-help :candidates] (vec candidates))
                    (assoc-in [:completion-help :buffer-id] buffer-id))]
       (if existing-window
         ;; *Completions* window exists, just update content (already done above)
         {:db db''
          :fx [[:dom/focus-minibuffer nil]]}
         ;; Create bottom split for *Completions*
         {:db (create-bottom-split-for-buffer db'' buffer-id)
          :fx [[:dom/focus-minibuffer nil]]})))))

(defn- delete-completions-window
  "Delete the *Completions* window from the window tree.
   Returns updated db with window removed."
  [db]
  (let [buffer-id (get-in db [:completion-help :buffer-id])
        window-id (when buffer-id (find-completions-window db buffer-id))]
    (if window-id
      ;; Delete the window from tree
      (let [window-tree (:window-tree db)]
        (letfn [(remove-from-tree [tree]
                  (cond
                    (nil? tree) nil
                    ;; Leaf node - can't be the target since we're looking for splits
                    (= (:type tree) :leaf) tree
                    ;; Split node - check if either child is the completions window
                    (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                    (cond
                      ;; First child is the completions window - replace split with second
                      (and (= (:type (:first tree)) :leaf)
                           (= (:id (:first tree)) window-id))
                      (:second tree)
                      ;; Second child is the completions window - replace split with first
                      (and (= (:type (:second tree)) :leaf)
                           (= (:id (:second tree)) window-id))
                      (:first tree)
                      ;; Recurse
                      :else
                      (assoc tree
                             :first (remove-from-tree (:first tree))
                             :second (remove-from-tree (:second tree))))
                    :else tree))]
          (-> db
              (assoc :window-tree (remove-from-tree window-tree))
              (assoc-in [:completion-help :window-id] nil))))
      ;; No completions window to delete
      db)))

(rf/reg-event-fx
 :completion-list/choose
 (fn [{:keys [db]} [_ candidate]]
   "Choose a completion from *Completions* buffer.
   Inserts the candidate into minibuffer and closes *Completions*."
   (if (minibuffer/minibuffer-active? db)
     {:db (-> db
              (assoc-in [:minibuffer :input] candidate)
              (assoc-in [:minibuffer :last-tab-input] nil)
              delete-completions-window)
      :fx [[:dom/focus-minibuffer nil]]}
     {:db db})))

(rf/reg-event-fx
 :completion-list/quit
 (fn [{:keys [db]} [_]]
   "Quit *Completions* buffer and return to minibuffer."
   {:db (delete-completions-window db)
    :fx [[:dom/focus-minibuffer nil]]}))

(rf/reg-event-fx
 :focus-minibuffer
 (fn [_ [_]]
   "Focus the minibuffer input element."
   {:fx [[:dom/focus-minibuffer nil]]}))

(rf/reg-event-fx
 :completion-list/choose-at-point
 (fn [{:keys [db]} [_]]
   "Choose the completion at or near point in *Completions* buffer.

   Looks for the completion candidate on the current line and
   inserts it into the minibuffer."
   (let [;; Get the candidates stored when *Completions* was created
         candidates (get-in db [:completion-help :candidates] [])
         buffer-id (get-in db [:completion-help :buffer-id])
         ;; Get current cursor position in *Completions* buffer
         active-window-id (:active-window-id db)
         active-window (db/find-window-in-tree (:window-tree db) active-window-id)
         cursor-line (get-in active-window [:cursor-position :line] 0)
         ;; Get buffer content to find candidate at point
         buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         buffer-text (when wasm-instance (.getText wasm-instance))
         lines (when buffer-text (clojure.string/split buffer-text #"\n"))
         ;; Skip header lines (first 3 lines are header text)
         content-line-index (- cursor-line 3)
         current-line (when (and lines
                                  (>= content-line-index 0)
                                  (< cursor-line (count lines)))
                        (nth lines cursor-line ""))]
     ;; Parse candidate from current line - find matching candidate
     (if-let [candidate (when current-line
                          (first (filter #(and (not (clojure.string/blank? %))
                                               (clojure.string/includes?
                                                (clojure.string/trim current-line)
                                                %))
                                         candidates)))]
       {:fx [[:dispatch [:completion-list/choose candidate]]]}
       {:fx [[:dispatch [:echo/message "No completion at point"]]]}))))

(rf/reg-event-fx
 :completion-list/next-completion
 (fn [_ [_]]
   "Move to next completion in *Completions* buffer."
   {:fx [[:dispatch [:editor/next-line]]]}))

(rf/reg-event-fx
 :completion-list/previous-completion
 (fn [_ [_]]
   "Move to previous completion in *Completions* buffer."
   {:fx [[:dispatch [:editor/previous-line]]]}))

(rf/reg-event-fx
 :completion-list/click-line
 (fn [{:keys [db]} [_ clicked-line]]
   "Handle click on a line in *Completions* buffer.
   Parses the clicked line and selects the completion.
   Issue #137: Clickable completion entries."
   (let [candidates (get-in db [:completion-help :candidates] [])
         ;; Line 0 is header 'Possible completions:', completion entries start at line 1
         ;; Each line has format '  candidate' or '> candidate' (selected)
         completion-index (dec clicked-line)]  ; Line 1 = index 0
     (if (and (>= completion-index 0) (< completion-index (count candidates)))
       (let [candidate (nth candidates completion-index)]
         {:fx [[:dispatch [:completion-list/choose candidate]]]})
       ;; Clicked on header or outside range
       {:db db}))))

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
