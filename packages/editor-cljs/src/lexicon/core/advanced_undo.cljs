(ns lexicon.core.advanced-undo
  "Advanced undo system with boundaries and change groups.

  Extends basic undo with:
  - Undo boundaries: Mark logical breaks in undo history
  - Change groups: Group multiple edits into single undo unit
  - Selective undo: Undo specific change groups
  - Undo recording control: Temporarily disable undo recording

  Undo stack structure:
  - Each entry: {:type :edit/:boundary/:group-start/:group-end ...}
  - Edits contain: {:type :edit :action :insert/:delete :pos N :text 'foo'}
  - Boundaries: {:type :boundary}
  - Group markers: {:type :group-start :id UUID} {:type :group-end :id UUID}

  Usage:
    (undo-boundary!)  ; Insert boundary in undo stack
    (with-undo-group  ; Group multiple edits
      (insert-text ...)
      (delete-text ...))
    (undo!)  ; Undo to previous boundary"
  (:require [re-frame.core :as rf]
            [lexicon.core.api.message]
            [lexicon.core.log :as log]))

;; -- Undo Stack Management --

(defn push-undo-entry!
  "Push ENTRY onto undo stack for BUFFER-ID.

  Entry types:
  - {:type :edit :op :insert/:delete-range/:replace ...}
  - {:type :boundary}
  - {:type :group-start :id UUID}
  - {:type :group-end :id UUID}
  - {:type :marker :marker-id ID :old-pos N :new-pos M}"
  [buffer-id entry]
  (rf/dispatch [:undo/push buffer-id entry]))

(rf/reg-event-db
  :undo/push
  (fn [db [_ buffer-id entry]]
    ;; Check if undo recording is enabled
    (let [recording-enabled? (get-in db [:undo :recording-enabled?] true)]
      (if recording-enabled?
        (update-in db [:buffers buffer-id :undo-stack]
                   (fn [stack]
                     (conj (or stack []) entry)))
        db))))

;; -- Undo Boundaries --

(defn undo-boundary!
  "Insert an undo boundary in BUFFER-ID's undo stack.

  Boundaries mark logical breaks in undo history.
  Undo command undoes all edits back to previous boundary."
  [buffer-id]
  (rf/dispatch-sync [:undo/boundary buffer-id]))

(rf/reg-event-db
  :undo/boundary
  (fn [db [_ buffer-id]]
    (let [stack (get-in db [:buffers buffer-id :undo-stack] [])
          ;; Don't add boundary if stack is empty or last entry is boundary
          last-entry (peek stack)
          should-add? (and (seq stack)
                          (not= (:type last-entry) :boundary))]
      (if should-add?
        (update-in db [:buffers buffer-id :undo-stack]
                   conj {:type :boundary})
        db))))

;; -- Change Groups --

(def current-undo-group
  "Atom holding current undo group ID or nil."
  (atom nil))

(defn begin-undo-group!
  "Start a new undo group in BUFFER-ID.

  All edits until end-undo-group! will be grouped together."
  [buffer-id]
  (let [group-id (random-uuid)]
    (reset! current-undo-group group-id)
    (push-undo-entry! buffer-id {:type :group-start :id group-id})
    group-id))

(defn end-undo-group!
  "End current undo group in BUFFER-ID."
  [buffer-id]
  (when-let [group-id @current-undo-group]
    (push-undo-entry! buffer-id {:type :group-end :id group-id})
    (reset! current-undo-group nil)
    group-id))

(defn with-undo-group*
  "Execute THUNK with undo grouping for BUFFER-ID.

  All edits within THUNK will be grouped as single undo unit."
  [buffer-id thunk]
  (begin-undo-group! buffer-id)
  (try
    (thunk)
    (finally
      (end-undo-group! buffer-id))))

;; Macro-like function for ClojureScript
(defn with-undo-group
  "Group edits into single undo unit.

  Usage:
    (with-undo-group buffer-id
      (fn []
        (insert-text ...)
        (delete-text ...)))"
  [buffer-id f]
  (with-undo-group* buffer-id f))

;; -- Undo Execution --

(defn skip-trailing-boundaries
  "Skip any trailing boundaries from end of stack.

  Returns: Index of first non-boundary from end (or index if no boundaries)"
  [stack index]
  (loop [i (dec index)]
    (cond
      (< i 0) 0
      (= (:type (nth stack i)) :boundary) (recur (dec i))
      :else (inc i))))  ; Return index AFTER the non-boundary

(defn find-previous-boundary
  "Find index of previous boundary, starting from INDEX and going backwards.

  Returns: Index of the boundary (or 0 if not found)"
  [stack index]
  (loop [i (dec index)]
    (cond
      (< i 0) 0
      (= (:type (nth stack i)) :boundary) i
      :else (recur (dec i)))))

(defn extract-undo-group
  "Extract entries between boundaries for undo.

  Steps:
  1. Skip any trailing boundaries
  2. Extract from after previous boundary to end
  3. Return entries and where to truncate stack

  Returns: {:entries [...] :boundary-index N}"
  [stack index]
  (let [;; Skip trailing boundaries to find actual edit operations
        end-index (skip-trailing-boundaries stack index)
        ;; Find the boundary before these operations
        boundary-idx (find-previous-boundary stack end-index)
        ;; Extract from after boundary to end
        start-index (if (pos? boundary-idx) (inc boundary-idx) 0)
        entries (subvec stack start-index end-index)]
    {:entries entries
     :boundary-index boundary-idx}))

(defn invert-undo-entry
  "Invert ENTRY for redo.

  :insert becomes :delete-range, :delete-range becomes :insert."
  [entry]
  (case (:type entry)
    :edit (case (:op entry)
           :insert (assoc entry :op :delete-range
                          :start (:position entry)
                          :length (count (:text entry)))
           :delete-range (assoc entry :op :insert
                                :position (:start entry)
                                :text (:text entry)))  ; Preserve the deleted text for re-insertion
    entry))

(defn apply-undo-entry
  "Apply ENTRY to BUFFER-STATE.

  Modifies buffer and returns updated state."
  [buffer-state entry]
  (case (:type entry)
    :edit (let [^js wasm (:wasm-instance buffer-state)
                op (:op entry)]
            (when wasm
              (try
                (case op
                  :insert (.insert wasm (:position entry) (:text entry))
                  :delete-range (.delete wasm (:start entry) (:length entry))
                  :replace (do
                             (.delete wasm (:start entry) (:length entry))
                             (.insert wasm (:start entry) (:text entry))))
                (catch js/Error e
                  (js/console.warn "Undo apply error:" e))))
            buffer-state)
    ;; Boundaries and group markers are metadata, no action needed
    buffer-state))

(defn undo-to-boundary!
  "Undo all entries back to previous boundary in BUFFER-ID."
  [buffer-id]
  (rf/dispatch [:undo/undo-to-boundary buffer-id]))

(rf/reg-event-fx
  :undo/undo-to-boundary
  (fn [{:keys [db]} [_ buffer-id]]
    (let [stack (get-in db [:buffers buffer-id :undo-stack] [])
          buffer-state (get-in db [:buffers buffer-id])]
      (if (seq stack)
        (let [{:keys [entries boundary-index]} (extract-undo-group stack (count stack))
              ;; Filter edit and marker entries
              undoable-entries (filter #(#{:edit :marker} (:type %)) entries)
              ;; Reverse order for undo
              reversed-entries (reverse undoable-entries)
              ;; Get point position for restoration - use FIRST marker from original order
              ;; (position BEFORE any changes), not last marker (position after all but last change)
              marker-entry (first (filter #(and (= (:type %) :marker)
                                                (= (:marker-id %) :point))
                                          entries))]

          ;; Apply undo entries (entries are already inverted, don't invert again!)
          (doseq [entry reversed-entries]
            (case (:type entry)
              :edit (apply-undo-entry buffer-state entry)
              :marker (when (= (:marker-id entry) :point)
                        ;; Restore point position
                        nil) ;; Will be handled via cursor update
              nil))

          ;; Update undo stack and redo stack, restore cursor
          (cond->
            {:db (-> db
                     (assoc-in [:buffers buffer-id :undo-stack]
                              (subvec stack 0 boundary-index))
                     (update-in [:buffers buffer-id :redo-stack]
                                (fn [redo]
                                  (conj (or redo []) {:entries entries :boundary-index boundary-index}))))}
            ;; Only add :fx when there's a cursor to restore
            marker-entry (assoc :fx [[:dispatch [:update-cursor-position (:old-pos marker-entry)]]])))
        {:db db}))))

;; -- Redo Support --

(defn redo!
  "Redo the last undone command in BUFFER-ID."
  [buffer-id]
  (rf/dispatch-sync [:undo/redo buffer-id]))

(rf/reg-event-fx
  :undo/redo
  (fn [{:keys [db]} [_ buffer-id]]
    (let [redo-stack (get-in db [:buffers buffer-id :redo-stack] [])
          buffer-state (get-in db [:buffers buffer-id])]
      (if (seq redo-stack)
        (let [redo-group (peek redo-stack)
              entries (:entries redo-group)
              ;; Filter edit and marker entries
              redoable-entries (filter #(#{:edit :marker} (:type %)) entries)
              ;; Apply in forward order (not reversed)
              marker-entry (last (filter #(= (:type %) :marker) redoable-entries))]

          ;; Apply redo entries
          (doseq [entry redoable-entries]
            (case (:type entry)
              :edit (apply-undo-entry buffer-state entry)
              :marker nil ;; Will be handled via cursor update
              nil))

          ;; Update stacks and restore cursor
          (cond->
            {:db (-> db
                     (update-in [:buffers buffer-id :redo-stack] pop)
                     (update-in [:buffers buffer-id :undo-stack]
                                (fn [stack]
                                  (into (or stack []) entries))))}
            ;; Only add :fx when there's a cursor to restore
            marker-entry (assoc :fx [[:dispatch [:update-cursor-position (:new-pos marker-entry)]]])))
        {:db db}))))

;; -- Clear Redo on New Edit --

(defn clear-redo-stack!
  "Clear redo stack for BUFFER-ID when a new edit is made."
  [buffer-id]
  (rf/dispatch [:undo/clear-redo buffer-id]))

(rf/reg-event-db
  :undo/clear-redo
  (fn [db [_ buffer-id]]
    (assoc-in db [:buffers buffer-id :redo-stack] [])))

;; -- Undo Recording Control --

(defn disable-undo-recording!
  "Temporarily disable undo recording.

  Useful during undo/redo to prevent recording undo operations."
  []
  (rf/dispatch [:undo/set-recording false]))

(defn enable-undo-recording!
  "Re-enable undo recording."
  []
  (rf/dispatch [:undo/set-recording true]))

(rf/reg-event-db
  :undo/set-recording
  (fn [db [_ enabled?]]
    (assoc-in db [:undo :recording-enabled?] enabled?)))

(defn with-undo-recording-disabled*
  "Execute THUNK with undo recording disabled."
  [thunk]
  (disable-undo-recording!)
  (try
    (thunk)
    (finally
      (enable-undo-recording!))))

;; -- Subscriptions --

(rf/reg-sub
  :undo/can-undo?
  (fn [db [_ buffer-id]]
    (seq (get-in db [:buffers buffer-id :undo-stack]))))

(rf/reg-sub
  :undo/can-redo?
  (fn [db [_ buffer-id]]
    (seq (get-in db [:buffers buffer-id :redo-stack]))))

(rf/reg-sub
  :undo/stack-size
  (fn [db [_ buffer-id]]
    (count (get-in db [:buffers buffer-id :undo-stack] []))))

(rf/reg-sub
  :undo/recording-enabled?
  (fn [db _]
    (get-in db [:undo :recording-enabled?] true)))

;; -- Initialization --

(defn initialize-undo!
  "Initialize advanced undo system."
  []
  (rf/dispatch-sync [:undo/initialize]))

(rf/reg-event-db
  :undo/initialize
  (fn [db [_]]
    (assoc-in db [:undo :recording-enabled?] true)))

;; Auto-initialize on namespace load
(initialize-undo!)

;; -- Integration with Basic Undo --

;; Enhanced undo command that respects boundaries
(rf/reg-event-db
  :undo-enhanced
  (fn [db [_]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])]
      (with-undo-recording-disabled*
        (fn []
          (undo-to-boundary! buffer-id)))
      db)))

;; -- Utilities --

(defn undo-list
  "Get undo stack for BUFFER-ID as list."
  [db buffer-id]
  (get-in db [:buffers buffer-id :undo-stack] []))

(defn undo-tree-size
  "Get total size of undo tree (stack + redo) for BUFFER-ID."
  [db buffer-id]
  (+ (count (get-in db [:buffers buffer-id :undo-stack] []))
     (count (get-in db [:buffers buffer-id :redo-stack] []))))

(defn clear-undo-tree!
  "Clear entire undo/redo history for BUFFER-ID."
  [buffer-id]
  (rf/dispatch [:undo/clear buffer-id]))

(rf/reg-event-db
  :undo/clear
  (fn [db [_ buffer-id]]
    (-> db
        (assoc-in [:buffers buffer-id :undo-stack] [])
        (assoc-in [:buffers buffer-id :redo-stack] []))))

;; -- Examples --

;; Example: Group multiple edits
(comment
  (with-undo-group buffer-id
    (fn []
      (push-undo-entry! buffer-id {:type :edit :action :insert :pos 0 :text "foo"})
      (push-undo-entry! buffer-id {:type :edit :action :insert :pos 3 :text "bar"}))))

;; Example: Manual boundary insertion
(comment
  (push-undo-entry! buffer-id {:type :edit :action :insert :pos 0 :text "a"})
  (undo-boundary! buffer-id)
  (push-undo-entry! buffer-id {:type :edit :action :insert :pos 1 :text "b"})
  ;; Undo will only undo "b", not "a"
  )
