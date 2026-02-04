(ns lexicon.core.events.buffer
  "Event handlers for buffer lifecycle, file I/O, and buffer switching.

  Handles:
  - Buffer creation and deletion
  - Buffer switching
  - File operations (open, save, write)
  - Buffer list management"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.core.db :as db]
            [lexicon.core.log :as log]
            [lexicon.core.api.message]
            [lexicon.core.completion.metadata :as completion-metadata]
            [lexicon.dired :as dired]))

;; -- Helper Functions --


(defn line-col-to-linear-pos
  "Convert line/column coordinates to linear position"
  [text line column]
  (let [lines (clojure.string/split text #"\n" -1)]  ; -1 keeps trailing empty strings
    (if (>= line (count lines))
      (count text)
      (let [lines-before (take line lines)
            chars-before-line (reduce + 0 (map count lines-before))
            newlines-before line ; One newline per line before current
            line-content (nth lines line "")
            safe-column (min column (count line-content))]
        (+ chars-before-line newlines-before safe-column)))))

(defn detect-language-from-filename
  "Detect language from file name/extension"
  [filename]
  (cond
    (re-matches #".*\.(js|jsx|mjs|cjs)$" filename) :javascript
    (re-matches #".*\.(ts|tsx)$" filename) :typescript
    (re-matches #".*\.(clj|cljs|cljc|edn)$" filename) :clojure
    (re-matches #".*\.(py)$" filename) :python
    (re-matches #".*\.(rs)$" filename) :rust
    (re-matches #".*\.(md|markdown)$" filename) :markdown
    :else :text))

;; -- Buffer Lifecycle Events --

(rf/reg-event-db
 :create-buffer
 (fn [db [_ name wasm-instance]]
   "Create a new buffer with the given name and WASM instance.
   Note: This event handler may not be called when buffer creation happens
   from inside another event handler due to dispatch-sync limitations.
   See lexicon.lisp/create-buffer which uses direct swap! instead."
   (let [buffer-id (db/next-buffer-id (:buffers db))
         new-buffer (db/create-buffer buffer-id name wasm-instance)]
     (assoc-in db [:buffers buffer-id] new-buffer))))

;; -- Buffer Metadata Events --

(rf/reg-event-db
 :buffer/set-mode
 (fn [db [_ buffer-id mode]]
   "Set the major mode for a buffer (canonical API for state ownership).

   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:buffers buffer-id :major-mode] mode)))

(rf/reg-event-db
 :buffer/increment-version
 (fn [db [_ buffer-id]]
   "Increment the editor version for a buffer (invalidates cache).

   This is the ONLY event that should update buffer :editor-version.
   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (update-in db [:buffers buffer-id :editor-version] inc)))

(rf/reg-event-db
 :buffer/set-undo-in-progress
 (fn [db [_ buffer-id value]]
   "Set the undo-in-progress flag for a buffer.

   This is the ONLY event that should update buffer :undo-in-progress?.
   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:buffers buffer-id :undo-in-progress?] value)))

(rf/reg-event-db
 :buffer/set-file
 (fn [db [_ buffer-id file-path]]
   "Associate a file path with a buffer.

   This is the ONLY event that should update buffer :file-handle.
   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:buffers buffer-id :file-handle] file-path)))

(rf/reg-event-db
 :buffer/set-point
 (fn [db [_ buffer-id position]]
   "Set point (cursor position) for a buffer.

   Point is buffer-local in Emacs semantics.
   This is the ONLY event that should update buffer :point.
   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:buffers buffer-id :point] position)))

(rf/reg-event-db
 :buffer/set-read-only
 (fn [db [_ buffer-id read-only?]]
   "Set read-only flag for a buffer.

   This is the ONLY event that should update buffer :is-read-only?.
   See .CLAUDE.md Architecture Principles: State Management & Ownership"
   (assoc-in db [:buffers buffer-id :is-read-only?] read-only?)))

(rf/reg-event-fx
 :read-only-mode
 (fn [{:keys [db]} [_]]
   "Toggle read-only mode for current buffer (C-x C-q).

   Emacs equivalent: read-only-mode (bound to C-x C-q)
   When buffer is read-only, user keyboard input is blocked.
   Programmatic access via inhibit-read-only can still modify."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         current-read-only? (:is-read-only? buffer false)
         new-read-only? (not current-read-only?)]
     {:db (assoc-in db [:buffers buffer-id :is-read-only?] new-read-only?)
      :fx [[:dispatch [:message (if new-read-only?
                                   "Buffer is now read-only"
                                   "Buffer is now writable")]]]})))

;; -- Phase 6.6: Buffer Primitives (Issue #100) --

(rf/reg-event-db
 :buffer/set-current
 (fn [db [_ buffer-id]]
   "Make buffer-id current WITHOUT displaying it.

   Unlike :switch-buffer which also updates the active window's display,
   this event only changes which buffer is considered 'current' for
   programmatic access. Used by set-buffer in Emacs Lisp.

   Also updates buffer access time for MRU ordering (other-buffer)."
   (-> db
       (assoc-in [:buffers buffer-id :last-accessed-time] (js/Date.now))
       (update :buffer-access-order
               (fn [order]
                 (vec (cons buffer-id (remove #(= % buffer-id) (or order [])))))))))

(rf/reg-event-db
 :buffer/narrow-to-region
 (fn [db [_ buffer-id start end]]
   "Restrict buffer view to [start, end).

   Emacs narrowing semantics:
   - BEGV (beginning of accessible region) set to start
   - ZV (end of accessible region) set to end
   - Point clamped to narrowed region
   - All editing operations respect narrowing"
   (let [begv (min start end)
         zv (max start end)
         current-point (get-in db [:buffers buffer-id :point] 0)
         clamped-point (-> current-point (max begv) (min zv))]
     (-> db
         (assoc-in [:buffers buffer-id :begv] begv)
         (assoc-in [:buffers buffer-id :zv] zv)
         (assoc-in [:buffers buffer-id :point] clamped-point)))))

(rf/reg-event-db
 :buffer/widen
 (fn [db [_ buffer-id]]
   "Remove narrowing restriction from buffer.

   Restores full buffer accessibility by clearing BEGV/ZV."
   (-> db
       (assoc-in [:buffers buffer-id :begv] nil)
       (assoc-in [:buffers buffer-id :zv] nil))))

(rf/reg-event-db
 :buffer/push-restriction
 (fn [db [_ buffer-id]]
   "Push current narrowing state onto stack (for save-restriction).

   Called at the start of save-restriction to preserve current BEGV/ZV."
   (let [begv (get-in db [:buffers buffer-id :begv])
         zv (get-in db [:buffers buffer-id :zv])]
     (update-in db [:buffers buffer-id :narrowing-stack]
                (fn [stack] (conj (or stack []) {:begv begv :zv zv}))))))

(rf/reg-event-db
 :buffer/pop-restriction
 (fn [db [_ buffer-id]]
   "Pop narrowing state from stack and restore it.

   Called at the end of save-restriction to restore previous BEGV/ZV."
   (let [stack (get-in db [:buffers buffer-id :narrowing-stack] [])
         prev-state (peek stack)
         new-stack (if (seq stack) (pop stack) [])]
     (-> db
         (assoc-in [:buffers buffer-id :narrowing-stack] new-stack)
         (assoc-in [:buffers buffer-id :begv] (:begv prev-state))
         (assoc-in [:buffers buffer-id :zv] (:zv prev-state))))))

(rf/reg-event-db
 :buffer/rename
 (fn [db [_ buffer-id new-name]]
   "Rename buffer to new-name.

   If a buffer with new-name already exists, returns db unchanged.
   Use rename-buffer with UNIQUE argument in Lisp to auto-generate unique name."
   (let [existing (db/find-buffer-by-name (:buffers db) new-name)]
     (if (and existing (not= (:id existing) buffer-id))
       db ; Name collision - return unchanged
       (assoc-in db [:buffers buffer-id :name] new-name)))))

(rf/reg-event-db
 :buffer/enable-undo
 (fn [db [_ buffer-id]]
   "Enable undo recording for buffer."
   (assoc-in db [:buffers buffer-id :undo-enabled?] true)))

(rf/reg-event-db
 :buffer/disable-undo
 (fn [db [_ buffer-id]]
   "Disable undo recording for buffer and clear undo stack."
   (-> db
       (assoc-in [:buffers buffer-id :undo-enabled?] false)
       (assoc-in [:buffers buffer-id :undo-stack] []))))

(rf/reg-event-db
 :buffer/make-local-variable
 (fn [db [_ buffer-id variable]]
   "Make VARIABLE buffer-local in BUFFER-ID.

   Copies current global value as initial local value if not already set."
   (let [global-value (get-in db [:global-vars variable])]
     (-> db
         (update-in [:buffers buffer-id :local-vars-set]
                    (fn [s] (conj (or s #{}) variable)))
         (update-in [:buffers buffer-id :local-vars variable]
                    (fn [v] (if (nil? v) global-value v)))))))

(rf/reg-event-db
 :buffer/setq
 (fn [db [_ buffer-id variable value]]
   "Set VARIABLE to VALUE, respecting buffer-local bindings.

   If VARIABLE is buffer-local in BUFFER-ID, sets the local value.
   Otherwise sets the global value."
   (let [is-local? (contains? (get-in db [:buffers buffer-id :local-vars-set]) variable)]
     (if is-local?
       (assoc-in db [:buffers buffer-id :local-vars variable] value)
       (assoc-in db [:global-vars variable] value)))))

(rf/reg-event-db
 :buffer/kill-all-local-variables
 (fn [db [_ buffer-id]]
   "Kill all buffer-local variable bindings in BUFFER-ID.

   Used by major mode switching to clear previous mode's locals."
   (-> db
       (assoc-in [:buffers buffer-id :local-vars] {})
       (assoc-in [:buffers buffer-id :local-vars-set] #{}))))

(rf/reg-event-fx
 :switch-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Switch to the specified buffer by updating the active window"
   (if (get-in db [:buffers buffer-id])
     (let [active-window-id (:active-window-id db)
           window-tree (:window-tree db)
           active-window (db/find-window-in-tree window-tree active-window-id)
           current-buffer-id (:buffer-id active-window)
           current-cursor (:cursor-position active-window)
           ;; Get target buffer's saved cursor position (or default to 0:0)
           target-cursor (get-in db [:buffers buffer-id :cursor-position] {:line 0 :column 0})
           ;; Save current cursor to current buffer
           db-with-saved-cursor (if current-buffer-id
                                  (assoc-in db [:buffers current-buffer-id :cursor-position] current-cursor)
                                  db)
           ;; Update window with new buffer and load its cursor
           new-tree (db/update-window-in-tree window-tree active-window-id
                                              #(assoc %
                                                      :buffer-id buffer-id
                                                      :cursor-position target-cursor))
           ;; Convert line/col to linear position for global state
           ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
           text (when wasm-instance (.getText wasm-instance))
           linear-pos (if text
                       (line-col-to-linear-pos text (:line target-cursor) (:column target-cursor))
                       0)]
       {:db (-> db-with-saved-cursor
                (assoc :window-tree new-tree))
        :fx [[:dispatch [:cursor/set-position linear-pos]]
             [:focus-editor]]})
     {:db db}))) ; Ignore if buffer doesn't exist

(rf/reg-event-fx
 :switch-to-buffer
 (fn [{:keys [db]} [_]]
   "Activate minibuffer for buffer switching (C-x b) with metadata"
   (let [buffers (:buffers db)
         buffer-names (sort (map :name (vals buffers)))
         current-buffer (get buffers (get-in db [:windows (:active-window-id db) :buffer-id]))
         current-name (:name current-buffer)
         prompt (str "Switch to buffer (default " current-name "): ")
         ;; Create metadata for buffer completion
         metadata (completion-metadata/make-metadata
                   :category :buffer
                   :annotation-function :buffer
                   :affixation-function :buffer
                   :display-sort-function :recent-first)]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt prompt
                        :completions buffer-names
                        :metadata metadata
                        :on-confirm [:switch-to-buffer-by-name]}]]]})))

(rf/reg-event-fx
 :switch-to-buffer-by-name
 (fn [{:keys [db]} [_ buffer-name]]
   "Switch to buffer by name, creating it if it doesn't exist"
   (let [buffers (:buffers db)
         current-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         current-buffer (get buffers current-buffer-id)
         ;; If input is empty, use current buffer name
         target-name (if (clojure.string/blank? buffer-name)
                      (:name current-buffer)
                      buffer-name)
         ;; Find buffer with matching name
         target-buffer (first (filter #(= (:name %) target-name) (vals buffers)))
         target-id (:id target-buffer)]
     (if target-id
       ;; Buffer exists - switch to it
       (do
         {:fx [[:dispatch [:switch-buffer target-id]]]})
       ;; Buffer doesn't exist - create it then switch to it
       (do
         (let [buffer-id (db/next-buffer-id buffers)
               WasmGapBuffer (get-in db [:system :wasm-constructor])
               wasm-instance (WasmGapBuffer. "")
               new-buffer {:id buffer-id
                          :wasm-instance wasm-instance
                          :file-handle nil
                          :name target-name
                          :is-modified? false
                          :mark-position nil
                          :cursor-position {:line 0 :column 0}
                          :selection-range nil
                          :major-mode :fundamental-mode
                          :minor-modes #{}
                          :buffer-local-vars {}
                          :ast nil
                          :language :text
                          :diagnostics []
                          :undo-stack []
                          :undo-in-progress? false
                          :editor-version 0
                          :cache {:text ""
                                  :line-count 1}}]
           {:db (assoc-in db [:buffers buffer-id] new-buffer)
            :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

(rf/reg-event-fx
 :kill-buffer
 (fn [{:keys [db]} [_]]
   "Activate minibuffer for buffer killing (C-x k)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         current-buffer-id (:buffer-id active-window)
         current-buffer (get (:buffers db) current-buffer-id)
         current-name (:name current-buffer)
         ;; Get all buffer names for completion
         buffer-names (map :name (vals (:buffers db)))
         prompt (str "Kill buffer (default " current-name "): ")]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt prompt
                        :completions (vec buffer-names)
                        :on-confirm [:kill-buffer-by-name]}]]]})))

(rf/reg-event-fx
 :kill-buffer-by-name
 (fn [{:keys [db]} [_ buffer-name]]
   "Kill buffer by name, or kill current if empty.

   Properly handles:
   - Switching to another buffer BEFORE killing if needed
   - Not killing the last buffer
   - Running kill-buffer hooks"
   (let [buffers (:buffers db)
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         current-buffer-id (:buffer-id active-window)
         current-buffer (get buffers current-buffer-id)
         ;; If input is empty, use current buffer name
         target-name (if (clojure.string/blank? buffer-name)
                       (:name current-buffer)
                       buffer-name)
         ;; Find buffer with matching name
         target-buffer (first (filter #(= (:name %) target-name) (vals buffers)))
         target-id (:id target-buffer)]
     (cond
       ;; Buffer not found
       (nil? target-id)
       {:db db
        :fx [[:dispatch [:echo/message (str "No buffer named " target-name)]]]}

       ;; Don't allow killing the last buffer
       (= (count buffers) 1)
       {:db db
        :fx [[:dispatch [:echo/message "Cannot kill the last buffer"]]]}

       :else
       (let [;; Run kill-buffer hooks
             hook-commands (get-in db [:hooks :kill-buffer-hook] [])
             ;; If killing active buffer, need to switch first
             need-switch? (= target-id current-buffer-id)
             ;; Find another buffer to switch to (prefer *scratch* if not being killed, else first available)
             other-buffer-id (or (some (fn [[id buf]] (when (and (= (:name buf) "*scratch*")
                                                                  (not= id target-id)) id)) buffers)
                                 (first (filter #(not= % target-id) (keys buffers))))
             ;; Build effect list
             effects (vec (concat
                           ;; Run hook commands first
                           (map (fn [cmd] [:dispatch [:execute-command cmd]]) hook-commands)
                           ;; Switch to another buffer if needed (BEFORE killing)
                           (when (and need-switch? other-buffer-id)
                             [[:dispatch [:switch-buffer other-buffer-id]]])
                           ;; Then remove the buffer
                           [[:dispatch [:kill-buffer-finalize target-id]]]))]
         {:db db
          :fx effects})))))

(rf/reg-event-db
 :kill-buffer-finalize
 (fn [db [_ buffer-id]]
   "Actually remove the buffer from db. Called after switch-buffer."
   (let [buffer-name (get-in db [:buffers buffer-id :name])]
     (js/console.log "Killing buffer:" buffer-name)
     (-> db
         (update :buffers dissoc buffer-id)
         ;; Remove from access order
         (update :buffer-access-order #(vec (remove #{buffer-id} %)))))))

(rf/reg-event-fx
 :list-buffers
 (fn [{:keys [db]} [_]]
   "Display list of all buffers (C-x C-b)"
   (let [buffers (:buffers db)
         ;; Check if *Buffer List* buffer already exists
         buffer-list-buffer (first (filter #(= (:name %) "*Buffer List*") (vals buffers)))]
     (if buffer-list-buffer
       ;; Just switch to existing buffer list
       {:fx [[:dispatch [:switch-buffer (:id buffer-list-buffer)]]]}
       ;; Create new buffer list buffer
       (let [buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             ;; Generate buffer list content
             buffer-lines (map (fn [buf]
                                 (str (if (:is-modified? buf) " *" "  ")
                                      " "
                                      (:name buf)
                                      "  "
                                      (or (:file-handle buf) "")))
                               (sort-by :id (vals buffers)))
             header "MR Buffer           File\n-- ------           ----\n"
             content (str header (clojure.string/join "\n" buffer-lines))
             lines (clojure.string/split content #"\n" -1)
             line-count (count lines)
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
             new-buffer {:id buffer-id
                         :wasm-instance wasm-instance
                         :file-handle nil
                         :name "*Buffer List*"
                         :is-modified? false
                         :mark-position nil
                         :cursor-position {:line 0 :column 0}
                         :selection-range nil
                         :major-mode :buffer-menu-mode
                         :minor-modes #{}
                         :buffer-local-vars {}
                         :ast nil
                         :language :text
                         :diagnostics []
                         :undo-stack []
                         :undo-in-progress? false
                         :editor-version 0
                         :cache {:text content
                                 :line-count line-count}}]
         (if-not wasm-instance
           ;; WASM not available - log error
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
           ;; Create buffer and then dispatch switch-buffer
           {:db (assoc-in db [:buffers buffer-id] new-buffer)
            :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

(rf/reg-event-fx
 :buffer-menu/select-buffer
 (fn [{:keys [db]} [_]]
   "Select buffer at current line in buffer-menu-mode (RET key)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get-in db [:buffers active-buffer-id])
         ;; Get cursor line from linear position (same as get-current-line-in-buffer-menu)
         cursor-pos (get-in db [:ui :cursor-position] 0)
         ^js wasm (get-in db [:buffers active-buffer-id :wasm-instance])
         text (when wasm (.getText wasm))
         cursor-line (if text
                       (count (filter #(= % \newline) (take cursor-pos text)))
                       0)]
     ;; Skip header lines (first 2 lines)
     (if (< cursor-line 2)
       {:fx [[:dispatch [:echo/message "No buffer on this line"]]]}
       (let [lines (when text (clojure.string/split text #"\n" -1))
             selected-line (nth lines cursor-line nil)
             ;; Extract buffer name from line (format: "MR Name  File")
             ;; MR is 2 chars (modified/readonly flags), then 1 space, then name
             ;; Buffer name starts at column 3 and ends before two consecutive spaces
             buffer-name (when (and selected-line (>= (count selected-line) 3))
                          (let [trimmed (subs selected-line 3) ; Skip "MR " prefix (3 chars)
                                name-part (first (clojure.string/split trimmed #"\s{2,}"))]
                            (clojure.string/trim name-part)))
             ;; Find the buffer with this name
             target-buffer (when buffer-name
                            (first (filter #(= (:name %) buffer-name) (vals (:buffers db)))))
             target-id (:id target-buffer)]
         (if target-id
           {:fx [[:dispatch [:switch-buffer target-id]]]}
           {:fx [[:dispatch [:echo/message (str "Buffer not found: " buffer-name)]]]}))))))

;; -- File I/O Events --

(rf/reg-event-fx
 :save-buffer
 (fn [{:keys [db]} [_]]
   "Save the active buffer to disk (C-x C-s)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
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

(rf/reg-event-fx
 :write-file
 (fn [{:keys [db]} [_]]
   "Save buffer to a new file (C-x C-w - Save As)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]

     (if wasm-instance
       (let [content (.getText ^js wasm-instance)]
         ;; Always prompt for save location (save as)
         {:fx [[:save-file-picker {:content content
                                   :buffer-id active-buffer-id}]]})
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

(rf/reg-event-fx
 :buffer-saved
 (fn [{:keys [db]} [_ {:keys [buffer-id file-handle]}]]
   "Mark buffer as saved and update file handle"
   (let [file-name (.-name file-handle)]
     {:db (-> db
              (assoc-in [:buffers buffer-id :is-modified?] false)
              (assoc-in [:buffers buffer-id :file-handle] file-handle)
              (assoc-in [:buffers buffer-id :name] file-name))
      :fx [[:dispatch [:lsp/on-buffer-saved buffer-id]]
           [:dispatch [:echo/message (str "Wrote " file-name)]]]})))

(defn- get-immediate-children
  "Get immediate children for a directory path from cache.
   Returns list of filenames/dirnames (with / suffix for directories)."
  [dir-cache dir-path]
  (when-let [entries (get dir-cache dir-path)]
    (map (fn [{:keys [name kind]}]
           (if (= kind "directory")
             (str name "/")
             name))
         entries)))

(defn file-completions-for-input
  "Compute file completions based on current input path.
   Returns full paths for completion values (so selection works correctly).
   Public so ui.cljs can call it for TAB directory descent."
  [dir-cache granted-dirs input]
  (let [;; Parse input into directory + partial filename
        last-slash (clojure.string/last-index-of input "/")
        [dir-path partial-name] (if last-slash
                                  [(subs input 0 (inc last-slash))
                                   (subs input (inc last-slash))]
                                  ["" input])
        ;; Normalize directory path (remove trailing slash for cache lookup)
        cache-key (if (and (seq dir-path) (clojure.string/ends-with? dir-path "/"))
                    (subs dir-path 0 (dec (count dir-path)))
                    dir-path)
        ;; Get immediate children
        children (get-immediate-children dir-cache cache-key)]
    (if children
      ;; Return full paths, filtered by partial name
      (->> children
           (filter #(or (clojure.string/blank? partial-name)
                        (clojure.string/starts-with? % partial-name)))
           (map #(str dir-path %)))  ; Full path for completion value
      ;; No cache for this directory - show granted roots
      (map (fn [[path _]] (str path "/")) granted-dirs))))

(rf/reg-event-fx
 :find-file
 (fn [{:keys [db]} [_]]
   "Open a file from disk.

   If File System Access API is supported and directories are granted,
   uses minibuffer with path completion. Otherwise falls back to
   browser file picker dialog.

   File completion shows ONLY immediate children of the current directory
   (Emacs behavior), not recursive full paths.

   See: #135 - File System Access API implementation"
   (let [api-supported? (get-in db [:fs-access :api-supported?] false)
         granted-dirs (get-in db [:fs-access :granted-directories] {})
         has-granted? (seq granted-dirs)
         dir-cache (get-in db [:fs-access :directory-cache] {})]
     (if (and api-supported? has-granted?)
       ;; Use minibuffer with file completion
       ;; Start with first granted directory as initial input
       (let [first-grant (first (keys granted-dirs))
             initial-input (str first-grant "/")
             ;; Get immediate children of initial directory
             completions (file-completions-for-input dir-cache granted-dirs initial-input)]
         {:fx [[:dispatch [:minibuffer/activate
                           {:prompt "Find file: "
                            :input initial-input
                            :completions (vec completions)
                            :metadata {:category :file}  ; Just the category - live state used for updates
                            :on-confirm [:find-file/from-path]}]]]})
       ;; Fall back to browser file picker
       {:fx [[:open-file-picker]]}))))

(rf/reg-event-fx
 :find-file/from-path
 (fn [{:keys [db]} [_ path]]
   "Open a file from a path entered in minibuffer.

   Resolves the path against granted directories and reads the file.
   Falls back to browser picker if path is not in a granted directory.

   If path is a directory (ends with /), opens dired instead of reading as file."
   (let [granted-dirs (get-in db [:fs-access :granted-directories] {})
         is-directory? (clojure.string/ends-with? path "/")]
     ;; Find which granted directory contains this path
     (if-let [matching-grant (first
                              (keep (fn [[grant-path {:keys [handle]}]]
                                      (when (clojure.string/starts-with? path grant-path)
                                        {:grant-path grant-path
                                         :handle handle
                                         :relative-path (subs path (count grant-path))}))
                                    granted-dirs))]
       (if is-directory?
         ;; Directory path - open dired
         {:fx [[:dispatch [:dired/open path]]]}
         ;; Read file from granted directory
         {:fx [[:fs-access/read-file-from-path
                {:grant-handle (:handle matching-grant)
                 :relative-path (:relative-path matching-grant)
                 :full-path path}]]})
       ;; Path not in granted directory - fall back to picker
       {:fx [[:dispatch [:message/display (str "Path not accessible: " path)]]
             [:open-file-picker]]}))))

(rf/reg-fx
 :dired/open-directory
 (fn [directory-path]
   "Effect to open a directory in Dired mode.

   Uses js/setTimeout to defer execution because dired/dired
   uses dispatch-sync internally via the lisp API, which cannot
   be called from within an event handler."
   (js/setTimeout #(dired/dired directory-path) 0)))

(rf/reg-event-fx
 :dired/open
 (fn [{:keys [db]} [_ directory-path]]
   "Open a directory in Dired mode.

   Called when find-file is given a directory path (ending with /).
   Ensures the directory is cached before opening dired.

   If directory is not cached, triggers caching first and defers dired
   until :dired/after-cache event is dispatched."
   (let [;; Normalize path for cache lookup (remove trailing slash)
         cache-key (if (clojure.string/ends-with? directory-path "/")
                     (subs directory-path 0 (dec (count directory-path)))
                     directory-path)
         dir-cache (get-in db [:fs-access :directory-cache] {})
         is-cached? (contains? dir-cache cache-key)
         granted-dirs (get-in db [:fs-access :granted-directories] {})]
     (if is-cached?
       ;; Directory already cached - open dired immediately
       {:db db
        :fx [[:dired/open-directory directory-path]]}
       ;; Need to cache first - find grant and trigger caching
       (if-let [matching-grant (first
                                (keep (fn [[grant-path {:keys [handle]}]]
                                        (when (clojure.string/starts-with? directory-path grant-path)
                                          {:grant-path grant-path :handle handle}))
                                      granted-dirs))]
         {:db (assoc-in db [:dired :pending-open] directory-path)
          :fx [[:fs-access/list-subdirectory-for-cache
                {:grant-handle (:handle matching-grant)
                 :grant-path (:grant-path matching-grant)
                 :target-path cache-key}]
               [:dispatch [:echo/message "Loading directory..."]]]}
         ;; Not in granted directory
         {:fx [[:dispatch [:echo/message (str "Directory not accessible: " directory-path)]]]})))))

(rf/reg-event-fx
 :dired/after-cache
 (fn [{:keys [db]} [_ _path]]
   "Called after directory is cached. Opens dired if there's a pending open."
   (if-let [pending-path (get-in db [:dired :pending-open])]
     {:db (update db :dired dissoc :pending-open)
      :fx [[:dired/open-directory pending-path]]}
     {:db db})))

(rf/reg-event-fx
 :dired/refresh-current
 (fn [{:keys [db]} [_ msg]]
   "Refresh current dired buffer and show message.
    Used as callback after file operations complete."
   (let [buffer-name (get-in db [:buffers
                                  (get-in db [:windows
                                              (db/find-window-in-tree
                                               (:window-tree db)
                                               (:active-window-id db))
                                              :buffer-id])
                                  :name] "")]
     ;; Only refresh if in a dired buffer
     (if (clojure.string/starts-with? buffer-name "*dired ")
       {:fx [[:dispatch [:dired/refresh-buffer buffer-name msg]]]}
       {:fx [[:dispatch [:echo/message (or msg "Operation complete")]]]}))))

(rf/reg-event-fx
 :dired/refresh-buffer
 (fn [{:keys [db]} [_ buffer-name msg]]
   "Refresh a specific dired buffer by re-listing the directory.
    Called after file operations complete."
   (let [;; Extract directory from buffer name: '*dired /path/to/dir*' -> '/path/to/dir'
         directory (when (and buffer-name (clojure.string/starts-with? buffer-name "*dired "))
                     (-> buffer-name
                         (clojure.string/replace #"^\*dired " "")
                         (clojure.string/replace #"\*$" "")))
         ;; Find granted directory that contains this path
         granted-dirs (get-in db [:fs-access :granted-directories] {})
         grant-info (first (keep (fn [[grant-path {:keys [handle]}]]
                                   (when (clojure.string/starts-with? (or directory "") grant-path)
                                     {:grant-path grant-path :handle handle}))
                                 granted-dirs))]
     (if grant-info
       {:fx [[:fs-access/list-directory-for-cache {:path directory
                                                    :handle (:handle grant-info)}]
             [:dispatch [:echo/message (or msg "Refreshed")]]]}
       {:fx [[:dispatch [:echo/message (or msg "Operation complete")]]]}))))

(rf/reg-event-fx
 :dired/operation-error
 (fn [_ [_ error-msg]]
   "Handle file operation errors."
   {:fx [[:dispatch [:echo/message (str "Error: " error-msg)]]]}))

(rf/reg-event-db
 :find-file/update-completions
 (fn [db [_ input]]
   "Update file completions based on current input path.
    Called when minibuffer input changes during file completion.
    Reads from live db state, not metadata snapshot."
   (let [;; Read from live state, not metadata snapshot
         dir-cache (get-in db [:fs-access :directory-cache] {})
         granted-dirs (get-in db [:fs-access :granted-directories] {})]
     (if (seq granted-dirs)
       (let [completions (file-completions-for-input dir-cache granted-dirs input)]
         (assoc-in db [:minibuffer :completions] (vec completions)))
       db))))

(rf/reg-fx
 :fs-access/read-file-from-path
 (fn [{:keys [grant-handle relative-path]}]
   "Read a file from a granted directory via relative path."
   (let [path-parts (->> (clojure.string/split
                          (clojure.string/replace relative-path #"^/" "")
                          #"/")
                         (filter seq)
                         vec)]
     (if (empty? path-parts)
       (rf/dispatch [:message/display "Invalid path"])
       ;; Navigate through directories to get file handle
       (letfn [(read-final-file [dir-handle filename]
                 (-> (.getFileHandle dir-handle filename)
                     (.then (fn [file-handle]
                              (-> (.getFile file-handle)
                                  (.then (fn [file]
                                           (.text file)))
                                  (.then (fn [content]
                                           (rf/dispatch
                                            [:file-read-success
                                             {:file-handle file-handle
                                              :content content
                                              :name filename}]))))))
                     (.catch (fn [err]
                               (rf/dispatch
                                [:message/display
                                 (str "Failed to read file: " (.-message err))])))))
               (resolve-path [handle remaining-parts]
                 (if (= 1 (count remaining-parts))
                   ;; Last part - read the file
                   (read-final-file handle (first remaining-parts))
                   ;; Navigate to subdirectory
                   (-> (.getDirectoryHandle handle (first remaining-parts))
                       (.then (fn [dir-handle]
                                (resolve-path dir-handle (rest remaining-parts))))
                       (.catch (fn [_err]
                                 (rf/dispatch
                                  [:message/display
                                   (str "Directory not found: " (first remaining-parts))]))))))]
         (resolve-path grant-handle path-parts))))))

(rf/reg-event-fx
 :insert-file
 (fn [{:keys [db]} [_]]
   "Insert contents of a file at point (C-x i)"
   {:db (assoc db :minibuffer {:active? true
                                 :prompt "Insert file: "
                                 :input ""
                                 :on-confirm [:insert-file/confirm]
                                 :on-cancel [:minibuffer/deactivate]})}))

(rf/reg-event-fx
 :insert-file/confirm
 (fn [{:keys [db]} [_]]
   "User confirmed insert-file in minibuffer - open file picker"
   {:fx [[:dispatch [:minibuffer/deactivate]]
         [:open-file-picker-for-insert]]}))

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
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (WasmGapBuffer. content)
         detected-language (detect-language-from-filename name)
         lines (clojure.string/split content #"\n" -1)
         line-count (count lines)
         ;; Issue #130: Detect major mode from filename for font-lock
         detected-mode (cond
                         (re-matches #".*\.(clj|cljs|cljc|edn)$" name) :clojure-mode
                         (re-matches #".*\.(js|jsx|mjs|cjs|ts|tsx)$" name) :javascript-mode
                         (re-matches #".*\.py$" name) :python-mode
                         (re-matches #".*\.rs$" name) :rust-mode
                         (re-matches #".*\.(html|htm)$" name) :html-mode
                         (re-matches #".*\.(css|scss|less)$" name) :css-mode
                         (re-matches #".*\.(md|markdown)$" name) :markdown-mode
                         :else :fundamental-mode)
         ;; Enable font-lock if global font-lock mode is on (default true)
         global-font-lock? (get-in db [:settings :global-font-lock-mode] true)
         new-buffer {:id buffer-id
                     :wasm-instance wasm-instance
                     :file-handle file-handle
                     :name name
                     :is-modified? false
                     :mark-position nil
                     :cursor-position {:line 0 :column 0}
                     :selection-range nil
                     :major-mode detected-mode
                     :minor-modes #{}
                     :buffer-local-vars {}
                     :ast nil
                     :language detected-language
                     :diagnostics []
                     :undo-stack []
                     :undo-in-progress? false
                     :editor-version 0
                     :font-lock-mode global-font-lock?  ; Issue #130: Enable font-lock
                     :text-properties {}                ; Issue #130: Initialize text properties
                     :cache {:text content
                             :line-count line-count}}]
       {:db (assoc-in db [:buffers buffer-id] new-buffer)
        :fx [[:dispatch [:switch-buffer buffer-id]]
             [:dispatch [:parser/request-parse buffer-id]]
             [:dispatch [:lsp/on-buffer-opened buffer-id]]
             ;; Issue #130: Trigger font-lock fontification after buffer is created
             (when global-font-lock?
               [:dispatch [:font-lock/fontify-buffer buffer-id]])]})))

(rf/reg-event-db
 :file-read-failure
 (fn [db [_ {:keys [error message]}]]
   "Handle file read failure"
   (println "File read failed:" message error)
   ;; Could add user notification here in the future
   db))

;; -- Insert File Events --

(rf/reg-fx
 :open-file-picker-for-insert
 (fn [_]
   "Handle file picker interaction for insert-file command"
   (-> (js/window.showOpenFilePicker)
       (.then (fn [file-handles]
                (let [file-handle (first file-handles)]
                  (-> (.getFile file-handle)
                      (.then (fn [file]
                               (-> (.text file)
                                   (.then (fn [content]
                                            (rf/dispatch [:file-insert-success
                                                         {:content content
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
 :file-insert-success
 (fn [{:keys [db]} [_ {:keys [content name]}]]
   "Handle successful file read for insert-file - insert at point"
   (let [current-pos (get-in db [:ui :cursor-position] 0)]
     (println "✓ Inserting file contents:" name "at position" current-pos)
     {:fx [[:dispatch [:editor/queue-transaction {:op :insert :text content}]]
           [:dispatch [:echo/message (str "Inserted " name)]]]})))
;; =============================================================================
;; Revert Buffer (M-x revert-buffer)
;; =============================================================================

(rf/reg-event-fx
 :revert-buffer
 (fn [{:keys [db]} [_]]
   "Revert buffer to its file on disk, discarding unsaved changes (M-x revert-buffer)"
   (let [active-window (lexicon.core.db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         file-handle (:file-handle buffer)
         buffer-name (:name buffer)
         is-modified? (:is-modified? buffer)]
     (cond
       ;; Buffer has no associated file
       (nil? file-handle)
       {:fx [[:dispatch [:echo/message (str "Buffer " buffer-name " is not visiting a file")]]]}

       ;; Buffer is modified, ask for confirmation
       is-modified?
       {:fx [[:dispatch [:minibuffer/activate
                        {:prompt (str "Revert buffer from file " buffer-name "? (yes or no) ")
                         :on-confirm [:revert-buffer/confirmed buffer-id file-handle]}]]]}

       ;; Buffer is unmodified, revert without asking
       :else
       {:fx [[:dispatch [:revert-buffer/do-revert buffer-id file-handle]]]}))))

(rf/reg-event-fx
 :revert-buffer/confirmed
 (fn [{:keys [db]} [_ buffer-id file-handle response]]
   "Handle user confirmation for revert-buffer"
   (if (or (= response "yes") (= response "y"))
     {:fx [[:dispatch [:revert-buffer/do-revert buffer-id file-handle]]]}
     {:fx [[:dispatch [:echo/message "Revert cancelled"]]]})))

(rf/reg-event-fx
 :revert-buffer/do-revert
 (fn [{:keys [db]} [_ buffer-id file-handle]]
   "Actually revert the buffer by re-reading the file"
   {:fx [[:revert-buffer-from-file {:buffer-id buffer-id :file-handle file-handle}]]}))

(rf/reg-fx
 :revert-buffer-from-file
 (fn [{:keys [buffer-id file-handle]}]
   "Read file from disk and replace buffer contents"
   (-> (.getFile file-handle)
       (.then (fn [file]
                (-> (.text file)
                    (.then (fn [content]
                             (rf/dispatch [:revert-buffer/success
                                          {:buffer-id buffer-id
                                           :content content
                                           :name (.-name file)}])))
                    (.catch (fn [error]
                              (rf/dispatch [:file-read-failure
                                           {:error error
                                            :message "Failed to read file for revert"}]))))))
       (.catch (fn [error]
                 (rf/dispatch [:file-read-failure
                              {:error error
                               :message "Failed to access file for revert"}]))))))

(rf/reg-event-fx
 :revert-buffer/success
 (fn [{:keys [db]} [_ {:keys [buffer-id content name]}]]
   "Replace buffer contents with file contents after revert"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         current-length (count (.getText wasm-instance))]
     (println "✓ Reverted buffer from file:" name)
     ;; Replace the entire buffer text
     (.delete wasm-instance 0 current-length)
     (.insert wasm-instance 0 content)
     ;; Update cache and clear modified flag
     (let [lines (clojure.string/split content #"\n" -1)
           line-count (count lines)]
       {:db (-> db
                (assoc-in [:buffers buffer-id :cache :text] content)
                (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                (assoc-in [:buffers buffer-id :is-modified?] false)
                (update-in [:buffers buffer-id :editor-version] inc))
        :fx [[:dispatch [:echo/message (str "Reverted buffer from file " name)]]]}))))
;; =============================================================================
;; Save Some Buffers (C-x s)
;; =============================================================================

(rf/reg-event-fx
 :save-some-buffers
 (fn [{:keys [db]} [_]]
   "Offer to save modified buffers (C-x s)"
   (let [buffers (:buffers db)
         ;; Find all modified buffers with file handles
         modified-buffers (filter (fn [[id buffer]]
                                    (and (:is-modified? buffer)
                                         (:file-handle buffer)))
                                  buffers)
         buffer-ids (map first modified-buffers)]
     (if (empty? buffer-ids)
       {:fx [[:dispatch [:echo/message "No buffers need saving"]]]}
       {:fx [[:dispatch [:save-some-buffers/next-buffer buffer-ids]]]}))))

(rf/reg-event-fx
 :save-some-buffers/next-buffer
 (fn [{:keys [db]} [_ buffer-ids]]
   "Process next buffer in save-some-buffers sequence"
   (if (empty? buffer-ids)
     ;; All done
     {:fx [[:dispatch [:echo/message "Done saving buffers"]]]}
     ;; Ask about next buffer
     (let [buffer-id (first buffer-ids)
           buffer (get-in db [:buffers buffer-id])
           buffer-name (:name buffer)]
       {:fx [[:dispatch [:minibuffer/activate
                        {:prompt (str "Save file " buffer-name "? (y or n) ")
                         :on-confirm [:save-some-buffers/confirmed buffer-ids]}]]]}))))

(rf/reg-event-fx
 :save-some-buffers/confirmed
 (fn [{:keys [db]} [_ buffer-ids response]]
   "Handle user response for save-some-buffers"
   (let [buffer-id (first buffer-ids)
         remaining-ids (rest buffer-ids)]
     (cond
       ;; User said yes, save this buffer
       (or (= response "y") (= response "yes"))
       (let [buffer (get-in db [:buffers buffer-id])
             wasm-instance (:wasm-instance buffer)
             file-handle (:file-handle buffer)
             content (.getText ^js wasm-instance)]
         {:fx [[:save-to-file-handle {:file-handle file-handle
                                      :content content
                                      :buffer-id buffer-id}]
               [:dispatch [:save-some-buffers/next-buffer remaining-ids]]]})

       ;; User said no, skip to next
       (or (= response "n") (= response "no"))
       {:fx [[:dispatch [:save-some-buffers/next-buffer remaining-ids]]]}

       ;; Invalid response, ask again
       :else
       {:fx [[:dispatch [:save-some-buffers/next-buffer buffer-ids]]]}))))
;; =============================================================================
;; Find Alternate File (C-x C-v)
;; =============================================================================

(rf/reg-event-fx
 :find-alternate-file
 (fn [{:keys [db]} [_]]
   "Kill current buffer and visit a different file (C-x C-v)"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         is-modified? (:is-modified? buffer)
         buffer-name (:name buffer)]
     (if is-modified?
       ;; Buffer is modified, ask for confirmation
       {:fx [[:dispatch [:minibuffer/activate
                        {:prompt (str "Buffer " buffer-name " modified; kill anyway? (yes or no) ")
                         :on-confirm [:find-alternate-file/confirmed buffer-id]}]]]}
       ;; Buffer is not modified, proceed directly
       {:fx [[:dispatch [:find-alternate-file/proceed buffer-id]]]}))))

(rf/reg-event-fx
 :find-alternate-file/confirmed
 (fn [{:keys [db]} [_ buffer-id response]]
   "Handle user confirmation for find-alternate-file"
   (if (or (= response "yes") (= response "y"))
     {:fx [[:dispatch [:find-alternate-file/proceed buffer-id]]]}
     {:fx [[:dispatch [:echo/message "Cancelled"]]]})))

(rf/reg-event-fx
 :find-alternate-file/proceed
 (fn [{:keys [db]} [_ buffer-id]]
   "Open file picker and kill current buffer after selection"
   ;; Store buffer-id to kill after file selection
   {:db (assoc-in db [:ui :find-alternate-file-buffer-id] buffer-id)
    :fx [[:open-file-picker-for-alternate]]}))

(rf/reg-fx
 :open-file-picker-for-alternate
 (fn [_]
   "Handle file picker for find-alternate-file"
   (-> (js/window.showOpenFilePicker)
       (.then (fn [file-handles]
                (let [file-handle (first file-handles)]
                  (-> (.getFile file-handle)
                      (.then (fn [file]
                               (-> (.text file)
                                   (.then (fn [content]
                                            (rf/dispatch [:find-alternate-file/success
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
 :find-alternate-file/success
 (fn [{:keys [db]} [_ {:keys [file-handle content name]}]]
   "Replace current buffer with newly opened file"
   (let [old-buffer-id (get-in db [:ui :find-alternate-file-buffer-id])
         old-buffer (get-in db [:buffers old-buffer-id])
         old-wasm-instance (:wasm-instance old-buffer)
         ;; Create new buffer in place of old one
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (WasmGapBuffer. content)
         detected-language (detect-language-from-filename name)
         lines (clojure.string/split content #"\n" -1)
         line-count (count lines)
         new-buffer {:id old-buffer-id
                    :wasm-instance wasm-instance
                    :file-handle file-handle
                    :name name
                    :is-modified? false
                    :mark-position nil
                    :cursor-position {:line 0 :column 0}
                    :selection-range nil
                    :major-mode :fundamental-mode
                    :minor-modes #{}
                    :buffer-local-vars {}
                    :ast nil
                    :language detected-language
                    :diagnostics []
                    :undo-stack []
                    :undo-in-progress? false
                    :editor-version 0
                    :cache {:text content
                            :line-count line-count}}]
     ;; Free old WASM instance
     (when old-wasm-instance
       (.free ^js old-wasm-instance))
     {:db (-> db
              (assoc-in [:buffers old-buffer-id] new-buffer)
              (update :ui dissoc :find-alternate-file-buffer-id))
      :fx [[:dispatch [:parser/request-parse old-buffer-id]]
           [:dispatch [:lsp/on-buffer-opened old-buffer-id]]
           [:focus-editor]]})))
;; =============================================================================
;; Replace Commands (M-x replace-string, M-x replace-regexp)
;; =============================================================================

(rf/reg-event-fx
 :replace-string
 (fn [{:keys [db]} [_]]
   "Replace string non-interactively from point to end of buffer (M-x replace-string) - Phase 6.5 Week 3-4"
   {:fx [[:dispatch [:minibuffer/activate
                    {:prompt "Replace string: "
                     :on-confirm [:replace-string/read-replacement]
                     :replace? true}]]]}))

(rf/reg-event-fx
 :replace-string/read-replacement
 (fn [{:keys [db]} [_ search-string]]
   "Read replacement string after search string - Phase 6.5 Week 3-4"
   (if (clojure.string/blank? search-string)
     {:fx [[:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/message "Empty search string"]]]}
     {:db (assoc-in db [:ui :replace-search-string] search-string)
      :fx [[:dispatch [:minibuffer/activate
                      {:prompt (str "Replace string " search-string " with: ")
                       :on-confirm [:replace-string/do-replace]
                       :replace? true}]]]})))

(rf/reg-event-fx
 :replace-string/do-replace
 (fn [{:keys [db]} [_ replacement-string]]
   "Perform non-interactive string replacement from point to end"
   (let [search-string     (get-in db [:ui :replace-search-string])
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         current-pos       (get-in db [:ui :cursor-position] 0)
         full-text         (.getText wasm-instance)
         text-from-point   (subs full-text current-pos)

         ;; Count occurrences
         escaped-search (.replace search-string #"[.*+?^${}()|\\[\\]\\\\]" "\\$&")
         search-pattern (js/RegExp. escaped-search "g")
         matches        (array-seq (.match text-from-point search-pattern))
         match-count    (if matches (count matches) 0)]

     (if (zero? match-count)
       {:db (update db :ui dissoc :replace-search-string)
        :fx [[:dispatch [:echo/message (str "No occurrences of \"" search-string "\"")]]]}

       (let [;; Replace all occurrences from point forward
             new-text-from-point (.replace text-from-point search-pattern replacement-string)
             new-full-text       (str (subs full-text 0 current-pos) new-text-from-point)
             text-length-to-end  (- (count full-text) current-pos)]

         ;; Update buffer using delete + insert
         (.delete wasm-instance current-pos text-length-to-end)
         (.insert wasm-instance current-pos new-text-from-point)

         (let [lines      (clojure.string/split new-full-text #"\n" -1)
               line-count (count lines)]
           {:db (-> db
                    (assoc-in [:buffers buffer-id :cache :text] new-full-text)
                    (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                    (assoc-in [:buffers buffer-id :is-modified?] true)
                    (update-in [:buffers buffer-id :editor-version] inc)
                    (update :ui dissoc :replace-search-string))
            :fx [[:dispatch [:echo/message (str "Replaced " match-count " occurrence"
                                                (if (= match-count 1) "" "s"))]]]}))))))

 (rf/reg-event-fx
  :replace-regexp
  (fn [{:keys [db]} [_]]
    "Replace regexp non-interactively from point to end of buffer (M-x replace-regexp) - Phase 6.5 Week 3-4"
    {:fx [[:dispatch [:minibuffer/activate
                      {:prompt     "Replace regexp: "
                       :on-confirm [:replace-regexp/read-replacement]
                       :replace? true}]]]}))

(rf/reg-event-fx
 :replace-regexp/read-replacement
 (fn [{:keys [db]} [_ regexp-string]]
   "Read replacement string after regexp - Phase 6.5 Week 3-4"
   (if (clojure.string/blank? regexp-string)
     {:fx [[:dispatch [:echo/message "Empty regexp"]]
           [:dispatch [:minibuffer/deactivate]]]}
     ;; Validate regexp
     (try
       (js/RegExp. regexp-string)
       {:db (assoc-in db [:ui :replace-regexp-string] regexp-string)
        :fx [[:dispatch [:minibuffer/activate
                         {:prompt     (str "Replace regexp " regexp-string " with: ")
                          :on-confirm [:replace-regexp/do-replace]
                          :replace?   true}]]]}
       (catch js/Error e
         {:fx [[:dispatch [:echo/message (str "Invalid regexp: " (.-message e))]]]})))))

(rf/reg-event-fx
 :replace-regexp/do-replace
 (fn [{:keys [db]} [_ replacement-string]]
   "Perform non-interactive regexp replacement from point to end"
   (let [regexp-string     (get-in db [:ui :replace-regexp-string])
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         current-pos       (get-in db [:ui :cursor-position] 0)
         full-text         (.getText wasm-instance)
         text-from-point   (subs full-text current-pos)]

     (try
       (let [;; Create regexp with global flag
             search-pattern (js/RegExp. regexp-string "g")

             ;; Count matches
             matches (array-seq (.match text-from-point search-pattern))
             match-count   (if matches (count matches) 0)]

         (if (zero? match-count)
           {:db (update db :ui dissoc :replace-regexp-string)
            :fx [[:dispatch [:echo/message (str "No match for regexp \"" regexp-string "\"")]]]}

           (let [;; Replace all matches from point forward
                 new-text-from-point (.replace text-from-point search-pattern replacement-string)
                 new-full-text       (str (subs full-text 0 current-pos) new-text-from-point)
                 text-length-to-end  (- (count full-text) current-pos)]

             ;; Update buffer using delete + insert
             (.delete wasm-instance current-pos text-length-to-end)
             (.insert wasm-instance current-pos new-text-from-point)

             (let [lines      (clojure.string/split new-full-text #"\n" -1)
                   line-count (count lines)]
               {:db (-> db
                        (assoc-in [:buffers buffer-id :cache :text] new-full-text)
                        (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                        (assoc-in [:buffers buffer-id :is-modified?] true)
                        (update-in [:buffers buffer-id :editor-version] inc)
                        (update :ui dissoc :replace-regexp-string))
                :fx [[:dispatch [:echo/message (str "Replaced " match-count " match"
                                                    (if (= match-count 1) "" "es"))]]]}))))
       (catch js/Error e
         {:db (update db :ui dissoc :replace-regexp-string)
          :fx [[:dispatch [:echo/message (str "Regexp error: " (.-message e))]]]})))))

;; =============================================================================
;; Query Replace (M-%, query-replace)
;; =============================================================================

(rf/reg-event-fx
 :query-replace
 (fn [{:keys [db]} [_]]
   "Start interactive query-replace (M-%) - Phase 6.5 Week 3-4"
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Query replace: "
                      :on-confirm [:query-replace/read-replacement]
                      :replace? true}]]]}))

(rf/reg-event-fx
 :query-replace/read-replacement
 (fn [{:keys [db]} [_ search-string]]
   "Read replacement string after search string - Phase 6.5 Week 3-4"
   (if (clojure.string/blank? search-string)
     {:fx [[:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/message "Empty search string"]]]}
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt (str "Query replace " search-string " with: ")
                        :on-confirm [:query-replace/start search-string]
                        :replace? true}]]]})))

(defn linear-to-line-col
  "Convert linear position to line/column. Simple implementation for query-replace."
  [text pos]
  (let [before-pos (subs text 0 pos)
        lines (clojure.string/split before-pos #"\n" -1)
        line (dec (count lines))
        column (count (last lines))]
    {:line line :column column}))

(defn find-next-match
  "Find the next match for search-string in text starting from pos.
   Returns {:start pos :end pos} or nil if no match found."
  [text search-string pos is-regexp?]
  (try
    (if is-regexp?
      ;; Regexp search
      (let [text-from-pos (subs text pos)
            pattern (js/RegExp. search-string)
            match (.match text-from-pos pattern)]
        (when match
          (let [match-index (.-index match)
                match-text (aget match 0)
                abs-start (+ pos match-index)
                abs-end (+ abs-start (count match-text))]
            {:start abs-start :end abs-end})))
      ;; Literal search
      (let [text-from-pos (subs text pos)
            match-index (.indexOf text-from-pos search-string)]
        (when (>= match-index 0)
          (let [abs-start (+ pos match-index)
                abs-end (+ abs-start (count search-string))]
            {:start abs-start :end abs-end}))))
    (catch js/Error e
      (js/console.error "Error in find-next-match:" e)
      nil)))

(defn sync-window-and-global-cursor
  "Update both window cursor AND global UI cursor atomically.

   This is required because we have two sources of truth:
   - :window-tree :cursor-position (line-col map) - for UI rendering
   - :ui :cursor-position (linear integer) - for E2E tests/legacy code

   These MUST stay synchronized as documented in db.cljs line 31.

   Issue #65: Without this sync, E2E tests fail (read global state)
   while manual testing works (reads window state)."
  [db window-id cursor-line-col]
  (let [window (db/find-window-in-tree (:window-tree db) window-id)
        buffer-id (:buffer-id window)
        buffer-text (get-in db [:buffers buffer-id :cache :text])

        ;; Convert line-col to linear for global UI state
        linear-cursor (line-col-to-linear-pos buffer-text
                                              (:line cursor-line-col)
                                              (:column cursor-line-col))

        ;; Update window tree
        new-tree (db/update-window-in-tree (:window-tree db) window-id
                                           #(assoc % :cursor-position cursor-line-col))]
    ;; Update BOTH window AND global cursor
    (-> db
        (assoc :window-tree new-tree)
        (assoc-in [:ui :cursor-position] linear-cursor))))

(rf/reg-event-fx
 :query-replace/start
 (fn [{:keys [db]} [_ search-string replacement-string]]
   "Initialize query-replace state and find first match"
   (let [active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         current-pos       (get-in db [:ui :cursor-position] 0)
         full-text         (.getText wasm-instance)
         is-regexp?        false  ; query-replace uses literal search
         match             (find-next-match full-text search-string current-pos is-regexp?)]

     (if match
       ;; Found first match - activate query-replace mode and create region to highlight the match
       (let [cursor-line-col (linear-to-line-col full-text (:end match))
              mark-linear-pos (:start match)  ; Linear position for mark
              active-window-id (:active-window-id db)

              ;; Sync window AND global cursor (Issue #65)
              synced-db (sync-window-and-global-cursor db active-window-id cursor-line-col)

              ;; Update window mark separately
              new-tree (db/update-window-in-tree (:window-tree synced-db) active-window-id
                                                 #(assoc % :mark-position mark-linear-pos))
              new-db (-> synced-db
                        (assoc :window-tree new-tree)
                        (assoc-in [:ui :query-replace :active?] true)
                        (assoc-in [:ui :query-replace :search-string] search-string)
                        (assoc-in [:ui :query-replace :replacement-string] replacement-string)
                        (assoc-in [:ui :query-replace :is-regexp?] is-regexp?)
                        (assoc-in [:ui :query-replace :current-match] match)
                        (assoc-in [:ui :query-replace :original-cursor-pos] current-pos)
                        (assoc-in [:ui :query-replace :replaced-count] 0)
                        (assoc-in [:ui :query-replace :match-history] [])
                        (assoc-in [:ui :query-replace :replace-all?] false))]
         {:db new-db
          :fx [[:dispatch [:echo/message (str "Query replacing " search-string " with " replacement-string
                                              " (y/n/!/q/^/. for help)")]]]})
       ;; No match found
       {:fx [[:dispatch [:echo/message (str "No match for \"" search-string "\"")]]]}))))

(rf/reg-event-fx
 :query-replace/handle-key
 (fn [{:keys [db]} [_ key-str]]
   "Handle key press during query-replace mode"
   (println "🔑 query-replace/handle-key received:" (pr-str key-str))
   (let [qr-state (get-in db [:ui :query-replace])]
     (case key-str
       ;; y or SPC - replace this match and continue
       ("y" "SPC")
       {:fx [[:dispatch [:query-replace/replace-current-and-continue]]]}

       ;; n or DEL - skip this match and continue
       ("n" "DEL")
       {:fx [[:dispatch [:query-replace/skip-and-continue]]]}

       ;; ! - replace all remaining without asking
       "!"
       {:fx [[:dispatch [:query-replace/replace-all]]]}

       ;; q, RET, or ESC - quit
       ("q" "RET" "ESC")
       {:fx [[:dispatch [:query-replace/quit]]]}

       ;; ^ - go back to previous match
       "^"
       {:fx [[:dispatch [:query-replace/go-back]]]}

       ;; . - replace this match and quit
       "."
       {:fx [[:dispatch [:query-replace/replace-and-quit]]]}

       ;; Unknown key - show help
       {:fx [[:dispatch [:echo/message "y=yes, n=no, !=all, q=quit, ^=back, .=replace&quit"]]]}))))

(rf/reg-event-fx
 :query-replace/replace-current-and-continue
 (fn [{:keys [db]} [_]]
   "Replace current match and find next"
   (let [qr-state          (get-in db [:ui :query-replace])
         search-string     (:search-string qr-state)
         replacement       (:replacement-string qr-state)
         is-regexp?        (:is-regexp? qr-state)
         current-match     (:current-match qr-state)
         match-start       (:start current-match)
         match-end         (:end current-match)
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         full-text         (.getText wasm-instance)

         ;; Perform replacement
         new-text          (str (subs full-text 0 match-start)
                                replacement
                                (subs full-text match-end))

         ;; Find next match after replacement
         next-search-pos   (+ match-start (count replacement))
         next-match        (find-next-match new-text search-string next-search-pos is-regexp?)]

     ;; Update buffer text using delete + insert
     (.delete wasm-instance match-start (- match-end match-start))
     (.insert wasm-instance match-start replacement)

     (if next-match
       ;; Found next match - continue and create region to highlight it
       (let [lines (clojure.string/split new-text #"\n" -1)
             line-count (count lines)
             cursor-line-col (linear-to-line-col new-text (:end next-match))
             mark-linear-pos (:start next-match)  ; Linear position for mark
             active-window-id (:active-window-id db)

             ;; Sync window AND global cursor (Issue #65)
             synced-db (sync-window-and-global-cursor db active-window-id cursor-line-col)

             ;; Update window mark separately
             new-tree (db/update-window-in-tree (:window-tree synced-db) active-window-id
                                                #(assoc % :mark-position mark-linear-pos))
             new-db (-> synced-db
                       (assoc :window-tree new-tree)
                       (assoc-in [:buffers buffer-id :cache :text] new-text)
                       (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                       (assoc-in [:buffers buffer-id :is-modified?] true)
                       (update-in [:buffers buffer-id :editor-version] inc)
                       (update-in [:ui :query-replace :replaced-count] inc)
                       (update-in [:ui :query-replace :match-history] conj current-match)
                       (assoc-in [:ui :query-replace :current-match] next-match))]
         {:db new-db})
       ;; No more matches - finish
       (let [lines (clojure.string/split new-text #"\n" -1)
             line-count (count lines)
             count (inc (:replaced-count qr-state))]
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] new-text)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (assoc-in [:buffers buffer-id :is-modified?] true)
                  (update-in [:buffers buffer-id :editor-version] inc)
                  (update :ui dissoc :query-replace))
          :fx [[:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message (str "Replaced " count " occurrence"
                                              (if (= count 1) "" "s"))]]]})))))

(rf/reg-event-fx
 :query-replace/skip-and-continue
 (fn [{:keys [db]} [_]]
   "Skip current match and find next"
   (let [qr-state          (get-in db [:ui :query-replace])
         search-string     (:search-string qr-state)
         is-regexp?        (:is-regexp? qr-state)
         current-match     (:current-match qr-state)
         match-end         (:end current-match)
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         full-text         (.getText wasm-instance)

         ;; Find next match after current
         next-match        (find-next-match full-text search-string match-end is-regexp?)]

     (println "🔍 SKIP: searching for" (pr-str search-string) "from pos" match-end)
     (println "🔍 SKIP: text length:" (count full-text) "next-match:" next-match)
     (println "🔍 SKIP: current-match:" current-match)

     (if next-match
       ;; Found next match - continue and create region to highlight it
       (let [cursor-line-col (linear-to-line-col full-text (:end next-match))
             mark-linear-pos (:start next-match)  ; Linear position for mark
             active-window-id (:active-window-id db)

             ;; Sync window AND global cursor (Issue #65)
             synced-db (sync-window-and-global-cursor db active-window-id cursor-line-col)

             ;; Update window mark separately
             new-tree (db/update-window-in-tree (:window-tree synced-db) active-window-id
                                                #(assoc % :mark-position mark-linear-pos))
             new-db (-> synced-db
                       (assoc :window-tree new-tree)
                       (update-in [:ui :query-replace :match-history] conj current-match)
                       (assoc-in [:ui :query-replace :current-match] next-match))]
         {:db new-db})
       ;; No more matches - finish
       (let [count (:replaced-count qr-state)]
         {:db (update db :ui dissoc :query-replace)
          :fx [[:dispatch [:echo/message (str "Replaced " count " occurrence"
                                              (if (= count 1) "" "s"))]]]})))))

(rf/reg-event-fx
 :query-replace/replace-all
 (fn [{:keys [db]} [_]]
   "Replace all remaining matches without prompting"
   (let [qr-state          (get-in db [:ui :query-replace])
         search-string     (:search-string qr-state)
         replacement       (:replacement-string qr-state)
         is-regexp?        (:is-regexp? qr-state)
         current-match     (:current-match qr-state)
         match-start       (:start current-match)
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         full-text         (.getText wasm-instance)

         ;; Replace all from current position forward
         text-before       (subs full-text 0 match-start)
         text-from-match   (subs full-text match-start)]

     (try
       (let [;; Escape special regex chars for literal search
             escape-regex-chars (fn [s]
                                  (clojure.string/replace s #"[.\*\+\?\^\$\{\}\(\)\|\[\]\\]" "\\\\$&"))
             ;; Count matches in remaining text
             search-pattern  (if is-regexp?
                               (js/RegExp. search-string "g")
                               (js/RegExp. (escape-regex-chars search-string) "g"))
             matches         (array-seq (.match text-from-match search-pattern))
             match-count     (if matches (count matches) 0)

             ;; Replace all in remaining text
             new-text-from   (.replace text-from-match search-pattern replacement)
             new-full-text   (str text-before new-text-from)
             current-length  (.-length wasm-instance)

             ;; Update buffer
             lines           (clojure.string/split new-full-text #"\n" -1)
             line-count      (count lines)
             total-replaced  (+ (:replaced-count qr-state) match-count)]

         (.delete wasm-instance 0 current-length)
         (.insert wasm-instance 0 new-full-text)

         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] new-full-text)
                  (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                  (assoc-in [:buffers buffer-id :is-modified?] true)
                  (update-in [:buffers buffer-id :editor-version] inc)
                  (update :ui dissoc :query-replace))
          :fx [[:dispatch [:echo/message (str "Replaced " total-replaced " occurrence"
                                              (if (= total-replaced 1) "" "s"))]]]})
       (catch js/Error e
         {:fx [[:dispatch [:query-replace/quit]]
               [:dispatch [:echo/message (str "Error: " (.-message e))]]]})))))

(rf/reg-event-fx
 :query-replace/quit
 (fn [{:keys [db]} [_]]
   "Quit query-replace mode"
   (let [count (get-in db [:ui :query-replace :replaced-count] 0)]
     {:db (update db :ui dissoc :query-replace)
      :fx [[:dispatch [:deactivate-mark]]
           [:dispatch [:echo/message (str "Replaced " count " occurrence"
                                          (if (= count 1) "" "s") " (quit)")]]]})))

(rf/reg-event-fx
 :query-replace/go-back
 (fn [{:keys [db]} [_]]
   "Go back to previous match"
   (let [qr-state      (get-in db [:ui :query-replace])
         match-history (:match-history qr-state)
         current-match (:current-match qr-state)]

     (if (empty? match-history)
       ;; No previous match
       {:fx [[:dispatch [:echo/message "No previous match"]]]}
       ;; Go back to previous match
       (let [prev-match (last match-history)
             new-history (vec (butlast match-history))]
         {:db (-> db
                  (assoc-in [:ui :query-replace :match-history] new-history)
                  (assoc-in [:ui :query-replace :current-match] prev-match))
          :fx [[:dispatch [:cursor/set-position (:start prev-match)]]]})))))

(rf/reg-event-fx
 :query-replace/replace-and-quit
 (fn [{:keys [db]} [_]]
   "Replace current match and quit"
   (let [qr-state          (get-in db [:ui :query-replace])
         search-string     (:search-string qr-state)
         replacement       (:replacement-string qr-state)
         current-match     (:current-match qr-state)
         match-start       (:start current-match)
         match-end         (:end current-match)
         active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         full-text         (.getText wasm-instance)

         ;; Perform replacement
         new-text          (str (subs full-text 0 match-start)
                                replacement
                                (subs full-text match-end))
         current-length    (.-length wasm-instance)
         lines             (clojure.string/split new-text #"\n" -1)
         line-count        (count lines)
         count             (inc (:replaced-count qr-state))]

     ;; Update buffer text using delete + insert
     (.delete wasm-instance 0 current-length)
     (.insert wasm-instance 0 new-text)

     {:db (-> db
              (assoc-in [:buffers buffer-id :cache :text] new-text)
              (assoc-in [:buffers buffer-id :cache :line-count] line-count)
              (assoc-in [:buffers buffer-id :is-modified?] true)
              (update-in [:buffers buffer-id :editor-version] inc)
              (update :ui dissoc :query-replace))
      :fx [[:dispatch [:echo/message (str "Replaced " count " occurrence"
                                          (if (= count 1) "" "s") " (done)")]]]})))

;; =============================================================================
;; Query Replace Regexp (C-M-%, query-replace-regexp)
;; =============================================================================

(rf/reg-event-fx
 :query-replace-regexp
 (fn [{:keys [db]} [_]]
   "Start interactive query-replace with regexp (C-M-%) - Phase 6.5 Week 3-4"
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Query replace regexp: "
                      :on-confirm [:query-replace-regexp/read-replacement]
                      :replace? true}]]]}))

(rf/reg-event-fx
 :query-replace-regexp/read-replacement
 (fn [{:keys [db]} [_ regexp-string]]
   "Read replacement string after regexp string - Phase 6.5 Week 3-4"
   (if (clojure.string/blank? regexp-string)
     {:fx [[:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/message "Empty regexp"]]]}
     ;; Validate regexp before proceeding
     (try
       (js/RegExp. regexp-string)
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt (str "Query replace regexp " regexp-string " with: ")
                          :on-confirm [:query-replace-regexp/start regexp-string]
                          :replace? true}]]]}
       (catch js/Error e
         {:fx [[:dispatch [:minibuffer/deactivate]]
               [:dispatch [:echo/message (str "Invalid regexp: " (.-message e))]]]})))))


(rf/reg-event-fx
 :query-replace-regexp/start
 (fn [{:keys [db]} [_ regexp-string replacement-string]]
   "Initialize query-replace-regexp state and find first match using regexp"
   (let [active-window     (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id         (:buffer-id active-window)
         buffer            (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         current-pos       (get-in db [:ui :cursor-position] 0)
         full-text         (.getText wasm-instance)
         is-regexp?        true  ; query-replace-regexp uses regexp search
         match             (find-next-match full-text regexp-string current-pos is-regexp?)]
     (if match
       ;; Found first match - activate query-replace mode and create region to highlight the match
       (let [cursor-line-col (linear-to-line-col full-text (:end match))
              mark-linear-pos (:start match)  ; Linear position for mark
              active-window-id (:active-window-id db)

              ;; Sync window AND global cursor (Issue #65)
              synced-db (sync-window-and-global-cursor db active-window-id cursor-line-col)

              ;; Update window mark separately
              new-tree (db/update-window-in-tree (:window-tree synced-db) active-window-id
                                                 #(assoc % :mark-position mark-linear-pos))
              new-db (-> synced-db
                        (assoc :window-tree new-tree)
                        (assoc-in [:ui :query-replace :active?] true)
                        (assoc-in [:ui :query-replace :search-string] regexp-string)
                        (assoc-in [:ui :query-replace :replacement-string] replacement-string)
                        (assoc-in [:ui :query-replace :is-regexp?] is-regexp?)
                        (assoc-in [:ui :query-replace :current-match] match)
                        (assoc-in [:ui :query-replace :original-cursor-pos] current-pos)
                        (assoc-in [:ui :query-replace :replaced-count] 0)
                        (assoc-in [:ui :query-replace :match-history] [])
                        (assoc-in [:ui :query-replace :replace-all?] false))]
         {:db new-db
          :fx [[:dispatch [:echo/message (str "Query replacing " regexp-string " with " replacement-string
                                              " (y/n/!/q/^/. for help)")]]]})
       ;; No match found
       {:fx [[:dispatch [:echo/message (str "No match for regexp: " regexp-string)]]]}))))

(rf/reg-event-fx
:close-buffer
(fn [{:keys [db]} [_ buffer-id]]
  "Close a buffer and free its WASM memory, with defensive logic"
  (let [buffers                    (:buffers db)
        buffer-to-close            (get buffers buffer-id)
        active-window-id           (:active-window-id db)
        active-window              (get (:windows db) active-window-id)
        currently-active-buffer-id (:buffer-id active-window)]
    
    (if buffer-to-close
      (let [wasm-instance        (:wasm-instance buffer-to-close)
            remaining-buffers    (dissoc buffers buffer-id)
            remaining-buffer-ids (keys remaining-buffers)]
        
        ;; Free the WASM instance memory
        (when wasm-instance
          (.free ^js wasm-instance))
        
        (if (empty? remaining-buffer-ids)
          ;; No buffers left - create a new default *scratch* buffer
          (let [new-buffer-id     (inc (apply max 0 (keys buffers)))
                WasmGapBuffer    (get-in db [:system :wasm-constructor])
                new-wasm-instance (WasmGapBuffer. "")]
            {:db (-> db
                     (assoc :buffers {new-buffer-id {:id            new-buffer-id
                                                     :wasm-instance new-wasm-instance
                                                     :file-handle   nil
                                                     :name          "*scratch*"
                                                     :is-modified?  false
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

;; =============================================================================
;; Buffer Editing Operations (for Emacs semantic compatibility)
;; =============================================================================

(rf/reg-event-fx
 :buffer/insert
 (fn [{:keys [db]} [_ buffer-id position text]]
   "Insert TEXT at POSITION in BUFFER-ID.

   Args:
     buffer-id - Buffer to insert into
     position - Linear position (0-indexed)
     text - String to insert

   Contract: 4.1 (Buffers), 4.2 (Point)"
   (let [buffer (get-in db [:buffers buffer-id])
         ^js wasm-instance (:wasm-instance buffer)
         read-only? (:is-read-only? buffer)
         buffer-text-before (when wasm-instance (.getText ^js wasm-instance))]
     ;; Only insert if buffer is not read-only
     (when (and wasm-instance (not read-only?))
       (.insert wasm-instance position text))
     (let [buffer-text-after (when wasm-instance (.getText ^js wasm-instance))
           log-msg (str "INSERT[" buffer-id "]: before=" (pr-str buffer-text-before)
                       " text=" (pr-str text)
                       " pos=" position
                       " after=" (pr-str buffer-text-after))]
       ;; Record undo entry directly (synchronous for tests)
       ;; NOTE: Boundaries are added by commands, not by individual operations
       (let [should-record? (and wasm-instance (not read-only?))
             recording-enabled? (get-in db [:undo :recording-enabled?] true)
             current-point (get-in db [:buffers buffer-id :point] 0)
             ;; Point should move forward by length of inserted text
             point-after-insert (+ position (count text))
             new-db (if (and should-record? recording-enabled?)
                      (let [;; Record point before the edit (for undo restoration)
                            marker-entry {:type :marker
                                          :marker-id :point
                                          :old-pos current-point
                                          :new-pos point-after-insert}
                            undo-entry {:type :edit
                                        :op :insert
                                        :position position
                                        :text text}
                            stack (get-in db [:buffers buffer-id :undo-stack] [])
                            ;; Add marker, then edit entry
                            updated-stack (-> stack
                                              (conj marker-entry)
                                              (conj undo-entry))]
                        (-> db
                            (assoc-in [:buffers buffer-id :undo-stack] updated-stack)
                            ;; Update point after insertion (Emacs semantics)
                            (assoc-in [:buffers buffer-id :point] point-after-insert)))
                      ;; Even if not recording, still update point
                      (assoc-in db [:buffers buffer-id :point] point-after-insert))]
         {:db new-db})))))

(rf/reg-event-fx
 :buffer/goto-char
 (fn [{:keys [db]} [_ buffer-id position]]
   "Move point to POSITION in BUFFER-ID.

   Args:
     buffer-id - Buffer to move point in
     position - Linear position (0-indexed)

   Contract: 4.2 (Point)"
   ;; Store point in buffer state
   (let [current-buffer-id (get-in db [:editor :current-buffer-id])
         new-db (assoc-in db [:buffers buffer-id :point] position)]
     ;; If this is the current buffer, also update UI cursor
     (if (= buffer-id current-buffer-id)
       {:db new-db
        :fx [[:dispatch [:cursor/set-position position]]]}
       {:db new-db}))))

(rf/reg-event-db
 :set-current-buffer
 (fn [db [_ buffer-id]]
   "Set BUFFER-ID as the current buffer.

   Args:
     buffer-id - Buffer to make current

   Contract: 4.1 (Buffers) - Phase 6.6 Emacs compatibility"
   (assoc-in db [:editor :current-buffer-id] buffer-id)))

