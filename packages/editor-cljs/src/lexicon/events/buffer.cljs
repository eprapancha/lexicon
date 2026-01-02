(ns lexicon.events.buffer
  "Event handlers for buffer lifecycle, file I/O, and buffer switching.

  Handles:
  - Buffer creation and deletion
  - Buffer switching
  - File operations (open, save, write)
  - Buffer list management"
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.db :as db]
            [lexicon.completion.metadata :as completion-metadata]))

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
   "Create a new buffer with the given name and WASM instance"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         new-buffer (db/create-buffer buffer-id name wasm-instance)]
     (assoc-in db [:buffers buffer-id] new-buffer))))

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
                (assoc :window-tree new-tree)
                (assoc-in [:ui :cursor-position] linear-pos))
        :fx [[:focus-editor]]})
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
               WasmEditorCore (get-in db [:system :wasm-constructor])
               wasm-instance (WasmEditorCore. "")
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
   (let [current-buffer (get (:buffers db) (get-in db [:windows (:active-window-id db) :buffer-id]))
         current-name (:name current-buffer)
         prompt (str "Kill buffer (default " current-name "): ")]
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt prompt
                        :on-confirm [:kill-buffer-by-name]}]]]})))

(rf/reg-event-fx
 :kill-buffer-by-name
 (fn [{:keys [db]} [_ buffer-name]]
   "Kill buffer by name, or kill current if empty"
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
       ;; Don't allow killing the last buffer
       (if (= (count buffers) 1)
         {:fx []} ; Silently ignore
         (let [;; Run kill-buffer hooks
               hook-commands (get-in db [:hooks :kill-buffer-hook] [])
               ;; If killing active buffer, switch to another one first
               need-switch? (= target-id current-buffer-id)
               ;; Find another buffer to switch to
               other-buffer-id (first (filter #(not= % target-id) (keys buffers)))
               updated-db (-> db
                              ;; Remove the buffer
                              (update :buffers dissoc target-id))]
           {:db updated-db
            :fx (concat
                  ;; Run hook commands first
                  (map (fn [cmd] [:dispatch [:execute-command cmd]]) hook-commands)
                  ;; Switch to another buffer if needed
                  (when (and need-switch? other-buffer-id)
                    [[:dispatch [:switch-buffer other-buffer-id]]]))}))
       ;; Buffer not found
       {:fx []}))))

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
             WasmEditorCore (get-in db [:system :wasm-constructor])
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
             wasm-instance (WasmEditorCore. content)]

         (let [new-buffer {:id buffer-id
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
           {:db (-> db
                    (assoc-in [:buffers buffer-id] new-buffer)
                    (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))))))

(rf/reg-event-fx
 :buffer-menu/select-buffer
 (fn [{:keys [db]} [_]]
   "Select buffer at current line in buffer-menu-mode (RET key)"
   (let [active-buffer-id (get-in db [:windows (:active-window-id db) :buffer-id])
         active-buffer (get-in db [:buffers active-buffer-id])
         cursor-line (get-in active-buffer [:cursor-position :line] 0)]
     ;; Skip header lines (first 2 lines)
     (if (< cursor-line 2)
       {:db db}
       (let [buffer-content (get-in active-buffer [:cache :text] "")
             lines (clojure.string/split buffer-content #"\n" -1)
             selected-line (nth lines cursor-line nil)
             ;; Extract buffer name from line (format: " * BufferName  FilePath")
             ;; Buffer name starts at column 4 and ends before two consecutive spaces
             buffer-name (when selected-line
                          (let [trimmed (subs selected-line 4) ; Skip "MR " prefix
                                name-part (first (clojure.string/split trimmed #"\s{2,}"))]
                            (clojure.string/trim name-part)))
             ;; Find the buffer with this name
             target-buffer (when buffer-name
                            (first (filter #(= (:name %) buffer-name) (vals (:buffers db)))))
             target-id (:id target-buffer)]
         (if target-id
           {:fx [[:dispatch [:switch-buffer target-id]]
                 [:dispatch [:kill-buffer-by-id (:id active-buffer)]]]}
           {:db db}))))))

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
         wasm-instance (WasmEditorCore. content)
         detected-language (detect-language-from-filename name)
         lines (clojure.string/split content #"\n" -1)
         line-count (count lines)
           new-buffer {:id buffer-id
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
       {:db (assoc-in db [:buffers buffer-id] new-buffer)
        :fx [[:dispatch [:switch-buffer buffer-id]]
             [:dispatch [:parser/request-parse buffer-id]]
             [:dispatch [:lsp/on-buffer-opened buffer-id]]]})))

(rf/reg-event-db
 :file-read-failure
 (fn [db [_ {:keys [error message]}]]
   "Handle file read failure"
   (println "File read failed:" message error)
   ;; Could add user notification here in the future
   db))

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
                 new-wasm-instance (WasmEditorCore. "")]
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

