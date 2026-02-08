(ns lexicon.core.occur
  "Occur mode - list all lines matching a regexp.

  M-x occur prompts for a regexp and creates an *Occur* buffer
  showing all matching lines with line numbers. Pressing RET
  on a match jumps to that line in the original buffer.

  Keybindings in *Occur* buffer:
  - RET: Jump to match in source buffer
  - n: Move to next match line (skips header/blank lines)
  - p: Move to previous match line (skips header/blank lines)
  - q: Quit occur buffer and return to source

  Emacs-faithful features:
  - Opens in split window alongside source
  - n/p navigate only between match lines
  - Navigation previews corresponding line in source
  - Match pattern is highlighted

  Based on Emacs lisp/replace.el (occur-mode section)"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State
;; =============================================================================

;; Store the source buffer info for navigation
;; {:buffer-id N
;;  :buffer-name "name"
;;  :regexp "pattern"
;;  :matches [{:line-num N :text "..." :occur-line N}...]
;;  :occur-match-lines [2 3 4 ...]}  ; 0-indexed lines in occur buffer that are matches
(defonce occur-source-buffer (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn format-line-num
  "Format line number with padding."
  [fmt & args]
  (reduce (fn [s arg]
            (str/replace-first s #"%\d*d" (str arg)))
          fmt
          args))

(defn find-matching-lines
  "Find all lines matching regexp in text.
   Returns vector of {:line-num N :text string :match-ranges [[start end]...]}"
  [text regexp-str]
  (try
    (let [lines (str/split-lines text)
          re (js/RegExp. regexp-str "gi")]
      (vec
       (keep-indexed
        (fn [idx line]
          ;; Find all matches in this line for highlighting
          (let [matches (loop [result []
                               last-index 0]
                          (set! (.-lastIndex re) last-index)
                          (if-let [m (.exec re line)]
                            (let [match-start (.-index m)
                                  match-text (aget m 0)
                                  match-end (+ match-start (count match-text))]
                              (recur (conj result [match-start match-end])
                                     match-end))
                            result))]
            (when (seq matches)
              {:line-num (inc idx)  ; 1-indexed
               :text line
               :match-ranges matches
               :position (reduce + (map #(inc (count %)) (take idx lines)))})))
        lines)))
    (catch :default _
      ;; Invalid regexp
      [])))

(defn format-occur-buffer
  "Format the occur buffer content.
   Returns {:content string
            :match-lines [line-indices...]
            :highlights [{:line N :ranges [[start end]...]}...]}"
  [matches regexp source-buffer-name]
  (let [header (str (count matches) " matches for \"" regexp "\" in buffer: " source-buffer-name "\n")
        ;; Header is line 0, blank line is line 1, matches start at line 2
        match-lines (vec (range 2 (+ 2 (count matches))))
        ;; Format each line and compute highlight positions
        ;; Line prefix is "%6d: " = 8 characters
        prefix-len 8
        formatted-lines (map-indexed
                         (fn [idx {:keys [line-num text match-ranges]}]
                           {:formatted (str (format-line-num "%6d: " line-num) text)
                            :occur-line (+ 2 idx)  ; 0-indexed line in occur buffer
                            ;; Offset match ranges by prefix length
                            :ranges (mapv (fn [[start end]]
                                           [(+ prefix-len start) (+ prefix-len end)])
                                         match-ranges)})
                         matches)
        content (str header "\n"
                     (str/join "\n" (map :formatted formatted-lines)))
        ;; Extract highlight data for the view layer
        highlights (mapv (fn [{:keys [occur-line ranges]}]
                          {:line occur-line :ranges ranges})
                        formatted-lines)]
    {:content content
     :match-lines match-lines
     :highlights highlights}))

(defn get-current-occur-line
  "Get the current line number (0-indexed) in the occur buffer."
  [db]
  (let [cursor-pos (get-in db [:ui :cursor-position] 0)
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        buffer (get-in db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)
        text (when wasm-instance (.getText wasm-instance))
        lines-before (when text (subs text 0 (min cursor-pos (count text))))]
    (if lines-before
      (count (filter #(= % \newline) lines-before))
      0)))

(defn find-next-match-line
  "Find the next match line index after current-line.
   Returns nil if no more matches."
  [match-lines current-line]
  (first (filter #(> % current-line) match-lines)))

(defn find-prev-match-line
  "Find the previous match line index before current-line.
   Returns nil if no previous matches."
  [match-lines current-line]
  (last (filter #(< % current-line) match-lines)))

(defn line-start-position
  "Calculate the character position at the start of line n (0-indexed)."
  [text line-num]
  (let [lines (str/split-lines text)]
    (if (zero? line-num)
      0
      (reduce + (map #(inc (count %)) (take line-num lines))))))

(defn get-source-line-at-occur-line
  "Get the source line number for a given occur buffer line.
   Returns nil if not on a match line."
  [occur-line]
  (when-let [source @occur-source-buffer]
    (let [matches (:matches source)
          ;; occur-line 2 = first match, 3 = second match, etc.
          match-index (- occur-line 2)]
      (when (and (>= match-index 0) (< match-index (count matches)))
        (:line-num (nth matches match-index))))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :occur/run
 (fn [{:keys [db]} [_ regexp]]
   "Run occur with the given regexp."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         source-buffer-id (:buffer-id active-window)
         source-window-id (:active-window-id db)
         buffer (get-in db [:buffers source-buffer-id])
         buffer-name (:name buffer)
         wasm-instance (:wasm-instance buffer)
         text (when wasm-instance (.getText wasm-instance))
         matches (find-matching-lines (or text "") regexp)]

     (if (empty? matches)
       {:db db
        :fx [[:dispatch [:message/display (str "No matches for \"" regexp "\"")]]]}

       ;; Create *Occur* buffer with split window
       (let [buffers (:buffers db)
             existing-occur (first (filter #(= (:name %) "*Occur*") (vals buffers)))
             occur-buffer-id (or (:id existing-occur) (db/next-buffer-id buffers))
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             {:keys [content match-lines highlights]} (format-occur-buffer matches regexp buffer-name)
             wasm-occur-instance (WasmGapBuffer. content)
             lines (str/split-lines content)
             line-count (count lines)
             ;; Enhance matches with occur-line info
             enhanced-matches (map-indexed
                               (fn [idx m]
                                 (assoc m :occur-line (+ 2 idx)))
                               matches)

             new-buffer {:id occur-buffer-id
                         :wasm-instance wasm-occur-instance
                         :file-handle nil
                         :name "*Occur*"
                         :is-modified? false
                         :is-read-only? true
                         :mark-position nil
                         :cursor-position {:line 0 :column 0}
                         :selection-range nil
                         :major-mode :occur-mode
                         :minor-modes #{}
                         :buffer-local-vars {}
                         :ast nil
                         :language :text
                         :diagnostics []
                         :undo-stack []
                         :undo-in-progress? false
                         :editor-version 0
                         ;; Store highlights for the view layer
                         :occur-highlights highlights
                         :cache {:text content
                                 :line-count line-count}}]
         ;; Store source buffer info for navigation
         (reset! occur-source-buffer {:buffer-id source-buffer-id
                                      :buffer-name buffer-name
                                      :window-id source-window-id
                                      :regexp regexp
                                      :matches (vec enhanced-matches)
                                      :match-lines match-lines})
         {:db (assoc-in db [:buffers occur-buffer-id] new-buffer)
          :fx [;; Split window horizontally and show occur in new window
               [:dispatch [:split-window-below]]
               [:dispatch [:switch-buffer occur-buffer-id]]
               ;; Move cursor to first match line
               [:dispatch [:occur/goto-first-match]]
               [:dispatch [:echo/message (str (count matches) " matches for \"" regexp "\"")]]]})))))

(rf/reg-event-fx
 :occur/goto-first-match
 (fn [{:keys [db]} [_]]
   "Move cursor to the first match line in occur buffer."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         text (when wasm-instance (.getText wasm-instance))]
     (when-let [source @occur-source-buffer]
       (let [match-lines (:match-lines source)]
         (when (seq match-lines)
           (let [first-match-line (first match-lines)
                 pos (line-start-position text first-match-line)]
             {:fx [[:dispatch [:update-cursor-position pos]]]})))))))

(rf/reg-event-fx
 :occur/goto-match
 (fn [{:keys [db]} [_ line-num]]
   "Jump to the match at line-num in the source buffer."
   (if-let [source @occur-source-buffer]
     (let [{:keys [buffer-id matches]} source
           match (first (filter #(= (:line-num %) line-num) matches))]
       (if match
         {:db db
          :fx [[:dispatch [:switch-buffer buffer-id]]
               [:dispatch [:goto-line (:line-num match)]]]}
         {:db db
          :fx [[:dispatch [:echo/message (str "No match for line " line-num)]]]}))
     {:db db
      :fx [[:dispatch [:echo/message "No source buffer stored"]]]})))

(rf/reg-event-fx
 :occur/goto-match-at-point
 (fn [{:keys [db]} [_]]
   "Jump to the match on the current line in occur buffer."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         buffer-name (:name buffer)]
     (if (= buffer-name "*Occur*")
       (let [current-line (get-current-occur-line db)]
         (if-let [source-line (get-source-line-at-occur-line current-line)]
           {:db db
            :fx [[:dispatch [:occur/goto-match source-line]]]}
           ;; Not on a match line - go to first match
           (if-let [source @occur-source-buffer]
             (if-let [first-match (first (:matches source))]
               {:db db
                :fx [[:dispatch [:occur/goto-match (:line-num first-match)]]]}
               {:db db
                :fx [[:dispatch [:echo/message "No matches"]]]})
             {:db db
              :fx [[:dispatch [:echo/message "No source buffer data"]]]})))
       {:db db
        :fx [[:dispatch [:echo/message "Not in *Occur* buffer"]]]}))))

(rf/reg-event-fx
 :occur
 (fn [{:keys [db]} [_ regexp]]
   "Main occur command - prompts for regexp if not provided."
   (if regexp
     {:db db
      :fx [[:dispatch [:occur/run regexp]]]}
     ;; Prompt for regexp
     {:db db
      :fx [[:dispatch [:minibuffer/activate
                       {:prompt "List lines matching regexp: "
                        :on-confirm [:occur/run]}]]]})))

;; =============================================================================
;; Occur Mode Navigation Commands
;; =============================================================================

(rf/reg-event-fx
 :occur/next-line
 (fn [{:keys [db]} [_]]
   "Move to the next match line in occur buffer (skip non-match lines)."
   (if-let [source @occur-source-buffer]
     (let [match-lines (:match-lines source)
           current-line (get-current-occur-line db)
           next-line (find-next-match-line match-lines current-line)]
       (if next-line
         (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
               buffer-id (:buffer-id active-window)
               buffer (get-in db [:buffers buffer-id])
               wasm-instance (:wasm-instance buffer)
               text (when wasm-instance (.getText wasm-instance))
               pos (line-start-position text next-line)
               source-line (get-source-line-at-occur-line next-line)]
           {:fx [[:dispatch [:update-cursor-position pos]]
                 ;; Preview: show source line in other window
                 (when source-line
                   [:dispatch [:occur/preview-source source-line]])]})
         {:db db
          :fx [[:dispatch [:echo/message "No more matches"]]]}))
     {:db db})))

(rf/reg-event-fx
 :occur/prev-line
 (fn [{:keys [db]} [_]]
   "Move to the previous match line in occur buffer (skip non-match lines)."
   (if-let [source @occur-source-buffer]
     (let [match-lines (:match-lines source)
           current-line (get-current-occur-line db)
           prev-line (find-prev-match-line match-lines current-line)]
       (if prev-line
         (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
               buffer-id (:buffer-id active-window)
               buffer (get-in db [:buffers buffer-id])
               wasm-instance (:wasm-instance buffer)
               text (when wasm-instance (.getText wasm-instance))
               pos (line-start-position text prev-line)
               source-line (get-source-line-at-occur-line prev-line)]
           {:fx [[:dispatch [:update-cursor-position pos]]
                 ;; Preview: show source line in other window
                 (when source-line
                   [:dispatch [:occur/preview-source source-line]])]})
         {:db db
          :fx [[:dispatch [:echo/message "No previous matches"]]]}))
     {:db db})))

(rf/reg-event-fx
 :occur/preview-source
 (fn [{:keys [db]} [_ source-line-num]]
   "Preview the source line in the source buffer window without switching focus."
   (if-let [source @occur-source-buffer]
     (let [source-buffer-id (:buffer-id source)
           source-buffer (get-in db [:buffers source-buffer-id])
           wasm-instance (:wasm-instance source-buffer)]
       (when wasm-instance
         (let [text (.getText wasm-instance)
               lines (str/split-lines text)
               target-line (max 1 (min source-line-num (count lines)))
               pos (reduce + (map #(inc (count %)) (take (dec target-line) lines)))]
           ;; Update the source buffer's cursor position for display
           ;; This will show in the source window without switching focus
           {:db (assoc-in db [:buffers source-buffer-id :cursor-position]
                          {:line (dec target-line) :column 0})})))
     {:db db})))

(rf/reg-event-fx
 :occur/quit
 (fn [{:keys [db]} [_]]
   "Quit the occur buffer and return to source buffer."
   (if-let [source @occur-source-buffer]
     ;; Delete the occur window and switch to source
     {:fx [[:dispatch [:delete-window]]
           [:dispatch [:switch-buffer (:buffer-id source)]]]}
     {:fx [[:dispatch [:delete-window]]]})))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-occur!
  "Initialize occur mode commands and keybindings."
  []
  ;; Register occur command
  (rf/dispatch [:register-command :occur
                {:docstring "Show all lines matching REGEXP (M-x occur)"
                 :interactive "sRegexp: "
                 :handler [:occur]}])

  ;; Register occur-mode navigation commands
  (rf/dispatch [:register-command :occur/goto-match-at-point
                {:docstring "Go to the occurrence on this line"
                 :handler [:occur/goto-match-at-point]}])

  (rf/dispatch [:register-command :occur/next-line
                {:docstring "Move to next match in occur buffer"
                 :handler [:occur/next-line]}])

  (rf/dispatch [:register-command :occur/prev-line
                {:docstring "Move to previous match in occur buffer"
                 :handler [:occur/prev-line]}])

  (rf/dispatch [:register-command :occur/quit
                {:docstring "Quit occur buffer"
                 :handler [:occur/quit]}])

  ;; Register occur-mode keymap
  (rf/dispatch-sync [:keymap/set-mode-key :occur-mode "RET" :occur/goto-match-at-point])
  (rf/dispatch-sync [:keymap/set-mode-key :occur-mode "n" :occur/next-line])
  (rf/dispatch-sync [:keymap/set-mode-key :occur-mode "p" :occur/prev-line])
  (rf/dispatch-sync [:keymap/set-mode-key :occur-mode "q" :occur/quit]))
