(ns lexicon.core.occur
  "Occur mode - list all lines matching a regexp.

  M-x occur prompts for a regexp and creates an *Occur* buffer
  showing all matching lines with line numbers. Pressing RET
  on a match jumps to that line in the original buffer.

  Based on Emacs lisp/replace.el (occur-mode section)"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State
;; =============================================================================

;; Store the source buffer info for navigation
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
   Returns vector of {:line-num N :text string :match-start N :match-end N}"
  [text regexp-str]
  (try
    (let [lines (str/split-lines text)]
      (vec
       (keep-indexed
        (fn [idx line]
          (when (re-find (js/RegExp. regexp-str "i") line)
            {:line-num (inc idx)  ; 1-indexed
             :text line
             :position (reduce + (map #(inc (count %)) (take idx lines)))}))
        lines)))
    (catch :default _
      ;; Invalid regexp
      [])))

(defn format-occur-buffer
  "Format the occur buffer content."
  [matches regexp source-buffer-name]
  (let [header (str (count matches) " matches for \"" regexp "\" in buffer: " source-buffer-name "\n\n")]
    (str header
         (str/join "\n"
                   (map (fn [{:keys [line-num text]}]
                          (str (format-line-num "%6d: " line-num) text))
                        matches)))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :occur/run
 (fn [{:keys [db]} [_ regexp]]
   "Run occur with the given regexp."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         buffer-name (:name buffer)
         wasm-instance (:wasm-instance buffer)
         text (when wasm-instance (.getText wasm-instance))
         matches (find-matching-lines (or text "") regexp)]

     (if (empty? matches)
       {:db db
        :fx [[:dispatch [:echo/message (str "No matches for \"" regexp "\"")]]]}

       (do
         ;; Store source buffer info
         (reset! occur-source-buffer {:buffer-id buffer-id
                                      :buffer-name buffer-name
                                      :matches matches})
         {:db db
          :fx [;; Create occur buffer
               [:dispatch [:buffer/create
                           {:name "*Occur*"
                            :content (format-occur-buffer matches regexp buffer-name)
                            :major-mode :occur-mode
                            :read-only true}]]
               [:dispatch [:echo/message (str (count matches) " matches")]]]})))))

(rf/reg-event-fx
 :occur/goto-match
 (fn [{:keys [db]} [_ line-num]]
   "Jump to the match at line-num in the source buffer."
   (when-let [source @occur-source-buffer]
     (let [{:keys [buffer-id matches]} source
           match (first (filter #(= (:line-num %) line-num) matches))]
       (when match
         {:db db
          :fx [[:dispatch [:switch-to-buffer-by-id buffer-id]]
               [:dispatch [:goto-line (:line-num match)]]]})))))

(rf/reg-event-fx
 :occur/goto-match-at-point
 (fn [{:keys [db]} [_]]
   "Jump to the match on the current line in occur buffer."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]
     (when (= (:name buffer) "*Occur*")
       (let [wasm-instance (:wasm-instance buffer)
             text (when wasm-instance (.getText wasm-instance))
             cursor-pos (get-in db [:ui :cursor-position] 0)
             ;; Find current line
             lines-before (subs text 0 cursor-pos)
             current-line-num (inc (count (filter #(= % \newline) lines-before)))
             ;; Get the line content (skip header lines)
             lines (str/split-lines text)
             current-line (nth lines (dec current-line-num) "")]
         ;; Parse line number from occur format "   123: text"
         (when-let [match (re-find #"^\s*(\d+):" current-line)]
           (let [source-line-num (js/parseInt (second match))]
             {:db db
              :fx [[:dispatch [:occur/goto-match source-line-num]]]})))))))

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
;; Occur Mode Setup
;; =============================================================================

(rf/reg-event-fx
 :occur-mode/setup
 (fn [{:keys [db]} [_ buffer-id]]
   "Set up occur-mode keybindings for a buffer."
   ;; occur-mode is read-only and has special keybindings
   {:db db}))

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

  ;; Register occur-mode navigation
  (rf/dispatch [:register-command :occur-mode-goto-occurrence
                {:docstring "Go to the occurrence on this line"
                 :handler [:occur/goto-match-at-point]}])

  ;; Note: RET in occur buffer should trigger goto-occurrence
  ;; This would typically be done via occur-mode keymap
  ;; For now, users can use M-x occur-mode-goto-occurrence
  )
