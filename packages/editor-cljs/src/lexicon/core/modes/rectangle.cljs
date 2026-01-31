(ns lexicon.core.modes.rectangle
  "Rectangle operations for Emacs-style columnar text editing.

  A rectangle is defined by two buffer positions (mark and point) that
  represent opposite corners. Operations work line-by-line, extracting
  column ranges from each line.

  Key commands:
  - C-x r k : kill-rectangle
  - C-x r d : delete-rectangle
  - C-x r y : yank-rectangle
  - C-x r o : open-rectangle
  - C-x r c : clear-rectangle
  - C-x r t : string-rectangle"
  (:require [re-frame.core :as rf]
            [lexicon.core.db :as db]
            [clojure.string :as str]))

;; =============================================================================
;; State
;; =============================================================================

;; The last killed rectangle (list of strings, one per line)
(defonce killed-rectangle (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn position-to-line-col
  "Convert a linear buffer position to {:line :column}."
  [text pos]
  (let [lines (str/split text #"\n" -1)
        result (loop [remaining-pos pos
                      current-line 0]
                 (if (>= current-line (count lines))
                   {:line (dec current-line) :column 0}
                   (let [line-length (count (nth lines current-line))
                         line-length-with-newline (inc line-length)]
                     (if (< remaining-pos line-length-with-newline)
                       {:line current-line :column (min remaining-pos line-length)}
                       (recur (- remaining-pos line-length-with-newline)
                              (inc current-line))))))]
    result))

(defn line-col-to-position
  "Convert {:line :column} to linear buffer position."
  [text line col]
  (let [lines (str/split text #"\n" -1)
        chars-before (reduce + (map #(inc (count %))
                                    (take line lines)))]
    (+ chars-before col)))

(defn get-rectangle-bounds
  "Given mark and point positions, return rectangle bounds as:
   {:start-line :end-line :start-col :end-col}
   with start <= end for both."
  [text mark-pos point-pos]
  (let [mark-coords (position-to-line-col text mark-pos)
        point-coords (position-to-line-col text point-pos)
        start-line (min (:line mark-coords) (:line point-coords))
        end-line (max (:line mark-coords) (:line point-coords))
        start-col (min (:column mark-coords) (:column point-coords))
        end-col (max (:column mark-coords) (:column point-coords))]
    {:start-line start-line
     :end-line end-line
     :start-col start-col
     :end-col end-col}))

(defn extract-rectangle-from-text
  "Extract a rectangle from text as a list of strings.
   Each string is the content of one line within the column range.
   If a line is shorter than end-col, pads with spaces."
  [text bounds]
  (let [{:keys [start-line end-line start-col end-col]} bounds
        lines (str/split text #"\n" -1)]
    (for [line-num (range start-line (inc end-line))]
      (let [line (get lines line-num "")
            line-len (count line)
            ;; Pad line if shorter than start-col
            padded-line (if (< line-len start-col)
                          (str line (apply str (repeat (- start-col line-len) " ")))
                          line)
            ;; Extract the column range, padding if needed
            extracted (subs padded-line start-col (min (count padded-line) end-col))
            ;; Pad extracted if shorter than width
            width (- end-col start-col)
            padded-extracted (if (< (count extracted) width)
                               (str extracted (apply str (repeat (- width (count extracted)) " ")))
                               extracted)]
        padded-extracted))))

(defn delete-rectangle-from-text
  "Delete a rectangle from text, returning the new text.
   Does not shift text left - just removes the columns."
  [text bounds]
  (let [{:keys [start-line end-line start-col end-col]} bounds
        lines (str/split text #"\n" -1)
        new-lines (map-indexed
                    (fn [idx line]
                      (if (and (>= idx start-line) (<= idx end-line))
                        ;; Remove the column range from this line
                        (let [line-len (count line)
                              before (subs line 0 (min start-col line-len))
                              after (if (< end-col line-len)
                                      (subs line end-col)
                                      "")]
                          (str before after))
                        line))
                    lines)]
    (str/join "\n" new-lines)))

(defn clear-rectangle-in-text
  "Replace rectangle content with spaces."
  [text bounds]
  (let [{:keys [start-line end-line start-col end-col]} bounds
        lines (str/split text #"\n" -1)
        width (- end-col start-col)
        spaces (apply str (repeat width " "))
        new-lines (map-indexed
                    (fn [idx line]
                      (if (and (>= idx start-line) (<= idx end-line))
                        (let [line-len (count line)
                              before (subs line 0 (min start-col line-len))
                              ;; Pad before if line is too short
                              before-padded (if (< (count before) start-col)
                                              (str before (apply str (repeat (- start-col (count before)) " ")))
                                              before)
                              after (if (< end-col line-len)
                                      (subs line end-col)
                                      "")]
                          (str before-padded spaces after))
                        line))
                    lines)]
    (str/join "\n" new-lines)))

(defn open-rectangle-in-text
  "Insert blank space to fill the rectangle, shifting text right."
  [text bounds]
  (let [{:keys [start-line end-line start-col end-col]} bounds
        lines (str/split text #"\n" -1)
        width (- end-col start-col)
        spaces (apply str (repeat width " "))
        new-lines (map-indexed
                    (fn [idx line]
                      (if (and (>= idx start-line) (<= idx end-line))
                        (let [line-len (count line)
                              before (subs line 0 (min start-col line-len))
                              ;; Pad before if line is too short
                              before-padded (if (< (count before) start-col)
                                              (str before (apply str (repeat (- start-col (count before)) " ")))
                                              before)
                              after (if (< start-col line-len)
                                      (subs line start-col)
                                      "")]
                          (str before-padded spaces after))
                        line))
                    lines)]
    (str/join "\n" new-lines)))

(defn string-rectangle-in-text
  "Replace each line of the rectangle with the given string."
  [text bounds replacement-string]
  (let [{:keys [start-line end-line start-col end-col]} bounds
        lines (str/split text #"\n" -1)
        new-lines (map-indexed
                    (fn [idx line]
                      (if (and (>= idx start-line) (<= idx end-line))
                        (let [line-len (count line)
                              before (subs line 0 (min start-col line-len))
                              ;; Pad before if line is too short
                              before-padded (if (< (count before) start-col)
                                              (str before (apply str (repeat (- start-col (count before)) " ")))
                                              before)
                              after (if (< end-col line-len)
                                      (subs line end-col)
                                      "")]
                          (str before-padded replacement-string after))
                        line))
                    lines)]
    (str/join "\n" new-lines)))

(defn insert-rectangle-at-point
  "Insert a rectangle (list of strings) at the current point.
   Each string is inserted on successive lines at the same column."
  [text point-pos rectangle]
  (when (and rectangle (seq rectangle))
    (let [{:keys [line column]} (position-to-line-col text point-pos)
          lines (str/split text #"\n" -1)
          ;; Ensure we have enough lines
          lines-padded (if (< (count lines) (+ line (count rectangle)))
                         (into (vec lines) (repeat (- (+ line (count rectangle)) (count lines)) ""))
                         lines)
          new-lines (map-indexed
                      (fn [idx line-text]
                        (let [rect-idx (- idx line)]
                          (if (and (>= rect-idx 0) (< rect-idx (count rectangle)))
                            ;; Insert rectangle line at the column
                            (let [rect-line (nth rectangle rect-idx)
                                  line-len (count line-text)
                                  before (subs line-text 0 (min column line-len))
                                  ;; Pad before if line is too short
                                  before-padded (if (< (count before) column)
                                                  (str before (apply str (repeat (- column (count before)) " ")))
                                                  before)
                                  after (if (< column line-len)
                                          (subs line-text column)
                                          "")]
                              (str before-padded rect-line after))
                            line-text)))
                      lines-padded)]
      (str/join "\n" new-lines))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :rectangle/kill
 (fn [{:keys [db]} [_]]
   "Kill (cut) the rectangle defined by mark and point.
   Saves to killed-rectangle for later yanking."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         mark-pos (:mark-position active-window)
         point-pos (get-in db [:ui :cursor-position])]
     (if (nil? mark-pos)
       {:db db
        :fx [[:dispatch [:echo/message "No mark set"]]]}
       (let [wasm-instance (:wasm-instance buffer)
             text (.getText wasm-instance)
             bounds (get-rectangle-bounds text mark-pos point-pos)
             rectangle (vec (extract-rectangle-from-text text bounds))
             new-text (delete-rectangle-from-text text bounds)]
         ;; Save rectangle and update buffer
         (reset! killed-rectangle rectangle)
         {:db db
          :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
               [:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message (str "Killed rectangle (" (count rectangle) " lines)")]]]})))))

(rf/reg-event-fx
 :rectangle/delete
 (fn [{:keys [db]} [_]]
   "Delete the rectangle without saving to kill ring."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         mark-pos (:mark-position active-window)
         point-pos (get-in db [:ui :cursor-position])]
     (if (nil? mark-pos)
       {:db db
        :fx [[:dispatch [:echo/message "No mark set"]]]}
       (let [wasm-instance (:wasm-instance buffer)
             text (.getText wasm-instance)
             bounds (get-rectangle-bounds text mark-pos point-pos)
             new-text (delete-rectangle-from-text text bounds)]
         {:db db
          :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
               [:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message "Deleted rectangle"]]]})))))

(rf/reg-event-fx
 :rectangle/yank
 (fn [{:keys [db]} [_]]
   "Yank (paste) the last killed rectangle at point."
   (if (nil? @killed-rectangle)
     {:db db
      :fx [[:dispatch [:echo/message "No rectangle to yank"]]]}
     (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
           buffer-id (:buffer-id active-window)
           buffer (get-in db [:buffers buffer-id])
           wasm-instance (:wasm-instance buffer)
           text (.getText wasm-instance)
           point-pos (get-in db [:ui :cursor-position])
           new-text (insert-rectangle-at-point text point-pos @killed-rectangle)]
       {:db db
        :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
             [:dispatch [:echo/message (str "Yanked rectangle (" (count @killed-rectangle) " lines)")]]]}))))

(rf/reg-event-fx
 :rectangle/open
 (fn [{:keys [db]} [_]]
   "Insert blank space to fill the rectangle, shifting text right."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         mark-pos (:mark-position active-window)
         point-pos (get-in db [:ui :cursor-position])]
     (if (nil? mark-pos)
       {:db db
        :fx [[:dispatch [:echo/message "No mark set"]]]}
       (let [wasm-instance (:wasm-instance buffer)
             text (.getText wasm-instance)
             bounds (get-rectangle-bounds text mark-pos point-pos)
             new-text (open-rectangle-in-text text bounds)]
         {:db db
          :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
               [:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message "Opened rectangle"]]]})))))

(rf/reg-event-fx
 :rectangle/clear
 (fn [{:keys [db]} [_]]
   "Replace rectangle content with spaces."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         mark-pos (:mark-position active-window)
         point-pos (get-in db [:ui :cursor-position])]
     (if (nil? mark-pos)
       {:db db
        :fx [[:dispatch [:echo/message "No mark set"]]]}
       (let [wasm-instance (:wasm-instance buffer)
             text (.getText wasm-instance)
             bounds (get-rectangle-bounds text mark-pos point-pos)
             new-text (clear-rectangle-in-text text bounds)]
         {:db db
          :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
               [:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message "Cleared rectangle"]]]})))))

(rf/reg-event-fx
 :rectangle/string
 (fn [{:keys [db]} [_ replacement-string]]
   "Replace each line of the rectangle with the given string."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         mark-pos (:mark-position active-window)
         point-pos (get-in db [:ui :cursor-position])]
     (if (nil? mark-pos)
       {:db db
        :fx [[:dispatch [:echo/message "No mark set"]]]}
       (let [wasm-instance (:wasm-instance buffer)
             text (.getText wasm-instance)
             bounds (get-rectangle-bounds text mark-pos point-pos)
             new-text (string-rectangle-in-text text bounds replacement-string)]
         {:db db
          :fx [[:dispatch [:editor/queue-transaction {:op :replace :start 0 :length (count text) :text new-text}]]
               [:dispatch [:deactivate-mark]]
               [:dispatch [:echo/message "String rectangle done"]]]})))))

;; =============================================================================
;; Command Registration
;; =============================================================================

(defn init-rectangle!
  "Initialize rectangle commands and keybindings."
  []
  ;; Register commands
  (rf/dispatch [:register-command :kill-rectangle
                {:docstring "Kill the rectangle defined by mark and point (C-x r k)"
                 :handler [:rectangle/kill]}])

  (rf/dispatch [:register-command :delete-rectangle
                {:docstring "Delete the rectangle without saving (C-x r d)"
                 :handler [:rectangle/delete]}])

  (rf/dispatch [:register-command :yank-rectangle
                {:docstring "Yank (paste) the last killed rectangle (C-x r y)"
                 :handler [:rectangle/yank]}])

  (rf/dispatch [:register-command :open-rectangle
                {:docstring "Insert blank space to fill rectangle, shifting right (C-x r o)"
                 :handler [:rectangle/open]}])

  (rf/dispatch [:register-command :clear-rectangle
                {:docstring "Replace rectangle content with spaces (C-x r c)"
                 :handler [:rectangle/clear]}])

  (rf/dispatch [:register-command :string-rectangle
                {:docstring "Replace each line of rectangle with a string (C-x r t)"
                 :interactive "sString rectangle: "
                 :handler [:rectangle/string]}])

  ;; Set up key bindings
  (rf/dispatch [:keymap/set-global "C-x r k" :kill-rectangle])
  (rf/dispatch [:keymap/set-global "C-x r d" :delete-rectangle])
  (rf/dispatch [:keymap/set-global "C-x r y" :yank-rectangle])
  (rf/dispatch [:keymap/set-global "C-x r o" :open-rectangle])
  (rf/dispatch [:keymap/set-global "C-x r c" :clear-rectangle])
  (rf/dispatch [:keymap/set-global "C-x r t" :string-rectangle]))
