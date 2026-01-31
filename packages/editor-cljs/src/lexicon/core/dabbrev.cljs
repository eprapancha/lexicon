(ns lexicon.core.dabbrev
  "Dynamic abbreviation expansion (dabbrev).

  M-/ expands the word before point to a matching word found elsewhere
  in the buffer. Repeated M-/ cycles through possible expansions.

  Based on Emacs lisp/dabbrev.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State for cycling through expansions
;; =============================================================================

;; The abbreviation being expanded
(defonce last-abbreviation (atom nil))

;; Set of already-tried expansions (for deduplication)
(defonce expansion-table (atom #{}))

;; The current expansion in the buffer
(defonce last-expansion (atom nil))

;; Position where abbreviation starts
(defonce abbrev-start-pos (atom nil))

;; Search state: {:direction :backward/:forward, :position N}
(defonce search-state (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn word-char?
  "Check if character is a word character (letter, digit, underscore)."
  [ch]
  (when ch
    (re-matches #"[\w]" (str ch))))

(defn get-abbrev-at-point
  "Extract the abbreviation (partial word) before point.
   Returns {:abbrev string :start position} or nil if no abbrev."
  [text cursor-pos]
  (when (and text (pos? cursor-pos) (<= cursor-pos (count text)))
    (let [before-cursor (subs text 0 cursor-pos)]
      ;; Walk backward from cursor to find start of word
      (loop [pos (dec (count before-cursor))]
        (if (neg? pos)
          ;; Reached beginning - whole prefix is the abbrev
          (when (pos? (count before-cursor))
            {:abbrev before-cursor :start 0})
          (let [ch (nth before-cursor pos)]
            (if (word-char? ch)
              (recur (dec pos))
              ;; Found non-word char, abbrev starts after it
              (let [abbrev (subs before-cursor (inc pos))]
                (when (pos? (count abbrev))
                  {:abbrev abbrev :start (inc pos)})))))))))

(defn find-expansion-backward
  "Search backward from position for a word starting with abbrev.
   Returns {:expansion string :position N} or nil."
  [text abbrev from-pos already-found]
  (let [abbrev-lower (str/lower-case abbrev)
        abbrev-len (count abbrev)]
    (loop [pos (dec from-pos)]
      (when (>= pos 0)
        ;; Find start of a word
        (let [at-word-start? (and (word-char? (nth text pos nil))
                                  (or (zero? pos)
                                      (not (word-char? (nth text (dec pos) nil)))))]
          (if at-word-start?
            ;; Check if this word starts with abbrev
            (let [word-end (loop [end pos]
                             (if (and (< end (count text))
                                      (word-char? (nth text end nil)))
                               (recur (inc end))
                               end))
                  word (subs text pos word-end)
                  word-lower (str/lower-case word)]
              (if (and (> (count word) abbrev-len)  ; Must be longer than abbrev
                       (str/starts-with? word-lower abbrev-lower)
                       (not (contains? already-found word-lower)))
                {:expansion word :position pos}
                (recur (dec pos))))
            (recur (dec pos))))))))

(defn find-expansion-forward
  "Search forward from position for a word starting with abbrev.
   Returns {:expansion string :position N} or nil."
  [text abbrev from-pos already-found]
  (let [abbrev-lower (str/lower-case abbrev)
        abbrev-len (count abbrev)
        text-len (count text)]
    (loop [pos from-pos]
      (when (< pos text-len)
        ;; Find start of a word
        (let [at-word-start? (and (word-char? (nth text pos nil))
                                  (or (zero? pos)
                                      (not (word-char? (nth text (dec pos) nil)))))]
          (if at-word-start?
            ;; Check if this word starts with abbrev
            (let [word-end (loop [end pos]
                             (if (and (< end text-len)
                                      (word-char? (nth text end nil)))
                               (recur (inc end))
                               end))
                  word (subs text pos word-end)
                  word-lower (str/lower-case word)]
              (if (and (> (count word) abbrev-len)
                       (str/starts-with? word-lower abbrev-lower)
                       (not (contains? already-found word-lower)))
                {:expansion word :position pos}
                (recur (inc word-end))))
            (recur (inc pos))))))))

(defn reset-dabbrev-state!
  "Reset all dabbrev state for a new expansion."
  []
  (reset! last-abbreviation nil)
  (reset! expansion-table #{})
  (reset! last-expansion nil)
  (reset! abbrev-start-pos nil)
  (reset! search-state nil))

(defn apply-case-pattern
  "Apply the case pattern of abbrev to expansion.
   - all caps abbrev -> all caps expansion
   - capitalized abbrev -> capitalized expansion
   - otherwise -> preserve expansion case"
  [abbrev expansion]
  (cond
    ;; All uppercase abbrev -> uppercase expansion
    (= abbrev (str/upper-case abbrev))
    (str/upper-case expansion)

    ;; Capitalized abbrev -> capitalize expansion
    (and (pos? (count abbrev))
         (let [first-char (subs abbrev 0 1)]
           (= first-char (str/upper-case first-char)))
         (= (subs abbrev 1) (str/lower-case (subs abbrev 1))))
    (str (str/upper-case (subs expansion 0 1)) (subs expansion 1))

    ;; Otherwise preserve original case
    :else expansion))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :dabbrev/expand
 (fn [{:keys [db]} [_]]
   "Expand the abbreviation before point (M-/).
    On repeated calls, cycle through possible expansions."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         wasm-instance (:wasm-instance buffer)
         text (.getText wasm-instance)
         cursor-pos (get-in db [:ui :cursor-position])
         last-command (:last-command db)]

     ;; Check if this is a continuation (repeated M-/)
     (if (and (= last-command :dabbrev-expand)
              @last-abbreviation
              @abbrev-start-pos)
       ;; Continuation - find next expansion
       (let [abbrev @last-abbreviation
             current-expansion @last-expansion
             search @search-state
             already-found @expansion-table

             ;; Undo the previous expansion first
             original-end (+ @abbrev-start-pos (count abbrev))
             current-end (+ @abbrev-start-pos (count (or current-expansion abbrev)))

             ;; Search for next expansion
             result (case (:direction search)
                      :backward
                      (or (find-expansion-backward text abbrev (:position search) already-found)
                          ;; Switch to forward search
                          (do (swap! search-state assoc :direction :forward :position original-end)
                              (find-expansion-forward text abbrev original-end already-found)))

                      :forward
                      (find-expansion-forward text abbrev (:position search) already-found)

                      ;; Default: start backward
                      (find-expansion-backward text abbrev @abbrev-start-pos already-found))]

         (if result
           ;; Found next expansion
           (let [{:keys [expansion position]} result
                 cased-expansion (apply-case-pattern abbrev expansion)]
             ;; Update state
             (swap! expansion-table conj (str/lower-case expansion))
             (reset! last-expansion cased-expansion)
             (swap! search-state assoc :position position)

             {:db (assoc db :last-command :dabbrev-expand)
              :fx [;; Delete old expansion and insert new one
                   [:dispatch [:editor/queue-transaction
                               {:op :replace
                                :start @abbrev-start-pos
                                :length (count (or current-expansion abbrev))
                                :text cased-expansion}]]
                   [:dispatch [:echo/message (str "Expansion: " cased-expansion)]]]})

           ;; No more expansions
           {:db (assoc db :last-command :dabbrev-expand)
            :fx [[:dispatch [:echo/message "No further expansions"]]]}))

       ;; New expansion - reset state and find first match
       (let [abbrev-info (get-abbrev-at-point text cursor-pos)]
         (if-not abbrev-info
           {:db db
            :fx [[:dispatch [:echo/message "No abbreviation at point"]]]}

           (let [{:keys [abbrev start]} abbrev-info
                 ;; Search backward first (Emacs default)
                 result (find-expansion-backward text abbrev cursor-pos #{})]

             (if result
               ;; Found expansion
               (let [{:keys [expansion position]} result
                     cased-expansion (apply-case-pattern abbrev expansion)]
                 ;; Initialize state
                 (reset! last-abbreviation abbrev)
                 (reset! expansion-table #{(str/lower-case expansion)})
                 (reset! last-expansion cased-expansion)
                 (reset! abbrev-start-pos start)
                 (reset! search-state {:direction :backward :position position})

                 {:db (assoc db :last-command :dabbrev-expand)
                  :fx [;; Replace abbrev with expansion
                       [:dispatch [:editor/queue-transaction
                                   {:op :replace
                                    :start start
                                    :length (count abbrev)
                                    :text cased-expansion}]]
                       [:dispatch [:echo/message (str "Expansion: " cased-expansion)]]]})

               ;; No expansion found backward, try forward
               (let [result-fwd (find-expansion-forward text abbrev cursor-pos #{})]
                 (if result-fwd
                   (let [{:keys [expansion position]} result-fwd
                         cased-expansion (apply-case-pattern abbrev expansion)]
                     (reset! last-abbreviation abbrev)
                     (reset! expansion-table #{(str/lower-case expansion)})
                     (reset! last-expansion cased-expansion)
                     (reset! abbrev-start-pos start)
                     (reset! search-state {:direction :forward :position position})

                     {:db (assoc db :last-command :dabbrev-expand)
                      :fx [[:dispatch [:editor/queue-transaction
                                       {:op :replace
                                        :start start
                                        :length (count abbrev)
                                        :text cased-expansion}]]
                           [:dispatch [:echo/message (str "Expansion: " cased-expansion)]]]})

                   ;; No expansion found at all
                   {:db db
                    :fx [[:dispatch [:echo/message (str "No expansion for \"" abbrev "\"")]]]}))))))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-dabbrev!
  "Initialize dabbrev commands and keybindings."
  []
  ;; Register command
  (rf/dispatch [:register-command :dabbrev-expand
                {:docstring "Expand the word before point by searching for matching words (M-/)"
                 :handler [:dabbrev/expand]}])

  ;; Set up key binding
  (rf/dispatch [:keymap/set-global "M-/" :dabbrev-expand]))
