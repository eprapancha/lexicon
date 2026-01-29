(ns lexicon.core.events.isearch
  "Incremental search (isearch) implementation - C-s and C-r

  Uses lexicon.lisp primitives for buffer access following Emacs architecture.

  Phase 1: Basic isearch
  - Activate isearch mode
  - Accumulate search string
  - Search incrementally on each keystroke
  - RET to exit, C-g to abort

  Phase 2: Repeat search
  - C-s/C-r to find next/previous
  - DEL to backtrack
  - Buffer wrap-around

  Phase 3: Polish
  - Case sensitivity
  - Failure/wrapped messages
  - Command keys exit isearch"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- case-fold-search?
  "Return true if search should be case-insensitive.
   Emacs convention: case-insensitive if search string is all lowercase."
  [string]
  (= string (str/lower-case string)))

(defn find-next-match
  "Find next match for search-string in text starting from pos.
   Returns {:start pos :end pos} or nil if no match found.
   case-fold? controls case-insensitive matching."
  [text search-string pos case-fold?]
  (when-not (str/blank? search-string)
    (let [search-text (if case-fold? (str/lower-case text) text)
          search-term (if case-fold? (str/lower-case search-string) search-string)
          text-from-pos (subs search-text pos)
          match-index (.indexOf text-from-pos search-term)]
      (when (>= match-index 0)
        (let [abs-start (+ pos match-index)
              abs-end (+ abs-start (count search-string))]
          {:start abs-start :end abs-end})))))

(defn find-prev-match
  "Find previous match for search-string in text before pos.
   Returns {:start pos :end pos} or nil if no match found."
  [text search-string pos case-fold?]
  (when-not (str/blank? search-string)
    (let [search-text (if case-fold? (str/lower-case text) text)
          search-term (if case-fold? (str/lower-case search-string) search-string)
          text-before-pos (subs search-text 0 pos)
          match-index (.lastIndexOf text-before-pos search-term)]
      (when (>= match-index 0)
        (let [abs-start match-index
              abs-end (+ abs-start (count search-string))]
          {:start abs-start :end abs-end})))))

(defn isearch-echo-message
  "Create echo message dispatch event for isearch status"
  [search-string failing? wrapped?]
  (let [prefix (cond
                 failing? "Failing I-search"
                 wrapped? "Overwrapped I-search"
                 :else "I-search")
        message (str prefix ": " search-string)]
    [:dispatch [:echo/message message]]))

;; =============================================================================
;; Phase 1: Basic Isearch
;; =============================================================================

(rf/reg-event-fx
 :isearch-forward
 (fn [{:keys [db]} [_]]
   "Start incremental search forward (C-s)"
   (let [current-pos (lisp/point)]
     {:db (-> db
              (assoc-in [:ui :isearch :active?] true)
              (assoc-in [:ui :isearch :direction] :forward)
              (assoc-in [:ui :isearch :search-string] "")
              (assoc-in [:ui :isearch :original-pos] current-pos)
              (assoc-in [:ui :isearch :match-history] [])
              (assoc-in [:ui :isearch :wrapped?] false)
              (assoc-in [:ui :isearch :failing?] false))
      :fx [[:dispatch [:minibuffer/activate
                       {:prompt "I-search: "
                        :input ""
                        :on-change [:isearch/minibuffer-update]
                        :on-confirm [:isearch/exit]
                        :on-cancel [:isearch/abort]}]]
           (isearch-echo-message "" false false)]})))

(rf/reg-event-fx
 :isearch-backward
 (fn [{:keys [db]} [_]]
   "Start incremental search backward (C-r)"
   (let [current-pos (lisp/point)]
     {:db (-> db
              (assoc-in [:ui :isearch :active?] true)
              (assoc-in [:ui :isearch :direction] :backward)
              (assoc-in [:ui :isearch :search-string] "")
              (assoc-in [:ui :isearch :original-pos] current-pos)
              (assoc-in [:ui :isearch :match-history] [])
              (assoc-in [:ui :isearch :wrapped?] false)
              (assoc-in [:ui :isearch :failing?] false))
      :fx [[:dispatch [:minibuffer/activate
                       {:prompt "I-search backward: "
                        :input ""
                        :on-change [:isearch/minibuffer-update]
                        :on-confirm [:isearch/exit]
                        :on-cancel [:isearch/abort]}]]
           (isearch-echo-message "" false false)]})))

(rf/reg-event-fx
 :isearch/minibuffer-update
 (fn [{:keys [db]} [_ new-input]]
   "Handle minibuffer input changes during isearch"
   (let [isearch-state (get-in db [:ui :isearch])
         direction (:direction isearch-state)
         full-text (lisp/buffer-string)
         original-pos (:original-pos isearch-state)
         case-fold? (case-fold-search? new-input)

         ;; Search from original position
         match (if (= direction :forward)
                 (find-next-match full-text new-input original-pos case-fold?)
                 (find-prev-match full-text new-input original-pos case-fold?))]

     (if match
       ;; Found match - move to it
       {:db (-> db
                (assoc-in [:ui :isearch :search-string] new-input)
                (assoc-in [:ui :isearch :current-match] match)
                (assoc-in [:ui :isearch :failing?] false))
        :fx [[:dispatch [:cursor/set-position (:start match)]]
             [:dispatch [:minibuffer/set-input new-input]]
             (isearch-echo-message new-input false false)]}
       ;; No match - mark as failing
       {:db (-> db
                (assoc-in [:ui :isearch :search-string] new-input)
                (assoc-in [:ui :isearch :failing?] true))
        :fx [[:dispatch [:minibuffer/set-input new-input]]
             (isearch-echo-message new-input true false)]}))))

(rf/reg-event-fx
 :isearch/handle-key
 (fn [{:keys [db]} [_ key-str]]
   "Handle key press during isearch mode"
   (let [isearch-state (get-in db [:ui :isearch])
         search-string (:search-string isearch-state)]
     (cond
       ;; RET - exit isearch and stay at current match
       (= key-str "RET")
       {:fx [[:dispatch [:isearch/exit]]]}

       ;; C-g - abort isearch and return to original position
       (= key-str "C-g")
       {:fx [[:dispatch [:isearch/abort]]]}

       ;; C-s - repeat search forward (or start if called during isearch)
       (= key-str "C-s")
       {:fx [[:dispatch [:isearch/repeat-forward]]]}

       ;; C-r - repeat search backward
       (= key-str "C-r")
       {:fx [[:dispatch [:isearch/repeat-backward]]]}

       ;; DEL/Backspace - remove last character and backtrack
       (or (= key-str "DEL") (= key-str "Backspace"))
       {:fx [[:dispatch [:isearch/delete-char]]]}

       ;; Printable character - add to search string
       (and (= (count key-str) 1)
            (>= (.charCodeAt key-str 0) 32))
       {:fx [[:dispatch [:isearch/add-char key-str]]]}

       ;; Any other command - exit isearch and execute command
       :else
       {:fx [[:dispatch [:isearch/exit]]
             [:dispatch [:handle-key-sequence key-str]]]}))))

(rf/reg-event-fx
 :isearch/add-char
 (fn [{:keys [db]} [_ char]]
   "Add character to search string and search incrementally"
   (let [isearch-state (get-in db [:ui :isearch])
         search-string (:search-string isearch-state)
         direction (:direction isearch-state)
         new-search (str search-string char)
         full-text (lisp/buffer-string)
         current-pos (lisp/point)
         case-fold? (case-fold-search? new-search)

         ;; Search from current position
         match (if (= direction :forward)
                 (find-next-match full-text new-search current-pos case-fold?)
                 (find-prev-match full-text new-search current-pos case-fold?))]

     (if match
       ;; Found match - move to it
       {:db (-> db
                (assoc-in [:ui :isearch :search-string] new-search)
                (assoc-in [:ui :isearch :current-match] match)
                (assoc-in [:ui :isearch :failing?] false))
        :fx [[:dispatch [:cursor/set-position (:start match)]]
             (isearch-echo-message new-search false false)]}
       ;; No match - mark as failing
       {:db (-> db
                (assoc-in [:ui :isearch :search-string] new-search)
                (assoc-in [:ui :isearch :failing?] true))
        :fx [(isearch-echo-message new-search true false)]}))))

(rf/reg-event-fx
 :isearch/delete-char
 (fn [{:keys [db]} [_]]
   "Remove last character from search string and backtrack"
   (let [isearch-state (get-in db [:ui :isearch])
         search-string (:search-string isearch-state)]
     (if (empty? search-string)
       ;; Already empty - do nothing
       {:db db}
       ;; Remove last character and re-search
       (let [new-search (subs search-string 0 (dec (count search-string)))]
         (if (empty? new-search)
           ;; Removed all characters - reset to beginning
           {:db (-> db
                    (assoc-in [:ui :isearch :search-string] "")
                    (assoc-in [:ui :isearch :current-match] nil)
                    (assoc-in [:ui :isearch :failing?] false)
                    (assoc-in [:ui :isearch :wrapped?] false))
            :fx [(isearch-echo-message "" false false)]}
           ;; Re-search with shorter string
           (let [full-text (lisp/buffer-string)
                 original-pos (:original-pos isearch-state)
                 direction (:direction isearch-state)
                 case-fold? (case-fold-search? new-search)

                 ;; Search from original position
                 match (if (= direction :forward)
                         (find-next-match full-text new-search original-pos case-fold?)
                         (find-prev-match full-text new-search original-pos case-fold?))]

             (if match
               {:db (-> db
                        (assoc-in [:ui :isearch :search-string] new-search)
                        (assoc-in [:ui :isearch :current-match] match)
                        (assoc-in [:ui :isearch :failing?] false))
                :fx [[:dispatch [:cursor/set-position (:start match)]]
                     (isearch-echo-message new-search false false)]}
               {:db (-> db
                        (assoc-in [:ui :isearch :search-string] new-search)
                        (assoc-in [:ui :isearch :failing?] true))
                :fx [(isearch-echo-message new-search true false)]}))))))))

(rf/reg-event-fx
 :isearch/repeat-forward
 (fn [{:keys [db]} [_]]
   "Find next occurrence (C-s during isearch)"
   (let [isearch-state (get-in db [:ui :isearch])
         search-string (:search-string isearch-state)
         current-match (:current-match isearch-state)
         full-text (lisp/buffer-string)
         case-fold? (case-fold-search? search-string)

         ;; Search from after current match
         search-pos (if current-match (:end current-match) (lisp/point))
         match (find-next-match full-text search-string search-pos case-fold?)]

     (if match
       ;; Found next match
       {:db (-> db
                (assoc-in [:ui :isearch :current-match] match)
                (assoc-in [:ui :isearch :failing?] false))
        :fx [[:dispatch [:cursor/set-position (:start match)]]
             (isearch-echo-message search-string false false)]}
       ;; No match - try wrapping from beginning
       (let [wrapped-match (find-next-match full-text search-string 0 case-fold?)]
         (if wrapped-match
           {:db (-> db
                    (assoc-in [:ui :isearch :current-match] wrapped-match)
                    (assoc-in [:ui :isearch :wrapped?] true)
                    (assoc-in [:ui :isearch :failing?] false))
            :fx [[:dispatch [:cursor/set-position (:start wrapped-match)]]
                 (isearch-echo-message search-string false true)]}
           {:db (-> db
                    (assoc-in [:ui :isearch :failing?] true))
            :fx [(isearch-echo-message search-string true false)]}))))))

(rf/reg-event-fx
 :isearch/repeat-backward
 (fn [{:keys [db]} [_]]
   "Find previous occurrence (C-r during isearch)"
   (let [isearch-state (get-in db [:ui :isearch])
         search-string (:search-string isearch-state)
         current-match (:current-match isearch-state)
         full-text (lisp/buffer-string)
         case-fold? (case-fold-search? search-string)

         ;; Search backwards from before current match
         search-pos (if current-match (:start current-match) (lisp/point))
         match (find-prev-match full-text search-string search-pos case-fold?)]

     (if match
       ;; Found previous match
       {:db (-> db
                (assoc-in [:ui :isearch :current-match] match)
                (assoc-in [:ui :isearch :failing?] false))
        :fx [[:dispatch [:cursor/set-position (:start match)]]
             (isearch-echo-message search-string false false)]}
       ;; No match - try wrapping from end
       (let [text-length (count full-text)
             wrapped-match (find-prev-match full-text search-string text-length case-fold?)]
         (if wrapped-match
           {:db (-> db
                    (assoc-in [:ui :isearch :current-match] wrapped-match)
                    (assoc-in [:ui :isearch :wrapped?] true)
                    (assoc-in [:ui :isearch :failing?] false))
            :fx [[:dispatch [:cursor/set-position (:start wrapped-match)]]
                 (isearch-echo-message search-string false true)]}
           {:db (-> db
                    (assoc-in [:ui :isearch :failing?] true))
            :fx [(isearch-echo-message search-string true false)]}))))))

(rf/reg-event-fx
 :isearch/exit
 (fn [{:keys [db]} [_]]
   "Exit isearch and stay at current match (RET).
    Emacs behavior:
    - Forward search (C-s): cursor at END of match
    - Backward search (C-r): cursor at START of match"
   (let [current-match (get-in db [:ui :isearch :current-match])
         direction (get-in db [:ui :isearch :direction] :forward)
         ;; Position cursor based on search direction
         exit-pos (when current-match
                    (if (= direction :forward)
                      (:end current-match)    ; Forward: end of match
                      (:start current-match)))] ; Backward: start of match
     {:db (-> db
              (update :ui dissoc :isearch))
      :fx (cond-> [[:dispatch [:minibuffer/deactivate]]
                   [:dispatch [:echo/clear]]]
            ;; Move cursor to appropriate position if we have a match
            exit-pos (conj [:dispatch [:cursor/set-position exit-pos]]))})))

(rf/reg-event-fx
 :isearch/abort
 (fn [{:keys [db]} [_]]
   "Abort isearch and return to original position (C-g)"
   (let [original-pos (get-in db [:ui :isearch :original-pos] 0)]
     {:db (-> db
              (update :ui dissoc :isearch))
      :fx [[:dispatch [:cursor/set-position original-pos]]
           [:dispatch [:minibuffer/deactivate]]
           [:dispatch [:echo/clear]]]})))
