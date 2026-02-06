(ns lexicon.core.hippie-expand
  "Hippie expand - try multiple expansion strategies.

  M-/ (or hippie-expand) tries various ways to complete the text before point.
  The list of expansion strategies is defined in `hippie-expand-try-functions-list`.

  Repeated calls cycle through possible expansions from all strategies.

  Based on Emacs lisp/hippie-exp.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def default-try-functions
  "Default list of expansion functions to try.
   Each function is a keyword that maps to an expansion strategy."
  [:try-expand-dabbrev
   :try-expand-dabbrev-all-buffers
   :try-expand-line
   :try-complete-file-name])

;; =============================================================================
;; State
;; =============================================================================

;; Current position in the try-functions list
(defonce current-try-index (atom 0))

;; Whether we're in the middle of a hippie-expand sequence
(defonce hippie-expand-active? (atom false))

;; The original text before any expansion
(defonce original-text (atom nil))

;; Position where expansion starts
(defonce expand-start-pos (atom nil))

;; List of expansions we've already tried (to avoid duplicates)
(defonce tried-expansions (atom #{}))

;; =============================================================================
;; Try Functions
;; =============================================================================

(defn get-word-before-point
  "Get the word (or partial word) before the cursor."
  [text cursor-pos]
  (when (and text (pos? cursor-pos) (<= cursor-pos (count text)))
    (let [before-cursor (subs text 0 cursor-pos)]
      (loop [pos (dec (count before-cursor))]
        (if (neg? pos)
          {:word before-cursor :start 0}
          (let [ch (nth before-cursor pos)]
            (if (re-matches #"[\w\-_]" (str ch))
              (recur (dec pos))
              {:word (subs before-cursor (inc pos))
               :start (inc pos)})))))))

(defn escape-regex
  "Escape special regex characters in a string."
  [s]
  (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn find-dabbrev-expansion
  "Find a dabbrev expansion in the given text.
   Returns {:expansion string :position N} or nil."
  [text word cursor-pos already-tried direction]
  (let [pattern (re-pattern (str "\\b" (escape-regex word) "(\\w+)"))
        search-text (if (= direction :backward)
                      (subs text 0 cursor-pos)
                      (subs text cursor-pos))]
    (when-let [matches (re-seq pattern search-text)]
      (let [expansions (->> matches
                            (map (fn [[_ suffix]] (str word suffix)))
                            (remove already-tried)
                            distinct)]
        (when (seq expansions)
          {:expansion (first expansions)
           :remaining (rest expansions)})))))

(defn get-active-buffer-id
  "Get the active buffer ID from db."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))]
    (:buffer-id active-window)))

(defn try-expand-dabbrev
  "Try to expand using dabbrev in current buffer.
   Returns {:expansion string} or nil."
  [db already-tried]
  (let [active-buffer-id (get-active-buffer-id db)
        buffer-state (get-in db [:buffers active-buffer-id])
        wasm (:wasm-instance buffer-state)
        text (when wasm (.getText wasm))
        cursor-pos (get-in db [:ui :cursor-position] 0)]
    (when-let [{:keys [word start]} (get-word-before-point text cursor-pos)]
      (when (and word (>= (count word) 1))
        (or (find-dabbrev-expansion text word start already-tried :backward)
            (find-dabbrev-expansion text word cursor-pos already-tried :forward))))))

(defn try-expand-dabbrev-all-buffers
  "Try to expand using dabbrev across all buffers."
  [db already-tried]
  (let [active-buffer-id (get-active-buffer-id db)
        buffer-state (get-in db [:buffers active-buffer-id])
        wasm (:wasm-instance buffer-state)
        text (when wasm (.getText wasm))
        cursor-pos (get-in db [:ui :cursor-position] 0)]
    (when-let [{:keys [word]} (get-word-before-point text cursor-pos)]
      (when (and word (>= (count word) 1))
        ;; Search other buffers
        (let [other-buffers (dissoc (:buffers db) active-buffer-id)]
          (some (fn [[_id buf]]
                  (when-let [buf-wasm (:wasm-instance buf)]
                    (let [buf-text (.getText buf-wasm)]
                      (find-dabbrev-expansion buf-text word 0 already-tried :forward))))
                other-buffers))))))

(defn try-expand-line
  "Try to expand to a complete line that starts with the current text."
  [db already-tried]
  (let [active-buffer-id (get-active-buffer-id db)
        buffer-state (get-in db [:buffers active-buffer-id])
        wasm (:wasm-instance buffer-state)
        text (when wasm (.getText wasm))
        cursor-pos (get-in db [:ui :cursor-position] 0)]
    (when text
      ;; Find start of current line
      (let [line-start (loop [pos (dec cursor-pos)]
                         (if (or (neg? pos) (= (nth text pos) \newline))
                           (inc pos)
                           (recur (dec pos))))
            current-line-prefix (subs text line-start cursor-pos)
            lines (str/split-lines text)]
        (when (and current-line-prefix (seq current-line-prefix))
          (let [matching-lines (->> lines
                                    (filter #(and (str/starts-with? % current-line-prefix)
                                                  (not= % current-line-prefix)
                                                  (not (contains? already-tried %))))
                                    distinct)]
            (when (seq matching-lines)
              {:expansion (first matching-lines)
               :line-expansion? true})))))))

(defn try-complete-file-name
  "Try to complete as a file name - placeholder."
  [_db _already-tried]
  ;; File completion would require filesystem access
  ;; For now, return nil
  nil)

(defn dispatch-try-function
  "Call the appropriate try function."
  [try-fn db already-tried]
  (case try-fn
    :try-expand-dabbrev (try-expand-dabbrev db already-tried)
    :try-expand-dabbrev-all-buffers (try-expand-dabbrev-all-buffers db already-tried)
    :try-expand-line (try-expand-line db already-tried)
    :try-complete-file-name (try-complete-file-name db already-tried)
    nil))

;; =============================================================================
;; Main Hippie Expand Logic
;; =============================================================================

(defn reset-hippie-state!
  "Reset hippie expand state for a new expansion."
  []
  (reset! current-try-index 0)
  (reset! hippie-expand-active? false)
  (reset! original-text nil)
  (reset! expand-start-pos nil)
  (reset! tried-expansions #{}))

(rf/reg-event-fx
 :hippie-expand/expand
 (fn [{:keys [db]} [_]]
   (let [last-command (:last-command db)
         continuing? (= last-command :hippie-expand)
         active-buffer-id (get-active-buffer-id db)
         buffer-state (get-in db [:buffers active-buffer-id])
         wasm (:wasm-instance buffer-state)
         text (when wasm (.getText wasm))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         try-functions default-try-functions]

     ;; Initialize state if starting fresh
     (when-not continuing?
       (reset-hippie-state!)
       (reset! hippie-expand-active? true)
       (when-let [{:keys [word start]} (get-word-before-point text cursor-pos)]
         (reset! original-text word)
         (reset! expand-start-pos start)))

     ;; Try functions in order
     (loop [idx @current-try-index]
       (if (>= idx (count try-functions))
         ;; Exhausted all try-functions
         (do
           (reset-hippie-state!)
           {:db (assoc db :last-command :hippie-expand)
            :fx [[:dispatch [:echo/message "No expansion found"]]]})

         (let [try-fn (nth try-functions idx)
               result (dispatch-try-function try-fn db @tried-expansions)]
           (if result
             ;; Found an expansion
             (let [{:keys [expansion line-expansion?]} result
                   word-info (get-word-before-point text cursor-pos)
                   start-pos (if line-expansion?
                               ;; For line expansion, find line start
                               (loop [pos (dec cursor-pos)]
                                 (if (or (neg? pos) (= (nth text pos) \newline))
                                   (inc pos)
                                   (recur (dec pos))))
                               (:start word-info))
                   delete-length (- cursor-pos start-pos)]

               ;; Track this expansion
               (swap! tried-expansions conj expansion)
               (reset! current-try-index idx)

               {:db (assoc db :last-command :hippie-expand)
                :fx [[:dispatch [:editor/queue-transaction
                                 {:op :replace
                                  :start start-pos
                                  :length delete-length
                                  :text expansion}]]
                     [:dispatch [:echo/message (str "Expansion: " expansion
                                                    " [" (name try-fn) "]")]]]})

             ;; This try-function failed, try next
             (do
               (swap! current-try-index inc)
               (recur (inc idx))))))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize hippie-expand commands and keybindings."
  []
  (rf/dispatch [:register-command :hippie-expand
                {:docstring "Expand text trying various methods (M-/)"
                 :handler [:hippie-expand/expand]}])

  ;; Don't override M-/ since dabbrev already uses it
  ;; Users can bind hippie-expand to a different key or use M-x
  )
