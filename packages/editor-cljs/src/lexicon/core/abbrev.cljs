(ns lexicon.core.abbrev
  "Abbrev mode - word abbreviation expansion.

  Provides abbreviation tables that expand short abbreviations into
  longer text. Supports:
  - Global abbrevs (work in all buffers)
  - Mode-specific abbrevs (work only in specific major modes)
  - Manual expansion with C-x a e (expand-abbrev)
  - Automatic expansion on trigger characters (space, punctuation)

  Based on Emacs lisp/abbrev.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Abbrev Tables
;; =============================================================================

;; Global abbreviation table
(defonce global-abbrev-table (atom {}))

;; Mode-specific abbrev tables: {mode-name {abbrev expansion}}
(defonce mode-abbrev-tables (atom {}))

;; =============================================================================
;; Abbrev Table Operations
;; =============================================================================

(defn define-abbrev
  "Define an abbreviation in a table.
   table-atom - atom containing the abbrev table
   abbrev - the abbreviation string
   expansion - what it expands to"
  [table-atom abbrev expansion]
  (swap! table-atom assoc (str/lower-case abbrev) {:expansion expansion
                                                    :count 0}))

(defn define-global-abbrev
  "Define a global abbreviation."
  [abbrev expansion]
  (define-abbrev global-abbrev-table abbrev expansion))

(defn define-mode-abbrev
  "Define a mode-specific abbreviation."
  [mode abbrev expansion]
  (when-not (contains? @mode-abbrev-tables mode)
    (swap! mode-abbrev-tables assoc mode {}))
  (swap! mode-abbrev-tables assoc-in [mode (str/lower-case abbrev)]
         {:expansion expansion :count 0}))

(defn get-abbrev-expansion
  "Look up an abbreviation, checking mode-specific then global tables.
   Returns {:expansion string :table :global|:mode} or nil."
  [abbrev mode]
  (let [abbrev-lower (str/lower-case abbrev)]
    (or
     ;; Check mode-specific table first
     (when-let [mode-table (get @mode-abbrev-tables mode)]
       (when-let [entry (get mode-table abbrev-lower)]
         {:expansion (:expansion entry) :table :mode}))
     ;; Then check global table
     (when-let [entry (get @global-abbrev-table abbrev-lower)]
       {:expansion (:expansion entry) :table :global}))))

(defn clear-abbrev-table
  "Clear all abbreviations from a table."
  [table-atom]
  (reset! table-atom {}))

;; =============================================================================
;; Abbrev Expansion
;; =============================================================================

(defn get-word-before-point
  "Get the word before the cursor position."
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

(defn get-active-buffer-id
  "Get the active buffer ID from db."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))]
    (:buffer-id active-window)))

(defn get-buffer-mode
  "Get the major mode of the current buffer."
  [db]
  (let [buffer-id (get-active-buffer-id db)
        buffer (get-in db [:buffers buffer-id])]
    (:mode buffer "fundamental")))

;; =============================================================================
;; Commands
;; =============================================================================

(rf/reg-event-fx
 :abbrev/expand-abbrev
 (fn [{:keys [db]} [_]]
   (let [buffer-id (get-active-buffer-id db)
         buffer-state (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer-state)
         text (when wasm (.getText wasm))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         mode (get-buffer-mode db)]
     (if-let [{:keys [word start]} (get-word-before-point text cursor-pos)]
       (if-let [{:keys [expansion]} (get-abbrev-expansion word mode)]
         ;; Found expansion - replace the abbreviation
         (let [delete-length (- cursor-pos start)]
           {:db db
            :fx [[:dispatch [:editor/queue-transaction
                             {:op :replace
                              :start start
                              :length delete-length
                              :text expansion}]]
                 [:dispatch [:echo/message (str "Expanded: " word " -> " expansion)]]]})
         ;; No expansion found
         {:db db
          :fx [[:dispatch [:echo/message "No expansion for this abbrev"]]]})
       ;; No word before point
       {:db db
        :fx [[:dispatch [:echo/message "No abbrev to expand"]]]}))))

(rf/reg-event-fx
 :abbrev/add-global-abbrev
 (fn [{:keys [db]} [_ abbrev expansion]]
   (define-global-abbrev abbrev expansion)
   {:db db
    :fx [[:dispatch [:echo/message (str "Defined global abbrev: " abbrev " -> " expansion)]]]}))

(rf/reg-event-fx
 :abbrev/add-mode-abbrev
 (fn [{:keys [db]} [_ abbrev expansion]]
   (let [mode (get-buffer-mode db)]
     (define-mode-abbrev mode abbrev expansion)
     {:db db
      :fx [[:dispatch [:echo/message (str "Defined " mode " abbrev: " abbrev " -> " expansion)]]]})))

(rf/reg-event-fx
 :abbrev/list-abbrevs
 (fn [{:keys [db]} [_]]
   (let [global-abbrevs @global-abbrev-table
         mode-abbrevs @mode-abbrev-tables
         lines (atom [])]
     ;; Add global abbrevs
     (when (seq global-abbrevs)
       (swap! lines conj "Global abbrevs:")
       (doseq [[abbrev {:keys [expansion]}] (sort-by first global-abbrevs)]
         (swap! lines conj (str "  " abbrev " -> " expansion))))
     ;; Add mode abbrevs
     (doseq [[mode table] (sort-by first mode-abbrevs)]
       (when (seq table)
         (swap! lines conj (str mode " abbrevs:"))
         (doseq [[abbrev {:keys [expansion]}] (sort-by first table)]
           (swap! lines conj (str "  " abbrev " -> " expansion)))))
     (if (seq @lines)
       {:db db
        :fx [[:dispatch [:echo/message (str/join "\n" @lines)]]]}
       {:db db
        :fx [[:dispatch [:echo/message "No abbrevs defined"]]]}))))

;; =============================================================================
;; Abbrev Mode Toggle
;; =============================================================================

(rf/reg-event-fx
 :abbrev/toggle-mode
 (fn [{:keys [db]} [_]]
   (let [buffer-id (get-active-buffer-id db)
         current-abbrev-mode (get-in db [:buffers buffer-id :abbrev-mode] false)
         new-mode (not current-abbrev-mode)]
     {:db (assoc-in db [:buffers buffer-id :abbrev-mode] new-mode)
      :fx [[:dispatch [:echo/message (if new-mode
                                       "Abbrev mode enabled"
                                       "Abbrev mode disabled")]]]})))

;; =============================================================================
;; Default Abbrevs (Examples)
;; =============================================================================

(defn install-default-abbrevs!
  "Install some default abbreviations for demonstration."
  []
  ;; Common typos
  (define-global-abbrev "teh" "the")
  (define-global-abbrev "adn" "and")
  (define-global-abbrev "taht" "that")
  (define-global-abbrev "wiht" "with")

  ;; Programming shortcuts for clojure mode
  (define-mode-abbrev "clojure" "defn" "(defn  [])")
  (define-mode-abbrev "clojure" "defp" "(defn-  [])")
  (define-mode-abbrev "clojure" "letb" "(let [])"))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize abbrev mode commands and keybindings."
  []
  ;; Register commands
  (rf/dispatch [:register-command :expand-abbrev
                {:docstring "Expand abbrev before point (C-x a e)"
                 :handler [:abbrev/expand-abbrev]}])

  (rf/dispatch [:register-command :abbrev-mode
                {:docstring "Toggle abbrev mode in current buffer"
                 :handler [:abbrev/toggle-mode]}])

  (rf/dispatch [:register-command :list-abbrevs
                {:docstring "List all defined abbreviations"
                 :handler [:abbrev/list-abbrevs]}])

  ;; Install default abbrevs
  (install-default-abbrevs!))
