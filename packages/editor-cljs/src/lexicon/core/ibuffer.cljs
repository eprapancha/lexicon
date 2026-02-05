(ns lexicon.core.ibuffer
  "IBuffer - advanced buffer management with filtering and grouping.

  Implements Emacs ibuffer.el functionality:
  - Filter groups by mode, name pattern, directory
  - Multiple sorting modes (name, size, mode, recency)
  - Bulk operations on filtered/marked buffers
  - Customizable column display

  Commands:
  - ibuffer: Open ibuffer interface
  - ibuffer-filter-by-mode: Filter buffers by major mode
  - ibuffer-filter-by-name: Filter buffers by name regexp
  - ibuffer-do-sort-by-alphabetic: Sort by name
  - ibuffer-do-sort-by-size: Sort by size
  - ibuffer-do-sort-by-major-mode: Sort by mode
  - ibuffer-toggle-sorting-mode: Cycle sorting
  - ibuffer-filter-disable: Remove all filters

  Based on Emacs lisp/ibuffer.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ibuffer-sorting-modes
  "Available sorting modes."
  [:alphabetic :size :major-mode :recency])

;; =============================================================================
;; State
;; =============================================================================

;; ibuffer state stored in db at [:ibuffer]
;; {:sorting-mode :alphabetic
;;  :sorting-reversed? false
;;  :filters []  ; [{:type :mode :value :clojure-mode} ...]
;;  :filter-groups []  ; [{:name "Code" :filters [{:type :mode ...}]} ...]
;;  :marks {line-number "D"}}

(defonce ibuffer-marks (atom {}))

;; =============================================================================
;; Buffer Info Extraction
;; =============================================================================

(defn- buffer-size
  "Get buffer size from WASM instance."
  [buffer]
  (if-let [wasm (:wasm-instance buffer)]
    (try (.-length wasm) (catch :default _ 0))
    0))

(defn- buffer-file-name
  "Get buffer file name or empty string."
  [buffer]
  (or (:file-path buffer) ""))

(defn- buffer-info
  "Extract display info from a buffer."
  [[id buffer]]
  {:id id
   :name (:name buffer "")
   :size (buffer-size buffer)
   :mode (:major-mode buffer :fundamental-mode)
   :modified? (:is-modified? buffer false)
   :read-only? (:is-read-only? buffer false)
   :file (buffer-file-name buffer)})

;; =============================================================================
;; Filtering
;; =============================================================================

(defn- matches-filter?
  "Check if a buffer info matches a single filter."
  [buf-info filter-spec]
  (case (:type filter-spec)
    :mode (= (:mode buf-info) (:value filter-spec))
    :name (re-find (re-pattern (:value filter-spec)) (:name buf-info))
    :filename (re-find (re-pattern (:value filter-spec)) (:file buf-info))
    :size (case (:op filter-spec)
            :> (> (:size buf-info) (:value filter-spec))
            :< (< (:size buf-info) (:value filter-spec))
            false)
    :starred (str/starts-with? (:name buf-info) "*")
    :visiting-file (not (str/blank? (:file buf-info)))
    ;; Default: match all
    true))

(defn- apply-filters
  "Apply list of filters to buffer infos."
  [buf-infos filters]
  (if (empty? filters)
    buf-infos
    (filter (fn [buf-info]
              (every? #(matches-filter? buf-info %) filters))
            buf-infos)))

;; =============================================================================
;; Sorting
;; =============================================================================

(defn- sort-buffers
  "Sort buffer infos by the given mode."
  [buf-infos sorting-mode reversed?]
  (let [sorted (case sorting-mode
                 :alphabetic (sort-by #(str/lower-case (:name %)) buf-infos)
                 :size (sort-by :size buf-infos)
                 :major-mode (sort-by #(name (:mode %)) buf-infos)
                 :recency buf-infos  ; Already in recency order from db
                 buf-infos)]
    (if reversed?
      (reverse sorted)
      sorted)))

;; =============================================================================
;; Filter Groups
;; =============================================================================

(def default-filter-groups
  "Default ibuffer filter groups."
  [{:name "Emacs"
    :filters [{:type :starred}]}])

(defn- group-buffers
  "Group buffers according to filter groups.
   Returns [{:name string :buffers [buf-info ...]} ...]"
  [buf-infos filter-groups]
  (if (empty? filter-groups)
    [{:name nil :buffers buf-infos}]
    (loop [remaining buf-infos
           groups filter-groups
           result []]
      (if (empty? groups)
        ;; Remaining buffers go in default group
        (if (seq remaining)
          (conj result {:name "Default" :buffers (vec remaining)})
          result)
        (let [group (first groups)
              {matched true unmatched false}
              (group-by (fn [buf]
                          (every? #(matches-filter? buf %) (:filters group)))
                        remaining)]
          (recur (or unmatched [])
                 (rest groups)
                 (if (seq matched)
                   (conj result {:name (:name group) :buffers (vec matched)})
                   result)))))))

;; =============================================================================
;; Display Formatting
;; =============================================================================

(defn- pad-string
  "Pad a string to a given width."
  [s width]
  (let [s (str s)
        len (count s)]
    (if (>= len width)
      (subs s 0 width)
      (str s (apply str (repeat (- width len) " "))))))

(defn- right-pad
  "Right-align a string in given width."
  [s width]
  (let [s (str s)
        len (count s)]
    (if (>= len width)
      s
      (str (apply str (repeat (- width len) " ")) s))))

(defn- format-buffer-line
  "Format a single buffer info as an ibuffer line."
  [buf-info]
  (let [mark " "
        modified (if (:modified? buf-info) "*" " ")
        readonly (if (:read-only? buf-info) "%" " ")
        name-str (pad-string (:name buf-info) 30)
        size-str (right-pad (str (:size buf-info)) 7)
        mode-str (pad-string (name (:mode buf-info)) 16)
        file-str (:file buf-info)]
    (str mark modified readonly " " name-str " " size-str " " mode-str " " file-str)))

(defn- format-group-header
  "Format a filter group header line."
  [group-name]
  (str "[ " group-name " ]"))

(defn- generate-ibuffer-content
  "Generate the complete ibuffer content."
  [db]
  (let [buffers (:buffers db)
        sorting-mode (get-in db [:ibuffer :sorting-mode] :recency)
        reversed? (get-in db [:ibuffer :sorting-reversed?] false)
        filters (get-in db [:ibuffer :filters] [])
        filter-groups (get-in db [:ibuffer :filter-groups] [])
        ;; Build buffer info list
        buf-infos (map buffer-info buffers)
        ;; Apply filters
        filtered (apply-filters buf-infos filters)
        ;; Sort
        sorted (sort-buffers filtered sorting-mode reversed?)
        ;; Group
        groups (group-buffers sorted filter-groups)
        ;; Header
        header (str "  " (pad-string "MR Buffer" 33)
                    " " (right-pad "Size" 7)
                    " " (pad-string "Mode" 16)
                    " Filename")
        separator (apply str (repeat (count header) "-"))]
    (str header "\n" separator "\n"
         (str/join "\n"
                   (mapcat (fn [group]
                             (concat
                              (when (:name group)
                                [(format-group-header (:name group))])
                              (map format-buffer-line (:buffers group))))
                           groups)))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :ibuffer/open
 (fn [{:keys [db]} [_]]
   "Open ibuffer interface."
   (let [content (generate-ibuffer-content db)
         buffer-name "*Ibuffer*"
         buffers (:buffers db)
         existing-id (some (fn [[id buf]]
                             (when (= (:name buf) buffer-name) id))
                           buffers)
         buffer-id (or existing-id
                       (db/next-buffer-id buffers))
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))]
     (reset! ibuffer-marks {})
     (if-not wasm-instance
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
       (let [lines (str/split content #"\n" -1)
             line-count (count lines)]
         {:db (-> db
                  (assoc-in [:buffers buffer-id]
                            {:id buffer-id
                             :name buffer-name
                             :wasm-instance wasm-instance
                             :file-handle nil
                             :major-mode :ibuffer-mode
                             :is-read-only? true
                             :is-modified? false
                             :mark-position nil
                             :cursor-position {:line 0 :column 0}
                             :selection-range nil
                             :minor-modes #{}
                             :buffer-local-vars {}
                             :ast nil
                             :language :text
                             :diagnostics []
                             :undo-stack []
                             :undo-in-progress? false
                             :editor-version 0
                             :text-properties {}
                             :overlays {}
                             :next-overlay-id 1
                             :cache {:text content
                                     :line-count line-count}}))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})))))

(rf/reg-event-fx
 :ibuffer/revert
 (fn [{:keys [db]} [_]]
   "Refresh ibuffer content."
   (let [active-window (get-in db [:window-tree])
         buffer-id (when (= (:type active-window) :leaf)
                     (:buffer-id active-window))
         content (generate-ibuffer-content db)
         ^js wasm (get-in db [:buffers buffer-id :wasm-instance])]
     (when wasm
       (let [current-len (.-length wasm)]
         (.delete wasm 0 current-len)
         (.insert wasm 0 content)))
     {:fx [[:dispatch [:echo/message "IBuffer refreshed"]]]})))

;; -- Sorting Events --

(rf/reg-event-fx
 :ibuffer/sort-by-alphabetic
 (fn [{:keys [db]} [_]]
   {:db (assoc-in db [:ibuffer :sorting-mode] :alphabetic)
    :fx [[:dispatch [:ibuffer/revert]]
         [:dispatch [:echo/message "Sorting by name"]]]}))

(rf/reg-event-fx
 :ibuffer/sort-by-size
 (fn [{:keys [db]} [_]]
   {:db (assoc-in db [:ibuffer :sorting-mode] :size)
    :fx [[:dispatch [:ibuffer/revert]]
         [:dispatch [:echo/message "Sorting by size"]]]}))

(rf/reg-event-fx
 :ibuffer/sort-by-major-mode
 (fn [{:keys [db]} [_]]
   {:db (assoc-in db [:ibuffer :sorting-mode] :major-mode)
    :fx [[:dispatch [:ibuffer/revert]]
         [:dispatch [:echo/message "Sorting by major mode"]]]}))

(rf/reg-event-fx
 :ibuffer/sort-by-recency
 (fn [{:keys [db]} [_]]
   {:db (assoc-in db [:ibuffer :sorting-mode] :recency)
    :fx [[:dispatch [:ibuffer/revert]]
         [:dispatch [:echo/message "Sorting by recency"]]]}))

(rf/reg-event-fx
 :ibuffer/toggle-sorting-mode
 (fn [{:keys [db]} [_]]
   (let [current (get-in db [:ibuffer :sorting-mode] :recency)
         idx (.indexOf ibuffer-sorting-modes current)
         next-idx (mod (inc idx) (count ibuffer-sorting-modes))
         next-mode (nth ibuffer-sorting-modes next-idx)]
     {:db (assoc-in db [:ibuffer :sorting-mode] next-mode)
      :fx [[:dispatch [:ibuffer/revert]]
           [:dispatch [:echo/message (str "Sorting by " (name next-mode))]]]})))

(rf/reg-event-fx
 :ibuffer/reverse-sorting
 (fn [{:keys [db]} [_]]
   (let [reversed? (get-in db [:ibuffer :sorting-reversed?] false)]
     {:db (assoc-in db [:ibuffer :sorting-reversed?] (not reversed?))
      :fx [[:dispatch [:ibuffer/revert]]
           [:dispatch [:echo/message (if reversed?
                                       "Sorting order: normal"
                                       "Sorting order: reversed")]]]})))

;; -- Filter Events --

(rf/reg-event-fx
 :ibuffer/filter-by-mode
 (fn [{:keys [db]} [_ mode-name]]
   "Filter ibuffer by major mode."
   (if mode-name
     (let [mode-kw (if (keyword? mode-name) mode-name (keyword mode-name))]
       {:db (update-in db [:ibuffer :filters] (fnil conj [])
                       {:type :mode :value mode-kw})
        :fx [[:dispatch [:ibuffer/revert]]]})
     ;; Prompt for mode
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt "Filter by major mode: "
                        :completions ["fundamental-mode" "clojure-mode"
                                      "javascript-mode" "python-mode"
                                      "rust-mode" "html-mode" "css-mode"
                                      "markdown-mode" "buffer-menu-mode"
                                      "dired-mode" "special-mode"]
                        :on-confirm [:ibuffer/filter-by-mode]}]]]})))

(rf/reg-event-fx
 :ibuffer/filter-by-name
 (fn [{:keys [db]} [_ pattern]]
   "Filter ibuffer by name regexp."
   (if pattern
     {:db (update-in db [:ibuffer :filters] (fnil conj [])
                     {:type :name :value pattern})
      :fx [[:dispatch [:ibuffer/revert]]]}
     {:fx [[:dispatch [:minibuffer/activate
                       {:prompt "Filter by name (regexp): "
                        :on-confirm [:ibuffer/filter-by-name]}]]]})))

(rf/reg-event-fx
 :ibuffer/filter-disable
 (fn [{:keys [db]} [_]]
   "Remove all ibuffer filters."
   {:db (assoc-in db [:ibuffer :filters] [])
    :fx [[:dispatch [:ibuffer/revert]]
         [:dispatch [:echo/message "Filters removed"]]]}))

;; -- Mark & Operation Events --

(defn- get-ibuffer-line
  "Get current line number in ibuffer."
  [db]
  (let [cursor-pos (get-in db [:ui :cursor-position] 0)
        active-window (get-in db [:window-tree])
        buffer-id (when (= (:type active-window) :leaf)
                    (:buffer-id active-window))
        wasm (get-in db [:buffers buffer-id :wasm-instance])
        text (when wasm (try (.getText wasm) (catch :default _ "")))]
    (when text
      (count (filter #(= % \newline) (take cursor-pos text))))))

(rf/reg-event-fx
 :ibuffer/mark-for-delete
 (fn [{:keys [db]} [_]]
   (let [line (get-ibuffer-line db)]
     (when (and line (>= line 2))
       (swap! ibuffer-marks assoc line "D")
       {:fx [[:dispatch [:echo/message "Marked for deletion"]]
             [:dispatch [:next-line]]]}))))

(rf/reg-event-fx
 :ibuffer/unmark
 (fn [{:keys [db]} [_]]
   (let [line (get-ibuffer-line db)]
     (when line
       (swap! ibuffer-marks dissoc line))
     {:fx [[:dispatch [:echo/message "Unmarked"]]
           [:dispatch [:next-line]]]})))

(rf/reg-event-fx
 :ibuffer/do-delete
 (fn [{:keys [db]} [_]]
   "Execute deletion of marked buffers."
   (let [marks @ibuffer-marks
         delete-lines (keep (fn [[line mark]] (when (= mark "D") line)) marks)
         ;; Map lines to buffer names
         content (generate-ibuffer-content db)
         lines (str/split content #"\n")
         buffer-names (keep (fn [line-num]
                              (when (< line-num (count lines))
                                (let [line-text (nth lines line-num)]
                                  ;; Extract buffer name: skip mark+flags (4 chars), take 30 chars
                                  (when (>= (count line-text) 4)
                                    (str/trim (subs line-text 4 (min 34 (count line-text))))))))
                            delete-lines)]
     (if (seq buffer-names)
       (do
         (reset! ibuffer-marks {})
         {:fx (into (mapv (fn [bname] [:dispatch [:kill-buffer-by-name bname]]) buffer-names)
                    [[:dispatch [:ibuffer/revert]]
                     [:dispatch [:echo/message (str "Deleted " (count buffer-names) " buffer(s)")]]])})
       {:fx [[:dispatch [:echo/message "No buffers marked for deletion"]]]}))))

;; -- Select Buffer --

(rf/reg-event-fx
 :ibuffer/select-buffer
 (fn [{:keys [db]} [_]]
   "Switch to buffer on current line."
   (let [line (get-ibuffer-line db)
         content (generate-ibuffer-content db)
         lines (str/split content #"\n")]
     (when (and line (>= line 2) (< line (count lines)))
       (let [line-text (nth lines line)]
         (when (>= (count line-text) 4)
           (let [buffer-name (str/trim (subs line-text 4 (min 34 (count line-text))))
                 target-id (some (fn [[id buf]]
                                   (when (= (:name buf) buffer-name) id))
                                 (:buffers db))]
             (when target-id
               {:fx [[:dispatch [:switch-buffer target-id]]]}))))))))

;; =============================================================================
;; Commands & Key Bindings
;; =============================================================================

(defn init!
  "Initialize ibuffer module and register commands."
  []
  ;; Main ibuffer command
  (rf/dispatch [:register-command :ibuffer
                {:docstring "Begin using IBuffer to edit a list of buffers"
                 :interactive nil
                 :handler [:ibuffer/open]}])

  ;; Sorting commands
  (rf/dispatch [:register-command :ibuffer-do-sort-by-alphabetic
                {:docstring "Sort ibuffer by buffer name"
                 :interactive nil
                 :handler [:ibuffer/sort-by-alphabetic]}])

  (rf/dispatch [:register-command :ibuffer-do-sort-by-size
                {:docstring "Sort ibuffer by buffer size"
                 :interactive nil
                 :handler [:ibuffer/sort-by-size]}])

  (rf/dispatch [:register-command :ibuffer-do-sort-by-major-mode
                {:docstring "Sort ibuffer by major mode"
                 :interactive nil
                 :handler [:ibuffer/sort-by-major-mode]}])

  (rf/dispatch [:register-command :ibuffer-do-sort-by-recency
                {:docstring "Sort ibuffer by recency"
                 :interactive nil
                 :handler [:ibuffer/sort-by-recency]}])

  (rf/dispatch [:register-command :ibuffer-toggle-sorting-mode
                {:docstring "Cycle through ibuffer sorting modes"
                 :interactive nil
                 :handler [:ibuffer/toggle-sorting-mode]}])

  ;; Filter commands
  (rf/dispatch [:register-command :ibuffer-filter-by-mode
                {:docstring "Filter ibuffer by major mode"
                 :interactive nil
                 :handler [:ibuffer/filter-by-mode]}])

  (rf/dispatch [:register-command :ibuffer-filter-by-name
                {:docstring "Filter ibuffer by buffer name regexp"
                 :interactive nil
                 :handler [:ibuffer/filter-by-name]}])

  (rf/dispatch [:register-command :ibuffer-filter-disable
                {:docstring "Remove all ibuffer filters"
                 :interactive nil
                 :handler [:ibuffer/filter-disable]}])

  ;; Ibuffer-mode keybindings
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "RET" :ibuffer/select-buffer])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "d" :ibuffer/mark-for-delete])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "x" :ibuffer/do-delete])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "u" :ibuffer/unmark])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "s" :ibuffer/toggle-sorting-mode])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "g" :ibuffer/revert])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "q" :ibuffer/quit])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "/" :ibuffer/filter-disable])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "n" :next-line])
  (rf/dispatch [:keymap/set-mode-key :ibuffer-mode "p" :previous-line])

  ;; Register keybinding-only commands
  (rf/dispatch [:register-command :ibuffer/select-buffer
                {:docstring "Switch to buffer at point in ibuffer"
                 :handler [:ibuffer/select-buffer]}])
  (rf/dispatch [:register-command :ibuffer/mark-for-delete
                {:docstring "Mark buffer for deletion in ibuffer"
                 :handler [:ibuffer/mark-for-delete]}])
  (rf/dispatch [:register-command :ibuffer/do-delete
                {:docstring "Execute marked deletions in ibuffer"
                 :handler [:ibuffer/do-delete]}])
  (rf/dispatch [:register-command :ibuffer/unmark
                {:docstring "Unmark buffer in ibuffer"
                 :handler [:ibuffer/unmark]}])
  (rf/dispatch [:register-command :ibuffer/revert
                {:docstring "Refresh ibuffer display"
                 :handler [:ibuffer/revert]}]))

;; Quit ibuffer
(rf/reg-event-fx
 :ibuffer/quit
 (fn [{:keys [_db]} [_]]
   {:fx [[:dispatch [:kill-buffer-by-name ""]]]}))
