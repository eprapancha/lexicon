(ns lexicon.packages.ibuffer
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

  Based on Emacs lisp/ibuffer.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ibuffer-sorting-modes
  "Available sorting modes."
  [:alphabetic :size :major-mode :recency])

;; =============================================================================
;; State
;; =============================================================================

(defonce ibuffer-state
  (atom {:sorting-mode :recency
         :sorting-reversed? false
         :filters []
         :filter-groups []}))

(defonce ibuffer-marks (atom {}))

;; =============================================================================
;; Filtering (Pure Functions)
;; =============================================================================

(defn- matches-filter?
  "Check if a buffer info matches a single filter."
  [buf-info filter-spec]
  (case (:type filter-spec)
    :mode (= (:mode buf-info) (:value filter-spec))
    :name (re-find (re-pattern (:value filter-spec)) (:name buf-info))
    :filename (when (:file buf-info)
                (re-find (re-pattern (:value filter-spec)) (:file buf-info)))
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
;; Sorting (Pure Functions)
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
;; Filter Groups (Pure Functions)
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
;; Display Formatting (Pure Functions)
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
        file-str (or (:file buf-info) "")]
    (str mark modified readonly " " name-str " " size-str " " mode-str " " file-str)))

(defn- format-group-header
  "Format a filter group header line."
  [group-name]
  (str "[ " group-name " ]"))

(defn- generate-ibuffer-content
  "Generate the complete ibuffer content."
  []
  (let [{:keys [sorting-mode sorting-reversed? filters filter-groups]} @ibuffer-state
        ;; Get buffer info from lisp.cljs
        buf-infos (lisp/all-buffer-info)
        ;; Apply filters
        filtered (apply-filters buf-infos filters)
        ;; Sort
        sorted (sort-buffers filtered sorting-mode sorting-reversed?)
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
;; IBuffer Commands
;; =============================================================================

(defn ibuffer-open!
  "Open ibuffer interface."
  []
  (reset! ibuffer-marks {})
  (let [content (generate-ibuffer-content)]
    ;; Kill existing ibuffer if any
    (when-let [existing-id (lisp/get-buffer "*Ibuffer*")]
      (lisp/kill-buffer existing-id))
    ;; Create new ibuffer
    (lisp/create-special-buffer "*Ibuffer*" content
                                 {:major-mode :ibuffer-mode
                                  :read-only true})
    (lisp/switch-to-buffer "*Ibuffer*")))

(defn ibuffer-revert!
  "Refresh ibuffer content."
  []
  (let [content (generate-ibuffer-content)]
    ;; Kill existing and recreate
    (when-let [existing-id (lisp/get-buffer "*Ibuffer*")]
      (lisp/kill-buffer existing-id))
    (lisp/create-special-buffer "*Ibuffer*" content
                                 {:major-mode :ibuffer-mode
                                  :read-only true})
    (lisp/switch-to-buffer "*Ibuffer*")
    (lisp/message "IBuffer refreshed")))

;; -- Sorting Commands --

(defn ibuffer-sort-by-alphabetic!
  []
  (swap! ibuffer-state assoc :sorting-mode :alphabetic)
  (ibuffer-revert!)
  (lisp/message "Sorting by name"))

(defn ibuffer-sort-by-size!
  []
  (swap! ibuffer-state assoc :sorting-mode :size)
  (ibuffer-revert!)
  (lisp/message "Sorting by size"))

(defn ibuffer-sort-by-major-mode!
  []
  (swap! ibuffer-state assoc :sorting-mode :major-mode)
  (ibuffer-revert!)
  (lisp/message "Sorting by major mode"))

(defn ibuffer-sort-by-recency!
  []
  (swap! ibuffer-state assoc :sorting-mode :recency)
  (ibuffer-revert!)
  (lisp/message "Sorting by recency"))

(defn ibuffer-toggle-sorting-mode!
  []
  (let [current (:sorting-mode @ibuffer-state)
        idx (.indexOf ibuffer-sorting-modes current)
        next-idx (mod (inc idx) (count ibuffer-sorting-modes))
        next-mode (nth ibuffer-sorting-modes next-idx)]
    (swap! ibuffer-state assoc :sorting-mode next-mode)
    (ibuffer-revert!)
    (lisp/message (str "Sorting by " (name next-mode)))))

(defn ibuffer-reverse-sorting!
  []
  (swap! ibuffer-state update :sorting-reversed? not)
  (ibuffer-revert!)
  (lisp/message (if (:sorting-reversed? @ibuffer-state)
                  "Sorting order: reversed"
                  "Sorting order: normal")))

;; -- Filter Commands --

(defn- get-ibuffer-line
  "Get current line number in ibuffer."
  []
  (let [pos (lisp/point)
        text (lisp/buffer-string)]
    (when text
      (count (filter #(= % \newline) (take pos text))))))

(defn ibuffer-filter-by-mode-interactive []
  (lisp/read-from-minibuffer
   "Filter by major mode: "
   (fn [mode-name]
     (when mode-name
       (let [mode-kw (if (keyword? mode-name) mode-name (keyword mode-name))]
         (swap! ibuffer-state update :filters (fnil conj [])
                {:type :mode :value mode-kw})
         (ibuffer-revert!))))))

(defn ibuffer-filter-by-name-interactive []
  (lisp/read-from-minibuffer
   "Filter by name (regexp): "
   (fn [pattern]
     (when pattern
       (swap! ibuffer-state update :filters (fnil conj [])
              {:type :name :value pattern})
       (ibuffer-revert!)))))

(defn ibuffer-filter-disable!
  "Remove all ibuffer filters."
  []
  (swap! ibuffer-state assoc :filters [])
  (ibuffer-revert!)
  (lisp/message "Filters removed"))

;; -- Mark & Operation Commands --

(defn ibuffer-mark-for-delete!
  []
  (let [line (get-ibuffer-line)]
    (when (and line (>= line 2))
      (swap! ibuffer-marks assoc line "D")
      (lisp/message "Marked for deletion"))))

(defn ibuffer-unmark!
  []
  (let [line (get-ibuffer-line)]
    (when line
      (swap! ibuffer-marks dissoc line))
    (lisp/message "Unmarked")))

(defn ibuffer-do-delete!
  "Execute deletion of marked buffers."
  []
  (let [marks @ibuffer-marks
        delete-lines (keep (fn [[line mark]] (when (= mark "D") line)) marks)
        content (generate-ibuffer-content)
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
        (doseq [bname buffer-names]
          (when-let [buf-id (lisp/get-buffer bname)]
            (lisp/kill-buffer buf-id)))
        (ibuffer-revert!)
        (lisp/message (str "Deleted " (count buffer-names) " buffer(s)")))
      (lisp/message "No buffers marked for deletion"))))

;; -- Select Buffer --

(defn ibuffer-select-buffer!
  "Switch to buffer on current line."
  []
  (let [line (get-ibuffer-line)
        content (generate-ibuffer-content)
        lines (str/split content #"\n")]
    (when (and line (>= line 2) (< line (count lines)))
      (let [line-text (nth lines line)]
        (when (>= (count line-text) 4)
          (let [buffer-name (str/trim (subs line-text 4 (min 34 (count line-text))))
                target-id (lisp/get-buffer buffer-name)]
            (when target-id
              (lisp/switch-to-buffer target-id))))))))

(defn ibuffer-quit!
  "Quit ibuffer."
  []
  (when-let [ibuf-id (lisp/get-buffer "*Ibuffer*")]
    (lisp/kill-buffer ibuf-id)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize ibuffer module and register commands."
  []
  ;; Main ibuffer command
  (lisp/define-command 'ibuffer
    ibuffer-open!
    "Begin using IBuffer to edit a list of buffers")

  ;; Sorting commands
  (lisp/define-command 'ibuffer-do-sort-by-alphabetic
    ibuffer-sort-by-alphabetic!
    "Sort ibuffer by buffer name")

  (lisp/define-command 'ibuffer-do-sort-by-size
    ibuffer-sort-by-size!
    "Sort ibuffer by buffer size")

  (lisp/define-command 'ibuffer-do-sort-by-major-mode
    ibuffer-sort-by-major-mode!
    "Sort ibuffer by major mode")

  (lisp/define-command 'ibuffer-do-sort-by-recency
    ibuffer-sort-by-recency!
    "Sort ibuffer by recency")

  (lisp/define-command 'ibuffer-toggle-sorting-mode
    ibuffer-toggle-sorting-mode!
    "Cycle through ibuffer sorting modes")

  ;; Filter commands
  (lisp/define-command 'ibuffer-filter-by-mode
    ibuffer-filter-by-mode-interactive
    "Filter ibuffer by major mode")

  (lisp/define-command 'ibuffer-filter-by-name
    ibuffer-filter-by-name-interactive
    "Filter ibuffer by buffer name regexp")

  (lisp/define-command 'ibuffer-filter-disable
    ibuffer-filter-disable!
    "Remove all ibuffer filters")

  ;; Mode commands
  (lisp/define-command 'ibuffer-select-buffer
    ibuffer-select-buffer!
    "Switch to buffer at point in ibuffer")

  (lisp/define-command 'ibuffer-mark-for-delete
    ibuffer-mark-for-delete!
    "Mark buffer for deletion in ibuffer")

  (lisp/define-command 'ibuffer-do-delete
    ibuffer-do-delete!
    "Execute marked deletions in ibuffer")

  (lisp/define-command 'ibuffer-unmark
    ibuffer-unmark!
    "Unmark buffer in ibuffer")

  (lisp/define-command 'ibuffer-revert
    ibuffer-revert!
    "Refresh ibuffer display")

  (lisp/define-command 'ibuffer-quit
    ibuffer-quit!
    "Quit ibuffer")

  ;; Ibuffer-mode keybindings
  (lisp/define-key-for-mode :ibuffer-mode "RET" :ibuffer-select-buffer)
  (lisp/define-key-for-mode :ibuffer-mode "d" :ibuffer-mark-for-delete)
  (lisp/define-key-for-mode :ibuffer-mode "x" :ibuffer-do-delete)
  (lisp/define-key-for-mode :ibuffer-mode "u" :ibuffer-unmark)
  (lisp/define-key-for-mode :ibuffer-mode "s" :ibuffer-toggle-sorting-mode)
  (lisp/define-key-for-mode :ibuffer-mode "g" :ibuffer-revert)
  (lisp/define-key-for-mode :ibuffer-mode "q" :ibuffer-quit)
  (lisp/define-key-for-mode :ibuffer-mode "/" :ibuffer-filter-disable)
  (lisp/define-key-for-mode :ibuffer-mode "n" :next-line)
  (lisp/define-key-for-mode :ibuffer-mode "p" :previous-line))
