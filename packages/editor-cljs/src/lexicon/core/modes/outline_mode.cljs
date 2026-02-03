(ns lexicon.core.modes.outline-mode
  "Outline Mode - Hierarchical text folding.

  outline-minor-mode provides commands for folding/unfolding text based on
  outline-heading patterns. This enables code navigation and organization.

  Emacs-compatible keybindings (all use C-c @ prefix in outline-minor-mode):
  - C-c @ C-t : outline-hide-body (hide all body text, show headings only)
  - C-c @ C-a : outline-show-all (show everything)
  - C-c @ C-c : outline-hide-entry (hide body under current heading)
  - C-c @ C-e : outline-show-entry (show body under current heading)
  - C-c @ TAB : outline-toggle-children (toggle visibility)

  Also implements hs-minor-mode (hideshow) for code block folding:
  - C-c @ C-h : hs-hide-block
  - C-c @ C-s : hs-show-block
  - C-c @ C-M-h : hs-hide-all
  - C-c @ C-M-s : hs-show-all"
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]
            [clojure.string :as str]))

;; =============================================================================
;; Outline Patterns by Major Mode
;; =============================================================================

(def outline-patterns
  "Map of major-mode to outline heading regexp patterns.
   Headings are detected by lines starting with these patterns."
  {:clojure-mode    #"^;;;\s"           ; ;;; for Clojure section comments
   :emacs-lisp-mode #"^;;;\s"           ; Same for Elisp
   :markdown-mode   #"^#+\s"            ; # headers in Markdown
   :org-mode        #"^\*+\s"           ; * headers in Org
   :python-mode     #"^(def|class)\s"   ; Python definitions
   :javascript-mode #"^(function|class|const|let|var)\s.*[{=]" ; JS definitions
   :rust-mode       #"^(fn|impl|struct|enum|mod)\s"  ; Rust items
   :fundamental-mode #"^;;;\s"          ; Default to Lisp-style
   :text-mode       #"^#+\s"})          ; Default to Markdown-style

(defn get-outline-pattern
  "Get the outline pattern for a given major mode."
  [major-mode]
  (or (get outline-patterns major-mode)
      (get outline-patterns :fundamental-mode)))

;; =============================================================================
;; Hideshow (Code Block) Patterns
;; =============================================================================

(def hideshow-patterns
  "Map of major-mode to start/end patterns for code blocks.
   {:start regex :end regex} or function that returns block bounds."
  {:clojure-mode    {:start #"^\s*\((defn?|defmacro|defmethod|defmulti|ns|let|when|if|cond|fn)\s"
                     :end   #"^\s*\)"}
   :javascript-mode {:start #"(function|class|if|for|while|switch|\{)"
                     :end   #"\}"}
   :python-mode     {:start #"^(def|class|if|for|while|try)\s"
                     :end   :indent}  ; Python uses indentation
   :rust-mode       {:start #"(fn|impl|struct|enum|mod|if|for|while|match)\s.*\{"
                     :end   #"\}"}})

;; =============================================================================
;; Folding State Management
;; =============================================================================

;; Folded regions are stored in buffer overlays with :invisible property
;; Format: overlay-id -> {:start line :end line :type :outline/:hideshow :invisible true}

(defn get-folded-regions
  "Get all folded regions for a buffer."
  [db buffer-id]
  (let [overlays (get-in db [:buffers buffer-id :overlays] {})]
    (filter (fn [[_ overlay]] (:invisible overlay)) overlays)))

(defn find-heading-level
  "Determine the heading level from a line (e.g., ### = level 3)."
  [line major-mode]
  (cond
    ;; Markdown: count # symbols
    (= major-mode :markdown-mode)
    (count (re-find #"^#+" line))

    ;; Org: count * symbols
    (= major-mode :org-mode)
    (count (re-find #"^\*+" line))

    ;; Clojure: ;;; = level 1, ;;;; = level 2, etc.
    (#{:clojure-mode :emacs-lisp-mode} major-mode)
    (- (count (re-find #"^;+" line)) 2)

    ;; Default: level 1 for any heading
    :else 1))

(defn find-heading-at-line
  "Check if line-number is a heading line. Returns level or nil."
  [db buffer-id line-number]
  (let [buffer (get-in db [:buffers buffer-id])
        major-mode (:major-mode buffer :fundamental-mode)
        pattern (get-outline-pattern major-mode)
        text (get-in buffer [:cache :text] "")
        lines (str/split text #"\n" -1)
        line (get lines line-number "")]
    (when (re-find pattern line)
      (find-heading-level line major-mode))))

(defn find-next-heading
  "Find the line number of the next heading at same or higher level.
   Returns nil if no next heading found."
  [db buffer-id from-line current-level]
  (let [buffer (get-in db [:buffers buffer-id])
        major-mode (:major-mode buffer :fundamental-mode)
        pattern (get-outline-pattern major-mode)
        text (get-in buffer [:cache :text] "")
        lines (str/split text #"\n" -1)
        line-count (count lines)]
    (loop [line (inc from-line)]
      (when (< line line-count)
        (let [line-text (get lines line "")]
          (if (re-find pattern line-text)
            (let [level (find-heading-level line-text major-mode)]
              (if (<= level current-level)
                line  ; Found same or higher level heading
                (recur (inc line))))  ; Lower level, keep looking
            (recur (inc line))))))))

(defn find-subtree-end
  "Find the end of the subtree starting at heading-line.
   Returns the last line belonging to this subtree."
  [db buffer-id heading-line heading-level]
  (let [next-heading (find-next-heading db buffer-id heading-line heading-level)
        buffer (get-in db [:buffers buffer-id])
        line-count (get-in buffer [:cache :line-count] 1)]
    (if next-heading
      (dec next-heading)  ; End just before next heading
      (dec line-count)))) ; End at last line of buffer

;; =============================================================================
;; Overlay-based Folding
;; =============================================================================

(rf/reg-event-db
 :outline/fold-region
 (fn [db [_ buffer-id start-line end-line fold-type]]
   "Fold a region by creating an invisible overlay."
   (let [next-id (get-in db [:buffers buffer-id :next-overlay-id] 1)
         overlay {:id next-id
                  :start-line start-line
                  :end-line end-line
                  :type fold-type
                  :invisible true
                  :after-string "..."}]
     (-> db
         (assoc-in [:buffers buffer-id :overlays next-id] overlay)
         (assoc-in [:buffers buffer-id :next-overlay-id] (inc next-id))))))

(rf/reg-event-db
 :outline/unfold-region
 (fn [db [_ buffer-id overlay-id]]
   "Unfold a region by removing its overlay."
   (update-in db [:buffers buffer-id :overlays] dissoc overlay-id)))

(rf/reg-event-db
 :outline/unfold-at-line
 (fn [db [_ buffer-id line-number]]
   "Unfold any overlay that includes the given line."
   (let [overlays (get-in db [:buffers buffer-id :overlays] {})]
     (reduce
      (fn [db' [id overlay]]
        (if (and (:invisible overlay)
                 (<= (:start-line overlay) line-number)
                 (>= (:end-line overlay) line-number))
          (update-in db' [:buffers buffer-id :overlays] dissoc id)
          db'))
      db
      overlays))))

;; =============================================================================
;; Outline Commands
;; =============================================================================

(rf/reg-event-fx
 :outline/hide-body
 (fn [{:keys [db]} [_]]
   "Hide all body text, showing only headings (C-c @ C-t)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         major-mode (:major-mode buffer :fundamental-mode)
         pattern (get-outline-pattern major-mode)
         text (get-in buffer [:cache :text] "")
         lines (str/split text #"\n" -1)

         ;; Find all headings and fold regions between them
         heading-lines (keep-indexed
                        (fn [idx line]
                          (when (re-find pattern line) idx))
                        lines)

         ;; Create fold events for regions between headings
         fold-events (map-indexed
                      (fn [idx heading-line]
                        (let [next-heading (get (vec heading-lines) (inc idx))
                              end-line (if next-heading
                                        (dec next-heading)
                                        (dec (count lines)))]
                          (when (> end-line heading-line)
                            [:dispatch [:outline/fold-region buffer-id
                                       (inc heading-line) end-line :outline]])))
                      heading-lines)]
     {:fx (concat
           ;; First unfold all
           [[:dispatch [:outline/show-all]]]
           ;; Then fold body regions
           (remove nil? fold-events)
           [[:dispatch [:echo/message "Outline body hidden"]]])})))

(rf/reg-event-fx
 :outline/show-all
 (fn [{:keys [db]} [_]]
   "Show all hidden text (C-c @ C-a)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         overlays (get-in db [:buffers buffer-id :overlays] {})
         ;; Remove all invisible overlays
         new-db (assoc-in db [:buffers buffer-id :overlays]
                         (into {}
                               (remove (fn [[_ o]] (:invisible o)) overlays)))]
     {:db new-db
      :fx [[:dispatch [:echo/message "Outline fully visible"]]]})))

(rf/reg-event-fx
 :outline/hide-entry
 (fn [{:keys [db]} [_]]
   "Hide the body under current heading (C-c @ C-c)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)
         heading-level (find-heading-at-line db buffer-id cursor-line)]
     (if heading-level
       (let [subtree-end (find-subtree-end db buffer-id cursor-line heading-level)]
         (when (> subtree-end cursor-line)
           {:fx [[:dispatch [:outline/fold-region buffer-id
                            (inc cursor-line) subtree-end :outline]]
                 [:dispatch [:echo/message "Entry hidden"]]]}))
       {:fx [[:dispatch [:echo/message "Not on a heading line"]]]}))))

(rf/reg-event-fx
 :outline/show-entry
 (fn [{:keys [db]} [_]]
   "Show the body under current heading (C-c @ C-e)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)]
     {:fx [[:dispatch [:outline/unfold-at-line buffer-id (inc cursor-line)]]
           [:dispatch [:echo/message "Entry shown"]]]})))

(rf/reg-event-fx
 :outline/toggle-children
 (fn [{:keys [db]} [_]]
   "Toggle visibility of current heading's body (C-c @ TAB or just TAB)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)
         heading-level (find-heading-at-line db buffer-id cursor-line)
         overlays (get-in db [:buffers buffer-id :overlays] {})
         ;; Check if there's a fold starting at this heading
         fold-at-heading (first
                          (filter (fn [[_ o]]
                                   (and (:invisible o)
                                        (= (:start-line o) (inc cursor-line))))
                                 overlays))]
     (if heading-level
       (if fold-at-heading
         ;; Already folded - unfold
         {:fx [[:dispatch [:outline/unfold-region buffer-id (first fold-at-heading)]]
               [:dispatch [:echo/message "Entry shown"]]]}
         ;; Not folded - fold
         (let [subtree-end (find-subtree-end db buffer-id cursor-line heading-level)]
           (when (> subtree-end cursor-line)
             {:fx [[:dispatch [:outline/fold-region buffer-id
                              (inc cursor-line) subtree-end :outline]]
                   [:dispatch [:echo/message "Entry hidden"]]]})))
       {:fx [[:dispatch [:echo/message "Not on a heading line"]]]}))))

;; =============================================================================
;; Hideshow Commands (for code blocks)
;; =============================================================================

(defn find-block-bounds
  "Find the start and end lines of the code block at cursor.
   Returns {:start line :end line} or nil."
  [db buffer-id cursor-line]
  (let [buffer (get-in db [:buffers buffer-id])
        major-mode (:major-mode buffer :fundamental-mode)
        text (get-in buffer [:cache :text] "")
        lines (vec (str/split text #"\n" -1))
        patterns (get hideshow-patterns major-mode)]

    (when patterns
      (let [start-pattern (:start patterns)
            current-line (get lines cursor-line "")]
        ;; Check if current line is a block start
        (when (re-find start-pattern current-line)
          ;; Find matching end - simple brace counting for now
          (let [open-char (first (re-find #"[\(\[\{]" current-line))
                close-char (case open-char
                            \( \)
                            \[ \]
                            \{ \}
                            nil)]
            (when close-char
              (loop [line cursor-line
                     depth 1]
                (let [next-line (inc line)]
                  (if (>= next-line (count lines))
                    {:start (inc cursor-line) :end (dec (count lines))}
                    (let [next-text (get lines next-line "")
                          opens (count (filter #(= % open-char) next-text))
                          closes (count (filter #(= % close-char) next-text))
                          new-depth (+ depth opens (- closes))]
                      (if (<= new-depth 0)
                        {:start (inc cursor-line) :end next-line}
                        (recur next-line new-depth)))))))))))))

(rf/reg-event-fx
 :hs/hide-block
 (fn [{:keys [db]} [_]]
   "Hide the code block at point (C-c @ C-h)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)
         bounds (find-block-bounds db buffer-id cursor-line)]
     (if bounds
       {:fx [[:dispatch [:outline/fold-region buffer-id
                        (:start bounds) (:end bounds) :hideshow]]
             [:dispatch [:echo/message "Block hidden"]]]}
       {:fx [[:dispatch [:echo/message "No block to hide at point"]]]}))))

(rf/reg-event-fx
 :hs/show-block
 (fn [{:keys [db]} [_]]
   "Show the code block at point (C-c @ C-s)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)]
     {:fx [[:dispatch [:outline/unfold-at-line buffer-id cursor-line]]
           [:dispatch [:echo/message "Block shown"]]]})))

(rf/reg-event-fx
 :hs/hide-all
 (fn [{:keys [db]} [_]]
   "Hide all code blocks in buffer (C-c @ C-M-h)."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])
         major-mode (:major-mode buffer :fundamental-mode)
         text (get-in buffer [:cache :text] "")
         lines (vec (str/split text #"\n" -1))
         patterns (get hideshow-patterns major-mode)]

     (if patterns
       (let [start-pattern (:start patterns)
             ;; Find all block starts
             block-starts (keep-indexed
                           (fn [idx line]
                             (when (re-find start-pattern line) idx))
                           lines)
             ;; Create fold events for each block
             fold-events (keep
                          (fn [start-line]
                            (when-let [bounds (find-block-bounds db buffer-id start-line)]
                              [:dispatch [:outline/fold-region buffer-id
                                         (:start bounds) (:end bounds) :hideshow]]))
                          block-starts)]
         {:fx (concat fold-events
                     [[:dispatch [:echo/message "All blocks hidden"]]])})
       {:fx [[:dispatch [:echo/message "Hideshow not configured for this mode"]]]}))))

(rf/reg-event-fx
 :hs/show-all
 (fn [{:keys [db]} [_]]
   "Show all hidden blocks (C-c @ C-M-s)."
   ;; Same as outline/show-all
   {:fx [[:dispatch [:outline/show-all]]]}))

(rf/reg-event-fx
 :hs/toggle-hiding
 (fn [{:keys [db]} [_]]
   "Toggle hiding of the block at point."
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         cursor-line (get-in active-window [:cursor-position :line] 0)
         overlays (get-in db [:buffers buffer-id :overlays] {})
         ;; Check if there's a fold at or covering this line
         fold-here (first
                    (filter (fn [[_ o]]
                             (and (:invisible o)
                                  (<= (:start-line o) cursor-line)
                                  (>= (:end-line o) cursor-line)))
                           overlays))]
     (if fold-here
       ;; Already folded - unfold
       {:fx [[:dispatch [:outline/unfold-region buffer-id (first fold-here)]]
             [:dispatch [:echo/message "Block shown"]]]}
       ;; Not folded - try to fold
       {:fx [[:dispatch [:hs/hide-block]]]}))))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-outline-minor-mode!
  "Initialize outline-minor-mode and hs-minor-mode."
  []
  ;; Define outline-minor-mode
  (modes/define-minor-mode
   :outline-minor-mode
   "Toggle outline-based folding of headings"
   {:lighter " Outl"
    :buffer-local? true
    :init-value false})

  ;; Define hs-minor-mode (hideshow)
  (modes/define-minor-mode
   :hs-minor-mode
   "Toggle code block folding (hideshow)"
   {:lighter " hs"
    :buffer-local? true
    :init-value false})

  ;; Register commands
  (rf/dispatch [:register-command :outline-hide-body
               {:docstring "Hide all body text, show only headings"
                :handler [:outline/hide-body]}])
  (rf/dispatch [:register-command :outline-show-all
               {:docstring "Show all hidden text"
                :handler [:outline/show-all]}])
  (rf/dispatch [:register-command :outline-hide-entry
               {:docstring "Hide body under current heading"
                :handler [:outline/hide-entry]}])
  (rf/dispatch [:register-command :outline-show-entry
               {:docstring "Show body under current heading"
                :handler [:outline/show-entry]}])
  (rf/dispatch [:register-command :outline-toggle-children
               {:docstring "Toggle visibility of current heading"
                :handler [:outline/toggle-children]}])

  ;; Hideshow commands
  (rf/dispatch [:register-command :hs-hide-block
               {:docstring "Hide the code block at point"
                :handler [:hs/hide-block]}])
  (rf/dispatch [:register-command :hs-show-block
               {:docstring "Show the code block at point"
                :handler [:hs/show-block]}])
  (rf/dispatch [:register-command :hs-hide-all
               {:docstring "Hide all code blocks"
                :handler [:hs/hide-all]}])
  (rf/dispatch [:register-command :hs-show-all
               {:docstring "Show all code blocks"
                :handler [:hs/show-all]}])
  (rf/dispatch [:register-command :hs-toggle-hiding
               {:docstring "Toggle hiding of code block at point"
                :handler [:hs/toggle-hiding]}])

  ;; Register keybindings (C-c @ prefix for outline)
  ;; These follow Emacs outline-minor-mode conventions
  (rf/dispatch [:keymap/set-global "C-c @ C-t" :outline-hide-body])
  (rf/dispatch [:keymap/set-global "C-c @ C-a" :outline-show-all])
  (rf/dispatch [:keymap/set-global "C-c @ C-c" :outline-hide-entry])
  (rf/dispatch [:keymap/set-global "C-c @ C-e" :outline-show-entry])
  (rf/dispatch [:keymap/set-global "C-c @ TAB" :outline-toggle-children])

  ;; Hideshow keybindings
  (rf/dispatch [:keymap/set-global "C-c @ C-h" :hs-hide-block])
  (rf/dispatch [:keymap/set-global "C-c @ C-s" :hs-show-block])
  (rf/dispatch [:keymap/set-global "C-c @ C-M-h" :hs-hide-all])
  (rf/dispatch [:keymap/set-global "C-c @ C-M-s" :hs-show-all]))

;; =============================================================================
;; Subscriptions for View Rendering
;; =============================================================================

(rf/reg-sub
 :outline/folded-regions
 (fn [db [_ buffer-id]]
   "Get all folded regions for a buffer."
   (get-folded-regions db buffer-id)))

(rf/reg-sub
 :outline/line-visible?
 (fn [[_ buffer-id line-number]]
   (rf/subscribe [:outline/folded-regions buffer-id]))
 (fn [folded-regions [_ _ line-number]]
   "Check if a line is visible (not inside a folded region)."
   (not-any? (fn [[_ overlay]]
               (and (<= (:start-line overlay) line-number)
                    (>= (:end-line overlay) line-number)))
             folded-regions)))

(rf/reg-sub
 :outline/fold-indicator
 (fn [[_ buffer-id line-number]]
   (rf/subscribe [:outline/folded-regions buffer-id]))
 (fn [folded-regions [_ _ line-number]]
   "Get fold indicator for a line (e.g., '...' if this line starts a fold)."
   (when-let [[_ overlay] (first
                           (filter (fn [[_ o]]
                                    (= (:start-line o) (inc line-number)))
                                  folded-regions))]
     (:after-string overlay "..."))))
