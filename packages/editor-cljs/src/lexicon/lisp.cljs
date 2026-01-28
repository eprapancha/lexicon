(ns lexicon.lisp
  "Impure Emacs-compatible Lisp API for SCI.

  These functions are the PUBLIC API that:
  - Users call from evaluated Lisp code
  - Packages call from elisp
  - Tests call via (eval-lisp ...)
  - REPL calls interactively

  Unlike lexicon.api.buffer (pure functions taking db), these:
  - Access live editor state via @re-frame.db/app-db
  - Dispatch events synchronously for mutations
  - Return values directly (no db parameter needed)

  This is the ONLY API tests should use. If tests need something not here,
  it means we need to add it to the public API, not create test-only functions."
  (:require [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [clojure.string :as str]
            [lexicon.core.api.buffer :as buf]
            [lexicon.core.api.message :as msg]
            [lexicon.core.db :as db]
            [lexicon.core.markers :as markers]
            [lexicon.core.advanced-undo :as advanced-undo]))

;; Forward declarations for functions used before definition
(declare goto-char line-beginning-position line-end-position current-buffer mark delete-region)

;; =============================================================================
;; Global Variable Storage (for setq/symbol-value)
;; =============================================================================

;; Atom storing global Lisp variables set via setq
(defonce global-vars
  (atom {'*prefix-arg* nil
         '*last-command* nil
         '*this-command* nil}))

;; =============================================================================
;; Hooks System
;; =============================================================================

;; Atom storing hook lists: symbol -> vector of functions
(defonce hooks-registry
  (atom {}))

(defn add-hook
  "Add FUNCTION to HOOK.

  HOOK is a symbol naming a hook variable.
  FUNCTION is the function to add.

  Functions are called in the order they were added.

  Usage: (add-hook 'my-hook (fn [] (insert \"ran\")))
  Returns: nil"
  [hook-symbol function]
  (swap! hooks-registry update hook-symbol
         (fn [fns] (conj (or fns []) function)))
  nil)

(defn remove-hook
  "Remove FUNCTION from HOOK.

  Usage: (remove-hook 'my-hook my-fn)
  Returns: nil"
  [hook-symbol function]
  (swap! hooks-registry update hook-symbol
         (fn [fns] (vec (remove #(= % function) (or fns [])))))
  nil)

(defn run-hooks
  "Run all functions on HOOK.

  Each function is called with no arguments.

  Usage: (run-hooks 'my-hook)
  Returns: nil"
  [hook-symbol]
  (let [fns (get @hooks-registry hook-symbol [])]
    (doseq [f fns]
      (f)))
  nil)

(defn run-hook-with-args
  "Run all functions on HOOK with ARGS.

  Each function is called with the provided arguments.

  Usage: (run-hook-with-args 'my-hook arg1 arg2)
  Returns: nil"
  [hook-symbol & args]
  (let [fns (get @hooks-registry hook-symbol [])]
    (doseq [f fns]
      (apply f args)))
  nil)

(defn run-hook-with-args-until-success
  "Run functions on HOOK until one returns non-nil.

  Stops at first function that returns a non-nil value.
  Returns that value.

  Usage: (run-hook-with-args-until-success 'my-hook)
  Returns: First non-nil return value, or nil if all return nil"
  [hook-symbol & args]
  (let [fns (get @hooks-registry hook-symbol [])]
    (loop [remaining fns]
      (if (empty? remaining)
        nil
        (let [f (first remaining)
              result (apply f args)]
          (if result
            result
            (recur (rest remaining))))))))

(defn run-hook-with-args-until-failure
  "Run functions on HOOK until one returns nil.

  Stops at first function that returns nil.
  Returns nil if any function returned nil, otherwise returns
  the result of the last function.

  Usage: (run-hook-with-args-until-failure 'my-hook)
  Returns: nil if any function returned nil"
  [hook-symbol & args]
  (let [fns (get @hooks-registry hook-symbol [])]
    (loop [remaining fns
           last-result nil]
      (if (empty? remaining)
        last-result
        (let [f (first remaining)
              result (apply f args)]
          (if (nil? result)
            nil  ; Stop and return nil on failure
            (recur (rest remaining) result)))))))

(defn setq
  "Set variable VAR to VALUE.

  In our implementation, this stores the value in a global atom.
  Multiple var/value pairs can be provided.

  Usage: (setq x 5)
         (setq x 5 y 10)
  Returns: Last value set"
  [& args]
  (let [pairs (partition 2 args)]
    (doseq [[var val] pairs]
      (swap! global-vars assoc var val))
    (second (last pairs))))

(defn symbol-value
  "Return the value of SYMBOL.

  Usage: (symbol-value 'x)
  Returns: Value or nil"
  [sym]
  (get @global-vars sym))

;; =============================================================================
;; Buffer Query Functions (Read-Only)
;; =============================================================================

(defn point
  "Return current cursor position as linear byte offset.

  Usage: (point)
  Returns: Integer position"
  []
  (or (buf/point @rfdb/app-db) 0))

(defn point-min
  "Return minimum accessible buffer position.

  With narrowing active, returns start of narrowed region.
  Without narrowing, returns 0.

  Usage: (point-min)
  Returns: Integer position"
  []
  (buf/point-min @rfdb/app-db))

(defn point-max
  "Return maximum valid buffer position (buffer length).

  Usage: (point-max)
  Returns: Integer position"
  []
  (or (buf/point-max @rfdb/app-db) 0))

(defn buffer-string
  "Return entire buffer contents as string.

  Usage: (buffer-string)
  Returns: String"
  []
  (or (buf/buffer-string @rfdb/app-db) ""))

(defn buffer-substring
  "Return text between START and END positions.

  Usage: (buffer-substring start end)
  Returns: String"
  [start end]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        buffer (get-in db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)]
    (when wasm-instance
      (let [text (.getText ^js wasm-instance)
            actual-start (max 0 (min start end))
            actual-end (min (count text) (max start end))]
        (subs text actual-start actual-end)))))

(defn buffer-substring-no-properties
  "Return text between START and END, excluding text properties.

  In Emacs, this differs from buffer-substring which includes properties.
  In our implementation, they are currently identical since we don't
  attach properties to returned strings.

  Usage: (buffer-substring-no-properties 0 10)
  Returns: String"
  [start end]
  (buffer-substring start end))

(defn buffer-name
  "Return name of current buffer.

  Usage: (buffer-name)
  Returns: String"
  []
  (buf/buffer-name @rfdb/app-db))

(defn buffer-size
  "Return size of current buffer in characters.

  Usage: (buffer-size)
  Returns: Integer"
  []
  (buf/buffer-size @rfdb/app-db))

;; -- Narrowing (Phase 6.6 - Issue #100) --

(defn narrow-to-region
  "Restrict editing to [START, END) region.

  All buffer operations will only see the narrowed region.
  Point-min returns START, point-max returns END.
  Point is clamped to the narrowed region.

  Usage: (narrow-to-region start end)
  Returns: nil"
  [start end]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/narrow-to-region buffer-id start end])
    nil))

(defn widen
  "Remove narrowing restriction from current buffer.

  Restores access to the entire buffer contents.

  Usage: (widen)
  Returns: nil"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/widen buffer-id])
    nil))

(defn buffer-narrowed-p
  "Return non-nil if buffer is narrowed.

  Usage: (buffer-narrowed-p)
  Returns: Boolean"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        begv (get-in db [:buffers buffer-id :begv])
        zv (get-in db [:buffers buffer-id :zv])]
    (or (some? begv) (some? zv))))

;; -- Buffer-Local Variables (Phase 6.6 - Issue #100) --

(defn make-local-variable
  "Make VARIABLE buffer-local in current buffer.

  The buffer-local value is initially the global value.

  Usage: (make-local-variable 'my-var)
  Returns: variable symbol"
  [variable]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/make-local-variable buffer-id variable])
    variable))

(defn buffer-local-value
  "Return value of VARIABLE in BUFFER.

  If VARIABLE is buffer-local in BUFFER, returns local value.
  Otherwise returns global value.

  Usage: (buffer-local-value 'my-var buffer-id)
  Returns: value"
  [variable buffer-id]
  (let [db @rfdb/app-db
        is-local? (contains? (get-in db [:buffers buffer-id :local-vars-set]) variable)]
    (if is-local?
      (get-in db [:buffers buffer-id :local-vars variable])
      (get-in db [:global-vars variable]))))

(defn setq-local
  "Set VARIABLE to VALUE as buffer-local.

  Makes VARIABLE buffer-local if not already, then sets its value.

  Usage: (setq-local 'my-var value)
  Returns: value"
  [variable value]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/make-local-variable buffer-id variable])
    (rf/dispatch-sync [:buffer/setq buffer-id variable value])
    value))

(defn kill-all-local-variables
  "Kill all buffer-local variable bindings.

  Used by major mode switching to clear previous mode's configuration.

  Usage: (kill-all-local-variables)
  Returns: nil"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/kill-all-local-variables buffer-id])
    nil))

;; -- Buffer Operations (Phase 6.6 - Issue #100) --

(defn set-buffer
  "Make BUFFER-ID the current buffer without displaying it.

  Unlike switch-to-buffer, this does not change what's displayed
  in any window. Used for programmatic buffer access.

  Usage: (set-buffer buffer-id)
  Returns: buffer-id"
  [buffer-id]
  (rf/dispatch-sync [:buffer/set-current buffer-id])
  buffer-id)

(defn rename-buffer
  "Rename current buffer to NEW-NAME.

  Returns nil if a buffer with NEW-NAME already exists.

  Usage: (rename-buffer \"new-name\")
  Returns: new-name or nil"
  [new-name]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        existing (db/find-buffer-by-name (:buffers db) new-name)]
    (if (and existing (not= (:id existing) buffer-id))
      nil
      (do
        (rf/dispatch-sync [:buffer/rename buffer-id new-name])
        new-name))))

(defn other-buffer
  "Return most recently used buffer other than current.

  Returns buffer-id of the MRU buffer, or nil if none.

  Usage: (other-buffer)
  Returns: buffer-id or nil"
  []
  (let [db @rfdb/app-db
        current-id (current-buffer)
        access-order (get db :buffer-access-order [])
        buffers (:buffers db)]
    (->> access-order
         (remove #(= % current-id))
         (filter #(get buffers %))
         (remove #(clojure.string/starts-with? (get-in buffers [% :name] "") " "))
         first)))

(defn buffer-enable-undo
  "Enable undo recording for current buffer.

  Usage: (buffer-enable-undo)
  Returns: nil"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/enable-undo buffer-id])
    nil))

(defn buffer-disable-undo
  "Disable undo recording and clear undo history.

  Usage: (buffer-disable-undo)
  Returns: nil"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/disable-undo buffer-id])
    nil))

(defn line-beginning-position
  "Return position at beginning of current line.

  Usage: (line-beginning-position)
  Returns: Integer position"
  []
  (buf/line-beginning-position @rfdb/app-db))

(defn line-end-position
  "Return position at end of current line.

  Usage: (line-end-position)
  Returns: Integer position"
  []
  (buf/line-end-position @rfdb/app-db))

(defn current-line
  "Return current line number (0-indexed).

  Usage: (current-line)
  Returns: Integer line number"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        cursor-pos (:cursor-position active-window)]
    (or (:line cursor-pos) 0)))

(defn line-count
  "Return total number of lines in buffer.

  Usage: (line-count)
  Returns: Integer"
  []
  (let [text (buffer-string)]
    (count (str/split text #"\n" -1))))

(defn forward-line
  "Move N lines forward (or backward if N is negative).

  Moves point to beginning of target line.
  Returns count of lines that could NOT be moved (0 if successful).

  Usage: (forward-line 1)  ; move down one line
         (forward-line -1) ; move up one line
  Returns: Integer (0 if moved all lines, positive if hit buffer boundary)"
  ([] (forward-line 1))
  ([n]
   (let [current (current-line)
         total (line-count)
         target (+ current n)
         ;; Clamp to valid range
         clamped (max 0 (min (dec total) target))
         ;; Calculate how many lines we couldn't move
         shortfall (abs (- target clamped))]
     ;; Move to beginning of target line using goto-char for proper position sync
     (let [db @rfdb/app-db
           active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
           buffer-id (:buffer-id active-window)
           buffer (get-in db [:buffers buffer-id])
           new-pos (buf/line-col-to-point buffer clamped 0)]
       (when new-pos
         (goto-char new-pos)))
     shortfall)))

(defn beginning-of-line
  "Move point to beginning of current line.

  Usage: (beginning-of-line)
  Returns: nil"
  []
  (goto-char (line-beginning-position))
  nil)

(defn end-of-line
  "Move point to end of current line.

  Usage: (end-of-line)
  Returns: nil"
  []
  (goto-char (line-end-position))
  nil)

(defn get-buffer
  "Get buffer by name.

  Usage: (get-buffer \"*scratch*\")
  Returns: Buffer ID or nil"
  [buffer-name]
  (when-let [buffer (buf/get-buffer @rfdb/app-db buffer-name)]
    (:id buffer)))

;; =============================================================================
;; Buffer Position Predicates
;; =============================================================================

(defn bobp
  "Return true if point is at the beginning of the buffer.

  With narrowing, returns true if point is at the beginning of the
  accessible portion (point-min).

  Usage: (bobp)
  Returns: Boolean"
  []
  (= (point) (point-min)))

(defn eobp
  "Return true if point is at the end of the buffer.

  With narrowing, returns true if point is at the end of the
  accessible portion (point-max).

  Usage: (eobp)
  Returns: Boolean"
  []
  (= (point) (point-max)))

(defn bolp
  "Return true if point is at the beginning of a line.

  Usage: (bolp)
  Returns: Boolean"
  []
  (= (point) (line-beginning-position)))

(defn eolp
  "Return true if point is at the end of a line.

  Usage: (eolp)
  Returns: Boolean"
  []
  (= (point) (line-end-position)))

;; =============================================================================
;; Character Access Functions
;; =============================================================================

(defn char-after
  "Return character at position POS (or point if nil).

  Returns nil if POS is at or past the end of the buffer.

  Usage: (char-after)
         (char-after 10)
  Returns: Character or nil"
  ([] (char-after nil))
  ([pos]
   (let [position (or pos (point))
         text (buffer-string)
         max-pos (count text)]
     (when (< position max-pos)
       (nth text position)))))

(defn char-before
  "Return character before position POS (or point if nil).

  Returns nil if POS is at or before the beginning of the buffer.

  Usage: (char-before)
         (char-before 10)
  Returns: Character or nil"
  ([] (char-before nil))
  ([pos]
   (let [position (or pos (point))]
     (when (> position 0)
       (let [text (buffer-string)]
         (when (< (dec position) (count text))
           (nth text (dec position))))))))

;; =============================================================================
;; Region Functions
;; =============================================================================

(defn region-beginning
  "Return position of the beginning of the region.

  The region is defined by point and mark.
  Returns the smaller of the two values.

  Usage: (region-beginning)
  Returns: Integer position"
  []
  (let [p (point)
        m (mark)]
    (if m
      (min p m)
      p)))

(defn region-end
  "Return position of the end of the region.

  The region is defined by point and mark.
  Returns the larger of the two values.

  Usage: (region-end)
  Returns: Integer position"
  []
  (let [p (point)
        m (mark)]
    (if m
      (max p m)
      p)))

(defn delete-and-extract-region
  "Delete the region and return its contents.

  Usage: (delete-and-extract-region start end)
  Returns: String (the deleted text)"
  [start end]
  (let [text (buffer-substring start end)]
    (delete-region start end)
    text))

;; =============================================================================
;; Text Properties
;; =============================================================================

(defn put-text-property
  "Set PROPERTY to VALUE for text from START to END.

  Text properties are invisible metadata attached to text ranges.
  Used for faces, invisibility, clickable regions, etc.

  Usage: (put-text-property 0 5 'face 'bold)
         (put-text-property 10 20 'invisible t)
  Returns: nil"
  [start end property value]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:text-property/put buffer-id start end property value])
    nil))

(defn get-text-property
  "Return value of PROPERTY at position POS.

  Returns nil if the property is not set at that position.

  Usage: (get-text-property 5 'face)
  Returns: Property value or nil"
  [pos property]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        props (get-in db [:buffers buffer-id :text-properties] {})]
    ;; Find a range that contains pos and has the property
    (some (fn [[start ranges]]
            (when (<= start pos)
              (some (fn [[end prop-map]]
                      (when (and (> end pos)
                                 (contains? prop-map property))
                        (get prop-map property)))
                    ranges)))
          props)))

(defn remove-text-properties
  "Remove text properties from START to END.

  If PROPERTIES is specified, removes only those properties.
  Otherwise removes all properties from the range.

  Usage: (remove-text-properties 0 10)
         (remove-text-properties 0 10 '(face invisible))
  Returns: nil"
  ([start end]
   (let [buffer-id (current-buffer)]
     (rf/dispatch-sync [:text-property/remove buffer-id start end nil])
     nil))
  ([start end properties]
   (let [buffer-id (current-buffer)]
     (doseq [prop properties]
       (rf/dispatch-sync [:text-property/remove buffer-id start end prop]))
     nil)))

;; =============================================================================
;; Search Functions
;; =============================================================================

(defn- case-fold-search?
  "Return true if search should be case-insensitive.
   Emacs convention: case-insensitive if search string is all lowercase."
  [string]
  (= string (str/lower-case string)))

(defn search-forward
  "Search forward for STRING starting after point.
   If found, move point to end of match and return point.
   If not found, return nil and leave point unchanged.

   Optional BOUND limits search to that position.
   Optional NOERROR if non-nil suppresses error (default behavior).
   Optional COUNT searches for Nth occurrence.

  Usage: (search-forward \"pattern\")
         (search-forward \"pattern\" limit)
  Returns: Position after match or nil"
  ([string] (search-forward string nil true 1))
  ([string bound] (search-forward string bound true 1))
  ([string bound noerror] (search-forward string bound noerror 1))
  ([string bound _noerror _count]
   (when-not (str/blank? string)
     (let [text (buffer-string)
           current-pos (point)
           case-fold? (case-fold-search? string)
           search-text (if case-fold? (str/lower-case text) text)
           search-term (if case-fold? (str/lower-case string) string)
           text-from-pos (subs search-text current-pos)
           effective-bound (or bound (count text))
           match-index (.indexOf text-from-pos search-term)]
       (when (and (>= match-index 0)
                  (<= (+ current-pos match-index (count string)) effective-bound))
         (let [match-end (+ current-pos match-index (count string))]
           (rf/dispatch-sync [:cursor/set-position match-end])
           match-end))))))

(defn search-backward
  "Search backward for STRING starting before point.
   If found, move point to beginning of match and return point.
   If not found, return nil and leave point unchanged.

   Optional BOUND limits search to that position.
   Optional NOERROR if non-nil suppresses error (default behavior).
   Optional COUNT searches for Nth occurrence.

  Usage: (search-backward \"pattern\")
         (search-backward \"pattern\" limit)
  Returns: Position at start of match or nil"
  ([string] (search-backward string nil true 1))
  ([string bound] (search-backward string bound true 1))
  ([string bound noerror] (search-backward string bound noerror 1))
  ([string bound _noerror _count]
   (when-not (str/blank? string)
     (let [text (buffer-string)
           current-pos (point)
           case-fold? (case-fold-search? string)
           search-text (if case-fold? (str/lower-case text) text)
           search-term (if case-fold? (str/lower-case string) string)
           text-before-pos (subs search-text 0 current-pos)
           effective-bound (or bound 0)
           match-index (.lastIndexOf text-before-pos search-term)]
       (when (and (>= match-index 0)
                  (>= match-index effective-bound))
         (rf/dispatch-sync [:cursor/set-position match-index])
         match-index)))))

;; =============================================================================
;; Buffer Mutation Functions
;; =============================================================================

(defn insert
  "Insert TEXT at point.

  NOTE: Uses direct WASM calls and swap! to update app-db because dispatch-sync
  doesn't work when called from inside an event handler.

  Usage: (insert \"hello\")
  Returns: nil (side effect only)"
  [text]
  (let [text-str (str text)]
    (when (pos? (count text-str))
      (let [db @rfdb/app-db
            active-window-id (:active-window-id db)
            active-window (db/find-window-in-tree (:window-tree db) active-window-id)
            buffer-id (:buffer-id active-window)
            buffer (get-in db [:buffers buffer-id])
            wasm-instance (:wasm-instance buffer)
            ;; Read position from current db state
            pos (or (buf/point db) 0)
            recording-enabled? (get-in db [:undo :recording-enabled?] true)
            undo-enabled? (get-in db [:buffers buffer-id :undo-enabled?] true)]
        ;; Run before-change-functions hook
        (run-hook-with-args 'before-change-functions pos pos)
        ;; Insert directly into WASM buffer
        (when wasm-instance
          (.insert ^js wasm-instance pos text-str)
          ;; Update cache, point position, and undo stack
          (let [new-text (.getText ^js wasm-instance)
                lines (clojure.string/split new-text #"\n" -1)
                line-count (count lines)
                new-pos (+ pos (count text-str))
                ;; Calculate line/col for window cursor
                new-line-col (buf/point-to-line-col {:wasm-instance wasm-instance} new-pos)]
            (swap! rfdb/app-db
                   (fn [current-db]
                     (let [;; Update window-tree cursor position
                           window-tree (:window-tree current-db)
                           new-window-tree (db/update-window-in-tree window-tree active-window-id
                                                                      #(assoc % :cursor-position new-line-col))
                           db-with-cache (-> current-db
                                             (assoc :window-tree new-window-tree)
                                             (assoc-in [:buffers buffer-id :cache :text] new-text)
                                             (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                                             (assoc-in [:buffers buffer-id :point] new-pos)
                                             (assoc-in [:ui :cursor-position] new-pos)
                                             (assoc-in [:buffers buffer-id :is-modified?] true))]
                       ;; Record undo entry if enabled
                       (if (and recording-enabled? undo-enabled?)
                         (let [;; Record point position for restoration
                               point-marker {:type :marker
                                             :marker-id :point
                                             :old-pos pos
                                             :new-pos new-pos}
                               undo-entry {:type :edit
                                           :op :insert
                                           :position pos
                                           :text text-str}
                               stack (get-in db-with-cache [:buffers buffer-id :undo-stack] [])]
                           (-> db-with-cache
                               (assoc-in [:buffers buffer-id :undo-stack] (conj stack point-marker undo-entry))
                               ;; Clear redo stack on new edit
                               (assoc-in [:buffers buffer-id :redo-stack] [])))
                         db-with-cache)))
            ;; Run after-change-functions hook: (beg end old-len)
            ;; For insert: old-len is 0, end is new position
            (run-hook-with-args 'after-change-functions pos new-pos 0))))))
    nil))

(defn delete-region
  "Delete text between START and END.

  Usage: (delete-region start end)
  Returns: nil (side effect only)"
  [start end]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        buffer (get-in db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)
        ;; Ensure start <= end
        actual-start (min start end)
        actual-end (max start end)
        length (- actual-end actual-start)
        recording-enabled? (get-in db [:undo :recording-enabled?] true)
        undo-enabled? (get-in db [:buffers buffer-id :undo-enabled?] true)
        ;; Capture text before deletion for undo
        deleted-text (when (and wasm-instance (pos? length))
                       (let [current-text (.getText ^js wasm-instance)]
                         (subs current-text actual-start actual-end)))]
    ;; Run before-change-functions hook
    (when (pos? length)
      (run-hook-with-args 'before-change-functions actual-start actual-end))
    (when (and wasm-instance (pos? length))
      (.delete ^js wasm-instance actual-start length)
      ;; Update cache
      (let [new-text (.getText ^js wasm-instance)
            lines (clojure.string/split new-text #"\n" -1)
            line-count (count lines)
            ;; Calculate line/col for window cursor
            new-line-col (buf/point-to-line-col {:wasm-instance wasm-instance} actual-start)
            active-window-id (:active-window-id db)]
        (swap! rfdb/app-db
               (fn [current-db]
                 (let [;; Update window-tree cursor position
                       window-tree (:window-tree current-db)
                       new-window-tree (db/update-window-in-tree window-tree active-window-id
                                                                  #(assoc % :cursor-position new-line-col))
                       db-with-cache (-> current-db
                                         (assoc :window-tree new-window-tree)
                                         (assoc-in [:buffers buffer-id :cache :text] new-text)
                                         (assoc-in [:buffers buffer-id :cache :line-count] line-count)
                                         (assoc-in [:buffers buffer-id :point] actual-start)
                                         (assoc-in [:ui :cursor-position] actual-start)
                                         (assoc-in [:buffers buffer-id :is-modified?] true))]
                   ;; Record undo entry if enabled
                   (if (and recording-enabled? undo-enabled? deleted-text)
                     (let [;; Get original point before deletion
                           old-point (get-in current-db [:buffers buffer-id :point] 0)
                           ;; Record point position for restoration
                           point-marker {:type :marker
                                         :marker-id :point
                                         :old-pos old-point
                                         :new-pos actual-start}
                           undo-entry {:type :edit
                                       :op :delete-range
                                       :start actual-start
                                       :length length
                                       :text deleted-text}
                           stack (get-in db-with-cache [:buffers buffer-id :undo-stack] [])]
                       (-> db-with-cache
                           (assoc-in [:buffers buffer-id :undo-stack] (conj stack point-marker undo-entry))
                           ;; Clear redo stack on new edit
                           (assoc-in [:buffers buffer-id :redo-stack] [])))
                     db-with-cache))))
        ;; Run after-change-functions hook: (beg end old-len)
        ;; For delete: beg and end are both actual-start (region collapsed), old-len is the deleted length
        (run-hook-with-args 'after-change-functions actual-start actual-start length))
    nil)))

(defn erase-buffer
  "Delete entire contents of current buffer.

  Usage: (erase-buffer)
  Returns: nil (side effect only)"
  []
  (let [size (buffer-size)]
    (when (pos? size)
      (delete-region 0 size)))
  nil)

(defn- resolve-position
  "Resolve POS to a numeric position.
  If POS is a marker ID, returns its position.
  Otherwise returns POS as-is."
  [pos]
  (let [db @rfdb/app-db
        marker (get-in db [:markers :table pos])]
    (if marker
      ;; It's a marker - get its position from WASM
      (let [buffer-id (:buffer-id marker)
            wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
            wasm-marker-id (:wasm-id marker)]
        (when (and wasm-instance (number? wasm-marker-id))
          (let [marker-pos (js/Number (.getMarkerPosition ^js wasm-instance (js/BigInt wasm-marker-id)))]
            (when (>= marker-pos 0)
              marker-pos))))
      ;; It's a plain number
      pos)))

(defn goto-char
  "Move cursor to position POS.

  POS can be either an integer position or a marker.

  Usage: (goto-char 100)
         (goto-char marker)
  Returns: nil (side effect only)"
  [pos]
  (let [actual-pos (resolve-position pos)]
    (when actual-pos
      (let [db @rfdb/app-db
            active-window-id (:active-window-id db)
            active-window (db/find-window-in-tree (:window-tree db) active-window-id)
            buffer-id (:buffer-id active-window)
            active-buffer (get (:buffers db) buffer-id)
            line-col (buf/point-to-line-col active-buffer actual-pos)]
        ;; Update window-tree cursor position directly (dispatch-sync doesn't work for nested calls)
        (swap! rfdb/app-db
               (fn [current-db]
                 (let [new-tree (db/update-window-in-tree (:window-tree current-db) active-window-id
                                                           #(assoc % :cursor-position line-col))]
                   (-> current-db
                       (assoc :window-tree new-tree)
                       (assoc-in [:buffers buffer-id :point] actual-pos)
                       (assoc-in [:ui :cursor-position] actual-pos))))))))
  nil)

;; =============================================================================
;; Kill Ring Functions
;; =============================================================================

(defn kill-region
  "Kill (cut) text between START and END.

  Usage: (kill-region start end)
  Returns: nil (side effect only)"
  [start end]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (rf/dispatch-sync [:edit/kill-region buffer-id start end])
    nil))

(defn yank
  "Yank (paste) from kill ring at point.

  Usage: (yank)
  Returns: nil (side effect only)"
  []
  (let [db @rfdb/app-db
        kill-ring (get db :kill-ring [])
        yank-pointer (get db :kill-ring-yank-pointer 0)
        text-to-yank (when (seq kill-ring) (nth kill-ring yank-pointer nil))]
    (when text-to-yank
      (let [pos (point)]
        (insert text-to-yank)
        ;; Track bounds for yank-pop
        (swap! rfdb/app-db
               (fn [current-db]
                 (-> current-db
                     (assoc :last-command :yank)
                     (assoc :last-yank-bounds {:start pos :end (+ pos (count text-to-yank))})))))))
  nil)

(defn kill-new
  "Add TEXT to front of kill ring.

  Usage: (kill-new \"text\")
  Returns: nil"
  [text]
  (when (and text (not (empty? (str text))))
    (swap! rfdb/app-db
           (fn [db]
             (let [kill-ring (get db :kill-ring [])
                   kill-ring-max (get db :kill-ring-max 120)
                   new-ring (take kill-ring-max (cons (str text) kill-ring))]
               (-> db
                   (assoc :kill-ring (vec new-ring))
                   (assoc :kill-ring-yank-pointer 0))))))
  nil)

(defn current-kill
  "Return the N-th entry in the kill ring.

  N=0 is the most recent, N=1 is the previous, etc.
  Also rotates the yank pointer to that entry.

  Usage: (current-kill 0)
  Returns: String or nil"
  [n]
  (let [db @rfdb/app-db
        kill-ring (get db :kill-ring [])
        index (mod n (max 1 (count kill-ring)))]
    (when (seq kill-ring)
      ;; Update the yank pointer
      (swap! rfdb/app-db assoc :kill-ring-yank-pointer index)
      (nth kill-ring index nil))))

(defn kill-append
  "Append TEXT to most recent kill ring entry.

  If BEFORE-P is non-nil, prepend instead of append.

  Usage: (kill-append \" more\" nil)
  Returns: nil"
  [text before-p]
  (when (and text (not (empty? (str text))))
    (swap! rfdb/app-db
           (fn [db]
             (let [kill-ring (get db :kill-ring [])
                   text-str (str text)]
               (if (seq kill-ring)
                 (let [first-kill (first kill-ring)
                       new-kill (if before-p
                                  (str text-str first-kill)
                                  (str first-kill text-str))]
                   (assoc db :kill-ring (vec (cons new-kill (rest kill-ring)))))
                 ;; No existing kill, just add it
                 (assoc db :kill-ring [text-str]))))))
  nil)

(defn copy-region-as-kill
  "Save region text to kill ring without deleting.

  Usage: (copy-region-as-kill start end)
  Returns: nil"
  [start end]
  (let [text (buffer-substring start end)]
    (when text
      (kill-new text)))
  nil)

(defn yank-pop
  "Replace just-yanked text with previous kill.

  Only valid immediately after yank or yank-pop.

  Usage: (yank-pop)
  Returns: nil"
  []
  (let [db @rfdb/app-db
        last-cmd (get db :last-command)
        kill-ring (get db :kill-ring [])
        yank-pointer (get db :kill-ring-yank-pointer 0)]
    (when (and (#{:yank :yank-pop} last-cmd)
               (> (count kill-ring) 1))
      ;; Get previous yank boundaries from last-yank-bounds
      (let [bounds (get db :last-yank-bounds)
            start (:start bounds)
            end (:end bounds)
            next-index (mod (inc yank-pointer) (count kill-ring))
            new-text (nth kill-ring next-index)]
        (when (and start end new-text)
          ;; Delete the yanked text
          (delete-region start end)
          ;; Insert new text
          (goto-char start)
          (insert new-text)
          ;; Update state
          (swap! rfdb/app-db
                 (fn [current-db]
                   (-> current-db
                       (assoc :kill-ring-yank-pointer next-index)
                       (assoc :last-command :yank-pop)
                       (assoc :last-yank-bounds {:start start :end (+ start (count new-text))}))))))))
  nil)

(defn kill-line
  "Kill to end of line, or kill newline if at end.

  With ARG, kill that many lines.

  Usage: (kill-line)
  Returns: nil"
  ([]
   (kill-line 1))
  ([arg]
   (let [pos (point)
         eol (line-end-position)]
     (if (= pos eol)
       ;; At end of line, kill the newline
       (let [text (buffer-substring pos (+ pos 1))]
         (when (= text "\n")
           (kill-new "\n")
           (delete-region pos (+ pos 1))))
       ;; Kill to end of line
       (let [text (buffer-substring pos eol)]
         (when (and text (pos? (count text)))
           (kill-new text)
           (delete-region pos eol)))))
   nil))

(defn- whitespace?
  "Check if character is whitespace (ClojureScript compatible)."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

(defn kill-word
  "Kill characters forward until word boundary.

  With ARG, kill that many words.

  Usage: (kill-word 1)
  Returns: nil"
  [arg]
  ;; Simple implementation: kill from point to next whitespace/non-word
  (let [pos (point)
        text (buffer-string)
        text-len (count text)]
    (when (< pos text-len)
      (let [;; Find end of word (skip non-space then skip space)
            end-pos (loop [i pos
                           in-word false]
                      (if (>= i text-len)
                        i
                        (let [ch (nth text i)]
                          (cond
                            ;; Whitespace after word - done
                            (and in-word (whitespace? ch)) i
                            ;; Word char - mark we're in a word
                            (not (whitespace? ch)) (recur (inc i) true)
                            ;; Skip leading whitespace
                            :else (recur (inc i) false)))))
            killed-text (buffer-substring pos end-pos)]
        (when (and killed-text (pos? (count killed-text)))
          (kill-new killed-text)
          (delete-region pos end-pos)))))
  nil)

(defn backward-kill-word
  "Kill characters backward until word boundary.

  With ARG, kill that many words.

  Usage: (backward-kill-word 1)
  Returns: nil"
  [arg]
  (let [pos (point)
        text (buffer-string)]
    (when (pos? pos)
      (let [;; Find start of word going backward
            start-pos (loop [i (dec pos)
                             in-word false]
                        (if (< i 0)
                          0
                          (let [ch (nth text i)]
                            (cond
                              ;; Whitespace after word - done
                              (and in-word (whitespace? ch)) (inc i)
                              ;; Word char - mark we're in a word
                              (not (whitespace? ch)) (recur (dec i) true)
                              ;; Skip trailing whitespace
                              :else (recur (dec i) false)))))
            killed-text (buffer-substring start-pos pos)]
        (when (and killed-text (pos? (count killed-text)))
          (kill-new killed-text)
          (delete-region start-pos pos)))))
  nil)

;; =============================================================================
;; Mark and Region
;; =============================================================================

(defn set-mark
  "Set mark at position POS (or point if nil).

  Usage: (set-mark 100)
  Returns: nil (side effect only)"
  [pos]
  (let [db @rfdb/app-db
        actual-pos (or pos (buf/point db))
        window-id (:active-window-id db)]
    ;; Mark is stored as a linear position in the window
    (rf/dispatch-sync [:window/set-mark window-id actual-pos])
    nil))

(defn mark
  "Return position of mark, or nil if not set.

  Usage: (mark)
  Returns: Integer position or nil"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))]
    ;; Mark is stored as a linear position
    (:mark-position active-window)))

(defn exchange-point-and-mark
  "Exchange point and mark.

  Usage: (exchange-point-and-mark)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:mark/exchange-point-and-mark])
  nil)

;; =============================================================================
;; Buffer Management
;; =============================================================================

(defn create-buffer
  "Create a new buffer with NAME.

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler (see lexicon.core.api.message for same pattern).

  Usage: (create-buffer \"*scratch*\")
  Returns: Buffer ID"
  [name]
  (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer ""))]
    (when wasm-instance
      ;; Directly update db to avoid dispatch-sync issues when called from event handlers
      ;; Same pattern as lexicon.core.api.message/message
      (let [buffer-id (db/next-buffer-id (:buffers @rfdb/app-db))
            new-buffer (db/create-buffer buffer-id name wasm-instance)]
        (swap! rfdb/app-db assoc-in [:buffers buffer-id] new-buffer)
        buffer-id))))

(defn- update-window-buffer
  "Update the buffer-id in the active window.
  Helper for switch-to-buffer that mimics :window/show-buffer event."
  [tree active-window-id buffer-id]
  (cond
    (nil? tree) nil
    (and (= (:type tree) :leaf)
         (= (:id tree) active-window-id))
    (assoc tree :buffer-id buffer-id)
    (= (:type tree) :split)
    (assoc tree
           :first (update-window-buffer (:first tree) active-window-id buffer-id)
           :second (update-window-buffer (:second tree) active-window-id buffer-id))
    :else tree))

(defn switch-to-buffer
  "Switch to buffer NAME (create if doesn't exist).

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler.

  Usage: (switch-to-buffer \"*scratch*\")
  Returns: nil (side effect only)"
  [name]
  (let [db @rfdb/app-db
        existing-buffer (buf/get-buffer db name)
        buffer-id (or (:id existing-buffer) (create-buffer name))]
    ;; Directly update window-tree to show buffer (same as :window/show-buffer)
    (when buffer-id
      (let [active-window-id (:active-window-id @rfdb/app-db)
            window-tree (:window-tree @rfdb/app-db)
            new-tree (update-window-buffer window-tree active-window-id buffer-id)]
        (swap! rfdb/app-db assoc :window-tree new-tree)))
    nil))

(defn current-buffer
  "Return ID of current buffer.

  Usage: (current-buffer)
  Returns: Buffer ID (integer)"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))]
    (:buffer-id active-window)))

(defn buffer-name-from-id
  "Return name of buffer with ID.

  Usage: (buffer-name-from-id 5)
  Returns: String (buffer name)"
  [buffer-id]
  (let [db @rfdb/app-db
        buffer (get-in db [:buffers buffer-id])]
    (:name buffer)))

(defn buffer-list
  "Return list of all buffer names.

  Usage: (buffer-list)
  Returns: Vector of strings"
  []
  (let [db @rfdb/app-db
        buffers (:buffers db)]
    (vec (map :name (vals buffers)))))

;; =============================================================================
;; Message/Echo Area
;; =============================================================================

(defn message
  "Display MESSAGE in echo area.

  Usage: (message \"Hello, world!\")
  Returns: nil (side effect only)"
  [& args]
  (let [text (apply str args)]
    (msg/message text)
    nil))

;; =============================================================================
;; Undo
;; =============================================================================

(defn undo
  "Undo last change in current buffer.

  Usage: (undo)
  Returns: nil (side effect only)"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (rf/dispatch-sync [:edit/undo buffer-id])
    nil))

(defn undo-boundary
  "Mark a boundary in the undo history.

  Edits between boundaries are undone as a single unit.
  Called automatically at command boundaries.

  Usage: (undo-boundary)
  Returns: nil"
  []
  (let [buffer-id (current-buffer)]
    (advanced-undo/undo-boundary! buffer-id)
    nil))

(defn redo
  "Redo the last undone change in current buffer.

  Usage: (redo)
  Returns: nil (side effect only)"
  []
  (let [buffer-id (current-buffer)]
    (advanced-undo/redo! buffer-id)
    nil))

;; =============================================================================
;; Window Management
;; =============================================================================

(defn split-window-horizontally
  "Split active window horizontally.

  Usage: (split-window-horizontally)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/split :horizontal])
  nil)

(defn delete-other-windows
  "Delete all windows except active one.

  Usage: (delete-other-windows)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/delete-others])
  nil)

;; =============================================================================
;; Buffer-File Association
;; =============================================================================

(defn set-visited-file-name
  "Associate file path with current buffer.

  Usage: (set-visited-file-name \"/path/to/file.txt\")
  Returns: nil (side effect only)"
  [file-path]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/set-file buffer-id file-path])
    nil))

(defn buffer-file-name
  "Return file path associated with current buffer.

  Usage: (buffer-file-name)
  Returns: String (file path) or nil"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (:file-handle buffer)))

;; =============================================================================
;; Buffer Modes
;; =============================================================================

(defn set-major-mode
  "Set major mode for current buffer.

  Usage: (set-major-mode 'clojure-mode)
  Returns: nil (side effect only)"
  [mode-symbol]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:mode/set-major buffer-id mode-symbol])
    nil))

(defn major-mode
  "Return major mode of current buffer.

  Usage: (major-mode)
  Returns: Symbol (mode name)"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (:major-mode buffer)))

(defn enable-minor-mode
  "Enable minor mode for current buffer.

  Usage: (enable-minor-mode 'line-numbers-mode)
  Returns: nil (side effect only)"
  [mode-symbol]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:mode/enable-minor buffer-id mode-symbol])
    nil))

(defn minor-mode-enabled?
  "Check if minor mode is enabled in current buffer.

  Usage: (minor-mode-enabled? 'line-numbers-mode)
  Returns: Boolean"
  [mode-symbol]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (contains? (:minor-modes buffer #{}) mode-symbol)))

;; =============================================================================
;; Buffer Properties
;; =============================================================================

(defn set-buffer-read-only
  "Set read-only flag for current buffer.

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler.

  Usage: (set-buffer-read-only t)
  Returns: nil (side effect only)"
  [read-only?]
  (let [buffer-id (current-buffer)]
    (swap! rfdb/app-db assoc-in [:buffers buffer-id :is-read-only?] read-only?)
    nil))

(defn buffer-modified-p
  "Return non-nil if current buffer has been modified since last save.

  Usage: (buffer-modified-p)
  Returns: Boolean"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (get buffer :is-modified? false)))

(defn set-buffer-modified-p
  "Set the modified flag for current buffer.

  If FLAG is nil, mark buffer as unmodified.
  If FLAG is non-nil, mark buffer as modified.

  Usage: (set-buffer-modified-p nil)  ; mark as unmodified
         (set-buffer-modified-p t)    ; mark as modified
  Returns: FLAG"
  [flag]
  (let [buffer-id (current-buffer)]
    (swap! rfdb/app-db assoc-in [:buffers buffer-id :is-modified?] (boolean flag))
    flag))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn save-buffer
  "Save current buffer to its associated file.

  Usage: (save-buffer)
  Returns: nil (side effect only)"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:save-buffer buffer-id])
    nil))

(defn revert-buffer
  "Revert current buffer to contents from disk.

  Usage: (revert-buffer)
  Returns: nil (side effect only)"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:revert-buffer buffer-id])
    nil))

;; =============================================================================
;; Minibuffer
;; =============================================================================

(defn read-string
  "Read a string from minibuffer with PROMPT.

  Usage: (read-string \"Enter text: \")
  Returns: String (user input)"
  [prompt]
  ;; TODO: This needs to be async or return a promise
  ;; For now, synchronous version for tests
  (rf/dispatch-sync [:minibuffer/activate {:prompt prompt
                                            :on-confirm [:minibuffer/deactivate]
                                            :on-cancel [:minibuffer/deactivate]}])
  ;; Return empty string for now - real implementation needs completion
  "")

;; =============================================================================
;; Keymaps
;; =============================================================================

(defn global-set-key
  "Bind KEY-SEQUENCE to COMMAND in global keymap.

  Usage: (global-set-key \"C-c C-c\" 'my-command)
  Returns: nil (side effect only)"
  [key-sequence command]
  (rf/dispatch-sync [:keymap/set-global key-sequence command])
  nil)

(defn local-set-key
  "Bind KEY-SEQUENCE to COMMAND in current buffer's local keymap.

  Usage: (local-set-key \"C-c C-c\" 'my-command)
  Returns: nil (side effect only)"
  [key-sequence command]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:keymap/set-local buffer-id key-sequence command])
    nil))

(defn key-binding
  "Look up KEY-SEQUENCE and return bound command.

  Usage: (key-binding \"C-c C-c\")
  Returns: Command symbol or nil"
  [key-sequence]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)]
    (or (get-in db [:buffers buffer-id :local-keymap key-sequence])
        (get-in db [:keymaps :global :bindings key-sequence]))))

;; =============================================================================
;; Commands
;; =============================================================================

(def ^:private lisp-command-registry
  "Registry of Lisp command functions.
   Maps command keywords to their implementing functions."
  (atom {}))

(defn define-command
  "Define a command that can be invoked via M-x.

  This is how packages register commands with the editor.
  The function will be called when the command is executed.

  Usage: (define-command 'my-command my-fn \"Description\" {:interactive ...})
  Returns: nil

  Options:
  - :interactive - Interactive spec for prompting (optional)
    Example: [{:type :async :prompt \"Directory: \"}]"
  ([command-name command-fn docstring]
   (define-command command-name command-fn docstring {}))
  ([command-name command-fn docstring options]
   (let [cmd-keyword (if (symbol? command-name) (keyword command-name) command-name)]
     (rf/dispatch-sync [:register-command cmd-keyword
                        (merge {:docstring docstring
                                :handler [:run-lisp-command cmd-keyword]}
                               (when (:interactive options)
                                 {:interactive (:interactive options)}))])
     ;; Store the function in a registry for :run-lisp-command to find
     (swap! lisp-command-registry assoc cmd-keyword command-fn)
     nil)))

(defn run-lisp-command
  "Execute a Lisp command by keyword.
   Called by :run-lisp-command event handler.

  Usage: (run-lisp-command :dired \"/home\")
  Returns: Result of command function"
  [command-keyword & args]
  (when-let [command-fn (get @lisp-command-registry command-keyword)]
    (apply command-fn args)))

;; Register the event handler for running lisp commands
;; This is done here to avoid circular deps with events/command.cljs
(rf/reg-event-fx
 :run-lisp-command
 (fn [_ [_ command-keyword & args]]
   (apply run-lisp-command command-keyword args)
   {}))

(defn call-interactively
  "Call COMMAND interactively.

  Usage: (call-interactively 'save-buffer)
  Returns: Result of command"
  [command-name]
  (let [db @rfdb/app-db
        cmd-keyword (if (symbol? command-name) (keyword command-name) command-name)
        command-def (get-in db [:commands cmd-keyword])
        handler (:handler command-def)]
    (when handler
      (rf/dispatch-sync handler))))

;; =============================================================================
;; Echo Area
;; =============================================================================

(defn current-message
  "Return current message in echo area.

  Usage: (current-message)
  Returns: String or nil"
  []
  (let [db @rfdb/app-db]
    (get-in db [:minibuffer :message])))

;; =============================================================================
;; Filesystem Primitives (for packages like Dired)
;; =============================================================================

;; Note: These use a mock filesystem for now. Real filesystem access
;; will be implemented via File System Access API when available.

(def ^:private mock-filesystem
  "Mock filesystem for testing Dired and other packages.
   In production, this will be replaced with real FS access."
  {"/" [{:name "home" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "usr" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "tmp" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "etc" :type :directory :size 4096 :modified "2025-01-20"}]
   "/home" [{:name "user" :type :directory :size 4096 :modified "2025-01-20"}]
   "/home/user" [{:name "documents" :type :directory :size 4096 :modified "2025-01-20"}
                 {:name "file.txt" :type :file :size 1234 :modified "2025-01-20"}
                 {:name "notes.org" :type :file :size 5678 :modified "2025-01-20"}]})

(defn directory-files
  "Return list of files in DIRECTORY.

  Usage: (directory-files \"/home/user\")
  Returns: Vector of filename strings"
  [directory]
  (let [entries (get mock-filesystem directory [])]
    (mapv :name entries)))

(defn file-attributes
  "Return attributes of FILE as a map.

  Usage: (file-attributes \"/home/user/file.txt\")
  Returns: {:name \"file.txt\" :type :file :size 1234 :modified \"...\"}"
  [file-path]
  (let [parent-dir (or (re-find #".*/" file-path) "/")
        filename (last (str/split file-path #"/"))
        entries (get mock-filesystem (str/replace parent-dir #"/$" "") [])]
    (first (filter #(= (:name %) filename) entries))))

(defn file-directory-p
  "Return true if PATH is a directory.

  Usage: (file-directory-p \"/home\")
  Returns: Boolean"
  [path]
  (let [attrs (file-attributes path)]
    (= (:type attrs) :directory)))

(defn file-exists-p
  "Return true if FILE exists.

  Usage: (file-exists-p \"/home/user/file.txt\")
  Returns: Boolean"
  [file-path]
  (some? (file-attributes file-path)))

(defn insert-directory
  "Insert directory listing for DIR at point.

  This is the core primitive for Dired buffer generation.
  Formats entries in ls -l style.

  Usage: (insert-directory \"/home/user\")
  Returns: nil (side effect: inserts text)"
  [directory]
  (let [entries (get mock-filesystem directory [])
        header (str "  " directory ":\n")
        pad-left (fn [s width]
                   (let [s (str s)
                         pad (- width (count s))]
                     (if (pos? pad)
                       (str (apply str (repeat pad " ")) s)
                       s)))
        format-entry (fn [{:keys [name type size modified]}]
                       (let [type-char (if (= type :directory) "d" "-")
                             perms "rwxr-xr-x"]
                         (str "  " type-char perms "  "
                              (pad-left size 6) "  "
                              modified "  " name)))
        lines (map format-entry entries)
        content (str header (str/join "\n" lines) "\n")]
    (insert content)))

;; =============================================================================
;; Markers (Issue #101)
;; =============================================================================

(defn make-marker
  "Create a new marker at POSITION in current buffer.

  If POSITION is nil, creates a marker not pointing anywhere.
  Optional INSERTION-TYPE controls behavior when text is inserted at marker:
  - nil (default): marker stays before inserted text
  - true: marker advances past inserted text

  Usage: (make-marker)
         (make-marker 0)
         (make-marker 100 t)
  Returns: Marker ID (integer)"
  ([] (make-marker nil nil))
  ([position] (make-marker position nil))
  ([position insertion-type]
   (let [buffer-id (current-buffer)]
     (if (nil? position)
       ;; Create marker not pointing anywhere
       (let [marker-id (get-in @rfdb/app-db [:markers :next-id] 0)]
         (swap! rfdb/app-db
                (fn [db]
                  (-> db
                      (assoc-in [:markers :table marker-id]
                                {:id marker-id
                                 :buffer-id nil
                                 :kind :custom
                                 :wasm-id nil
                                 :insertion-type insertion-type
                                 :created-at (js/Date.now)
                                 :metadata {}})
                      (update-in [:markers :next-id] inc))))
         marker-id)
       ;; Create marker at position using re-frame dispatch
       (do
         (rf/dispatch-sync [:markers/create buffer-id position :custom insertion-type {}])
         ;; Get the created marker ID (it's next-id - 1 after creation)
         (dec (get-in @rfdb/app-db [:markers :next-id] 1)))))))

(defn marker-position
  "Return position of MARKER, or nil if marker points nowhere.

  Usage: (marker-position marker)
  Returns: Integer position or nil"
  [marker-id]
  ;; Access db directly (subscriptions don't work in SCI context)
  (let [db @rfdb/app-db
        marker (get-in db [:markers :table marker-id])
        buffer-id (:buffer-id marker)
        wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
        wasm-marker-id (:wasm-id marker)]
    (when (and wasm-instance (number? wasm-marker-id))
      ;; WASM expects BigInt for marker ID, returns BigInt (i64), convert to JS Number
      (let [pos (js/Number (.getMarkerPosition ^js wasm-instance (js/BigInt wasm-marker-id)))]
        (when (>= pos 0)
          pos)))))

(defn marker-buffer
  "Return buffer that MARKER points into, or nil if none.

  Usage: (marker-buffer marker)
  Returns: Buffer ID or nil"
  [marker-id]
  (get-in @rfdb/app-db [:markers :table marker-id :buffer-id]))

(defn set-marker
  "Set MARKER to point to POSITION in BUFFER.

  If POSITION is nil, unset the marker (it points nowhere).
  If BUFFER is nil, uses current buffer.

  Usage: (set-marker marker position)
         (set-marker marker position buffer-id)
  Returns: marker-id"
  ([marker-id position] (set-marker marker-id position nil))
  ([marker-id position buffer-id]
   (let [db @rfdb/app-db
         marker (get-in db [:markers :table marker-id])
         target-buffer-id (or buffer-id (current-buffer))]
     (if (nil? position)
       ;; Unset marker
       (do
         (when-let [old-wasm-id (:wasm-id marker)]
           (when-let [wasm-instance (get-in db [:buffers (:buffer-id marker) :wasm-instance])]
             (.deleteMarker ^js wasm-instance (js/BigInt old-wasm-id))))
         (swap! rfdb/app-db
                (fn [db]
                  (-> db
                      (assoc-in [:markers :table marker-id :buffer-id] nil)
                      (assoc-in [:markers :table marker-id :wasm-id] nil)
                      (update-in [:buffers (:buffer-id marker) :markers] disj marker-id))))
         marker-id)
       ;; Set marker to position
       (let [wasm-instance (get-in db [:buffers target-buffer-id :wasm-instance])]
         (when wasm-instance
           ;; Delete old WASM marker if exists
           (when-let [old-wasm-id (:wasm-id marker)]
             (when-let [old-wasm (get-in db [:buffers (:buffer-id marker) :wasm-instance])]
               (.deleteMarker ^js old-wasm (js/BigInt old-wasm-id))))
           ;; Create new WASM marker (WASM returns BigInt, convert to Number)
           (let [new-wasm-id (js/Number (.createMarker ^js wasm-instance position))]
             (when (>= new-wasm-id 0)
               (swap! rfdb/app-db
                      (fn [db]
                        (-> db
                            (assoc-in [:markers :table marker-id :buffer-id] target-buffer-id)
                            (assoc-in [:markers :table marker-id :wasm-id] new-wasm-id)
                            (update-in [:buffers (:buffer-id marker) :markers] disj marker-id)
                            (update-in [:buffers target-buffer-id :markers] (fnil conj #{}) marker-id)))))))
         marker-id)))))

(defn move-marker
  "Move MARKER to POSITION in its current buffer.

  Unlike set-marker, does not change which buffer marker is in.

  Usage: (move-marker marker position)
  Returns: marker-id"
  [marker-id position]
  (let [db @rfdb/app-db
        marker (get-in db [:markers :table marker-id])
        buffer-id (:buffer-id marker)
        wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
        wasm-marker-id (:wasm-id marker)]
    (when (and wasm-instance wasm-marker-id)
      (.moveMarker ^js wasm-instance (js/BigInt wasm-marker-id) position))
    marker-id))

(defn copy-marker
  "Create a copy of MARKER.

  Optional INSERTION-TYPE overrides the original marker's insertion type.
  If not provided, preserves the original marker's insertion type.
  If explicitly provided (even nil), uses the provided value.

  Usage: (copy-marker marker)
         (copy-marker marker t)
         (copy-marker marker nil)
  Returns: New marker ID"
  ([marker-id]
   ;; No override - use original's insertion-type
   (let [db @rfdb/app-db
         marker (get-in db [:markers :table marker-id])
         position (marker-position marker-id)
         type (:insertion-type marker)]
     (if position
       (make-marker position type)
       (make-marker nil type))))
  ([marker-id insertion-type]
   ;; Explicit override - use provided value (even if nil)
   (let [position (marker-position marker-id)]
     (if position
       (make-marker position insertion-type)
       (make-marker nil insertion-type)))))

(defn marker-insertion-type
  "Return insertion type of MARKER.

  Returns nil if marker stays before inserted text,
  true if marker advances past inserted text.

  Usage: (marker-insertion-type marker)
  Returns: nil or true"
  [marker-id]
  (get-in @rfdb/app-db [:markers :table marker-id :insertion-type]))

(defn set-marker-insertion-type
  "Set insertion type of MARKER.

  TYPE = nil: marker stays before text inserted at its position
  TYPE = true: marker advances past text inserted at its position

  Usage: (set-marker-insertion-type marker t)
  Returns: type"
  [marker-id type]
  (swap! rfdb/app-db assoc-in [:markers :table marker-id :insertion-type] type)
  type)

(defn markerp
  "Return true if OBJECT is a marker.

  Usage: (markerp obj)
  Returns: Boolean"
  [obj]
  (and (integer? obj)
       (some? (get-in @rfdb/app-db [:markers :table obj]))))

(defn insert-before-markers
  "Insert TEXT at point, keeping all markers at their original positions.

  Unlike regular insert, markers at insertion point stay BEFORE the inserted
  text (at their original position). This is the opposite of normal insertion
  behavior where markers with insertion-type=t would advance.

  Usage: (insert-before-markers \"text\")
  Returns: nil"
  [text]
  ;; For proper semantics, we need to:
  ;; 1. Find all markers at current position
  ;; 2. Remember their positions
  ;; 3. Insert text (WASM auto-advances markers)
  ;; 4. Move ALL markers back to original position (override normal behavior)
  (let [pos (point)
        buffer-id (current-buffer)
        db @rfdb/app-db
        marker-ids (get-in db [:buffers buffer-id :markers] #{})
        ;; Capture markers at the insertion point BEFORE insert
        markers-at-point (vec (filter (fn [mid]
                                        (= pos (marker-position mid)))
                                      marker-ids))]
    ;; Insert the text (this will auto-advance markers in WASM)
    (insert text)
    ;; Move ALL markers at the insertion point back to their original position
    ;; This overrides the normal WASM auto-advance behavior
    (doseq [mid markers-at-point]
      (move-marker mid pos))
    nil))

;; =============================================================================
;; Type Predicates
;; =============================================================================

(defn functionp
  "Return non-nil if OBJECT is a function.

  A function is any object that can be called with funcall.
  This includes functions, closures, and symbols bound to functions.

  Usage: (functionp obj)
  Returns: Boolean"
  [obj]
  (or (fn? obj)
      (and (symbol? obj)
           (fn? (get @global-vars obj)))))

(defn commandp
  "Return non-nil if OBJECT is a command.

  A command is a function that can be called interactively.
  In Lexicon, this means it's registered via define-command or
  :register-command.

  Usage: (commandp 'save-buffer)
  Returns: t or nil"
  [obj]
  (let [db @rfdb/app-db
        cmd-keyword (cond
                      (keyword? obj) obj
                      (symbol? obj) (keyword obj)
                      (string? obj) (keyword obj)
                      :else nil)]
    (when (and cmd-keyword
               (get-in db [:commands cmd-keyword]))
      true)))

(defn fboundp
  "Return non-nil if SYMBOL's function definition is not void.

  Usage: (fboundp 'my-function)
  Returns: Boolean"
  [symbol]
  (or (some? (get @global-vars symbol))
      (commandp symbol)))

(defn boundp
  "Return non-nil if SYMBOL's value is not void.

  Usage: (boundp 'my-variable)
  Returns: Boolean"
  [symbol]
  (contains? @global-vars symbol))

;; =============================================================================
;; Autoload
;; =============================================================================

(def ^:private autoload-registry
  "Registry of autoload definitions.
   Maps function symbols to autoload info:
   {:filename \"file\"
    :docstring \"doc\"
    :interactive bool
    :type :function|:macro|:keymap}"
  (atom {}))

(defn autoload
  "Define FUNCTION to autoload from FILENAME.

  This creates a placeholder for FUNCTION that will trigger loading
  FILENAME when FUNCTION is first called.

  Arguments:
  - function: Symbol naming the function to autoload
  - filename: String with file/module to load
  - docstring: Optional documentation string
  - interactive: Optional - t if command, nil otherwise
  - type: Optional - 'macro, 'keymap, or nil for function

  Note: Full lazy loading is not yet implemented. This function
  records the autoload definition for future use.

  Usage: (autoload 'run-prolog \"prolog\" \"Run Prolog\" t)
  Returns: nil"
  ([function filename]
   (autoload function filename nil nil nil))
  ([function filename docstring]
   (autoload function filename docstring nil nil))
  ([function filename docstring interactive]
   (autoload function filename docstring interactive nil))
  ([function filename docstring interactive type]
   (let [sym (if (symbol? function) function (symbol function))]
     ;; Store autoload info in registry
     (swap! autoload-registry assoc sym
            {:filename filename
             :docstring docstring
             :interactive interactive
             :type type})
     ;; If interactive, register as a command placeholder
     (when interactive
       (let [cmd-keyword (keyword sym)]
         (rf/dispatch-sync [:register-command cmd-keyword
                            {:docstring (or docstring (str "Autoloaded from " filename))
                             :handler [:autoload-trigger cmd-keyword filename]}])))
     nil)))

(defn autoloadp
  "Return non-nil if OBJECT is an autoload placeholder.

  Usage: (autoloadp 'some-function)
  Returns: t or nil"
  [obj]
  (let [sym (cond
              (symbol? obj) obj
              (keyword? obj) (symbol (name obj))
              :else nil)]
    (when (and sym (contains? @autoload-registry sym))
      true)))

;; Event handler for autoload trigger (placeholder - logs warning)
(rf/reg-event-fx
 :autoload-trigger
 (fn [_ [_ cmd-keyword filename]]
   (js/console.warn (str "Autoload triggered for " cmd-keyword " from " filename
                         " - lazy loading not yet implemented"))
   {}))

;; =============================================================================
;; Additional Buffer Functions
;; =============================================================================

(defn get-buffer-create
  "Get buffer by NAME, creating it if it doesn't exist.

  Usage: (get-buffer-create \"*scratch*\")
  Returns: Buffer ID"
  [buffer-name]
  (if-let [existing (get-buffer buffer-name)]
    existing
    (create-buffer buffer-name)))

(defn kill-buffer
  "Kill buffer with NAME or ID.

  Usage: (kill-buffer \"*scratch*\")
         (kill-buffer buffer-id)
  Returns: t if killed, nil if not"
  [buffer-or-name]
  (let [buffer-id (if (string? buffer-or-name)
                    (get-buffer buffer-or-name)
                    buffer-or-name)]
    (when buffer-id
      (swap! rfdb/app-db update :buffers dissoc buffer-id)
      true)))

(defn looking-at
  "Return t if text after point matches REGEXP.

  Usage: (looking-at \"Hello\")
  Returns: Boolean"
  [regexp]
  (let [text (buffer-string)
        pos (point)
        remaining (subs text pos)
        pattern (re-pattern (str "^" regexp))]
    (boolean (re-find pattern remaining))))

(defn forward-word
  "Move point forward ARG words.

  Returns count of words that could NOT be moved.

  Usage: (forward-word)
         (forward-word 2)
  Returns: Integer"
  ([] (forward-word 1))
  ([arg]
   (let [text (buffer-string)
         text-len (count text)]
     (loop [remaining arg
            pos (point)]
       (if (or (<= remaining 0) (>= pos text-len))
         (do
           (goto-char pos)
           (max 0 remaining))
         ;; Skip whitespace
         (let [pos-after-ws (loop [p pos]
                              (if (and (< p text-len)
                                       (let [ch (nth text p)]
                                         (or (= ch \space) (= ch \tab) (= ch \newline))))
                                (recur (inc p))
                                p))
               ;; Skip word chars
               pos-after-word (loop [p pos-after-ws]
                                (if (and (< p text-len)
                                         (let [ch (nth text p)]
                                           (not (or (= ch \space) (= ch \tab) (= ch \newline)))))
                                  (recur (inc p))
                                  p))]
           (if (= pos-after-word pos-after-ws)
             ;; No word found
             (do
               (goto-char pos-after-ws)
               remaining)
             (recur (dec remaining) pos-after-word))))))))

(defn lisp-error
  "Signal an error with MESSAGE.

  Usage: (error \"Something went wrong\")
  Throws: Exception"
  [& args]
  (throw (js/Error. (apply str args))))

;; =============================================================================
;; Export for SCI
;; =============================================================================

(def sci-namespace
  "Namespace map for SCI context.

  This is the complete public Lisp API."
  {;; Point and position
   'point point
   'point-min point-min
   'point-max point-max
   'goto-char goto-char
   'line-beginning-position line-beginning-position
   'line-end-position line-end-position
   ;; Position predicates
   'bobp bobp
   'eobp eobp
   'bolp bolp
   'eolp eolp
   ;; Character access
   'char-after char-after
   'char-before char-before
   ;; Region
   'region-beginning region-beginning
   'region-end region-end
   'delete-and-extract-region delete-and-extract-region
   ;; Text properties
   'put-text-property put-text-property
   'get-text-property get-text-property
   'remove-text-properties remove-text-properties
   ;; Line navigation
   'current-line current-line
   'line-count line-count
   'forward-line forward-line
   'beginning-of-line beginning-of-line
   'end-of-line end-of-line
   ;; Buffer content
   'buffer-string buffer-string
   'buffer-substring buffer-substring
   'buffer-substring-no-properties buffer-substring-no-properties
   'insert insert
   'delete-region delete-region
   'erase-buffer erase-buffer
   ;; Search
   'search-forward search-forward
   'search-backward search-backward
   ;; Buffer info
   'buffer-name buffer-name
   'buffer-size buffer-size
   'current-buffer current-buffer
   'buffer-name-from-id buffer-name-from-id
   'buffer-list buffer-list
   'get-buffer get-buffer
   ;; Buffer management
   'create-buffer create-buffer
   'switch-to-buffer switch-to-buffer
   ;; Kill ring
   'kill-region kill-region
   'yank yank
   'kill-new kill-new
   'current-kill current-kill
   'kill-append kill-append
   'copy-region-as-kill copy-region-as-kill
   'yank-pop yank-pop
   'kill-line kill-line
   'kill-word kill-word
   'backward-kill-word backward-kill-word
   ;; Mark
   'set-mark set-mark
   'mark mark
   'exchange-point-and-mark exchange-point-and-mark
   ;; Undo
   'undo undo
   'undo-boundary undo-boundary
   'redo redo
   ;; Windows
   'split-window-horizontally split-window-horizontally
   'delete-other-windows delete-other-windows
   ;; Files
   'set-visited-file-name set-visited-file-name
   'buffer-file-name buffer-file-name
   'save-buffer save-buffer
   'revert-buffer revert-buffer
   ;; Modes
   'set-major-mode set-major-mode
   'major-mode major-mode
   'enable-minor-mode enable-minor-mode
   'minor-mode-enabled? minor-mode-enabled?
   ;; Buffer properties
   'set-buffer-read-only set-buffer-read-only
   'buffer-modified-p buffer-modified-p
   'set-buffer-modified-p set-buffer-modified-p
   ;; Minibuffer
   'read-string read-string
   ;; Keymaps
   'global-set-key global-set-key
   'local-set-key local-set-key
   'key-binding key-binding
   ;; Commands
   'call-interactively call-interactively
   'commandp commandp
   ;; Type predicates (#106)
   'functionp functionp
   'fboundp fboundp
   'boundp boundp
   'autoloadp autoloadp
   ;; Autoload (#106)
   'autoload autoload
   ;; Messages
   'message message
   'current-message current-message
   ;; Filesystem (for packages like Dired)
   'directory-files directory-files
   'file-attributes file-attributes
   'file-directory-p file-directory-p
   'file-exists-p file-exists-p
   'insert-directory insert-directory
   ;; Phase 6.6: Narrowing (Issue #100)
   'narrow-to-region narrow-to-region
   'widen widen
   'buffer-narrowed-p buffer-narrowed-p
   ;; Phase 6.6: Buffer-local variables (Issue #100)
   'make-local-variable make-local-variable
   'buffer-local-value buffer-local-value
   'setq-local setq-local
   'kill-all-local-variables kill-all-local-variables
   ;; Phase 6.6: Buffer operations (Issue #100)
   'set-buffer set-buffer
   'rename-buffer rename-buffer
   'other-buffer other-buffer
   'buffer-enable-undo buffer-enable-undo
   'buffer-disable-undo buffer-disable-undo
   ;; Phase 6.6: Markers (Issue #101)
   'make-marker make-marker
   'marker-position marker-position
   'marker-buffer marker-buffer
   'set-marker set-marker
   'move-marker move-marker
   'copy-marker copy-marker
   'marker-insertion-type marker-insertion-type
   'set-marker-insertion-type set-marker-insertion-type
   'markerp markerp
   'insert-before-markers insert-before-markers
   ;; Global variables (#106)
   'setq setq
   'symbol-value symbol-value
   '*prefix-arg* (fn [] (get @global-vars '*prefix-arg*))
   ;; Hooks (#106)
   'add-hook add-hook
   'remove-hook remove-hook
   'run-hooks run-hooks
   'run-hook-with-args run-hook-with-args
   'run-hook-with-args-until-success run-hook-with-args-until-success
   'run-hook-with-args-until-failure run-hook-with-args-until-failure
   ;; Additional buffer functions
   'get-buffer-create get-buffer-create
   'kill-buffer kill-buffer
   'looking-at looking-at
   'forward-word forward-word
   'error lisp-error})
