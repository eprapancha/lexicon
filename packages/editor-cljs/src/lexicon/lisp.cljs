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
            [lexicon.core.minibuffer :as minibuffer]
            [lexicon.core.markers :as markers]
            [lexicon.core.advanced-undo :as advanced-undo]
            [lexicon.core.dynamic :as dyn]))

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

;; =============================================================================
;; inhibit-read-only Support
;; =============================================================================

(defn inhibit-read-only
  "Get the current value of inhibit-read-only.

  Returns: true if read-only is inhibited, false otherwise."
  []
  (dyn/inhibit-read-only?))

(defn with-inhibit-read-only
  "Execute BODY-FN with inhibit-read-only bound to true.

  Allows modification of read-only buffers during the execution of BODY-FN.
  Used by modes like Dired and Help to update their read-only buffers.

  Usage: (with-inhibit-read-only (fn [] (insert \"text\")))
  Returns: The result of BODY-FN"
  [body-fn]
  (binding [dyn/*inhibit-read-only* true]
    (body-fn)))

;; =============================================================================
;; Global Variables (setq/symbol-value)
;; =============================================================================

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

(defn last-command
  "Return the last command that was executed.

  Usage: (last-command)
  Returns: Command keyword or nil"
  []
  (get @global-vars '*last-command*))

(defn this-command
  "Return the command currently being executed.

  Usage: (this-command)
  Returns: Command keyword or nil"
  []
  (get @global-vars '*this-command*))

(defn set-this-command!
  "Set the current command (called by command system).
   Also updates last-command to previous this-command."
  [cmd]
  (let [prev (get @global-vars '*this-command*)]
    (swap! global-vars assoc
           '*last-command* prev
           '*this-command* cmd)))

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

(defn word-before-point
  "Return the word before point and its start position.

  Usage: (word-before-point)
  Returns: {:word string :start position} or nil if no word"
  []
  (let [pos (point)
        text (buffer-string)]
    (when (and text (pos? pos) (<= pos (count text)))
      (let [before-cursor (subs text 0 pos)]
        (loop [idx (dec (count before-cursor))]
          (if (neg? idx)
            {:word before-cursor :start 0}
            (let [ch (nth before-cursor idx)]
              (if (re-matches #"[\w\-_]" (str ch))
                (recur (dec idx))
                {:word (subs before-cursor (inc idx))
                 :start (inc idx)}))))))))

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

(defn text-properties-at
  "Return all text properties at position POS as a plist.

  Returns a map of property names to values for all properties
  that apply at POS.

  Usage: (text-properties-at 5)
  Returns: Map like {:face :bold :read-only true} or {}"
  [pos]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        props (get-in db [:buffers buffer-id :text-properties] {})]
    ;; Find all ranges that contain pos and merge their properties
    (reduce (fn [result [start ranges]]
              (if (<= start pos)
                (reduce (fn [r [end prop-map]]
                          (if (> end pos)
                            (merge r prop-map)
                            r))
                        result
                        ranges)
                result))
            {}
            props)))

(defn add-text-properties
  "Add text properties to range without removing existing ones.

  Unlike put-text-property which overwrites, add-text-properties
  only adds new properties and doesn't change existing values.

  Usage: (add-text-properties 0 10 {:face :bold})
  Returns: t if any property changed, nil otherwise"
  [start end props-map]
  (let [buffer-id (current-buffer)]
    (doseq [[prop value] props-map]
      ;; Convert keyword to symbol for consistency with Emacs
      (let [prop-sym (if (keyword? prop) (symbol (name prop)) prop)]
        ;; Only set if not already present
        (when-not (get-text-property start prop-sym)
          (rf/dispatch-sync [:text-property/put buffer-id start end prop-sym value]))))
    true))

(defn next-property-change
  "Return position of next text property change after POS.

  Finds the next position where any text property changes value.
  Returns nil if no change found before LIMIT (or end of buffer).

  Usage: (next-property-change 0)
         (next-property-change 0 100)
  Returns: Position or nil"
  ([pos] (next-property-change pos nil))
  ([pos limit]
   (let [db @rfdb/app-db
         buffer-id (current-buffer)
         props (get-in db [:buffers buffer-id :text-properties] {})
         text-len (count (buffer-string))
         effective-limit (or limit text-len)
         ;; Collect all start and end positions after pos
         change-points (->> props
                            (mapcat (fn [[start ranges]]
                                      (cons start (keys ranges))))
                            (filter #(and (> % pos) (<= % effective-limit)))
                            (sort)
                            (distinct))]
     (first change-points))))

(defn previous-property-change
  "Return position of previous text property change before POS.

  Finds the previous position where any text property changes value.
  Returns nil if no change found after LIMIT (or beginning of buffer).

  Usage: (previous-property-change 10)
         (previous-property-change 10 0)
  Returns: Position or nil"
  ([pos] (previous-property-change pos nil))
  ([pos limit]
   (let [db @rfdb/app-db
         buffer-id (current-buffer)
         props (get-in db [:buffers buffer-id :text-properties] {})
         effective-limit (or limit 0)
         ;; Collect all start and end positions before pos
         change-points (->> props
                            (mapcat (fn [[start ranges]]
                                      (cons start (keys ranges))))
                            (filter #(and (< % pos) (>= % effective-limit)))
                            (sort)
                            (reverse))]
     (first change-points))))

(defn next-single-property-change
  "Return position where PROPERTY next changes after POS.

  Like next-property-change but only considers a specific property.
  Returns nil if PROPERTY doesn't change before LIMIT.

  Usage: (next-single-property-change 0 'face)
         (next-single-property-change 0 'face 100)
  Returns: Position or nil"
  ([pos property] (next-single-property-change pos property nil))
  ([pos property limit]
   (let [db @rfdb/app-db
         buffer-id (current-buffer)
         props (get-in db [:buffers buffer-id :text-properties] {})
         text-len (count (buffer-string))
         effective-limit (or limit text-len)
         ;; Find positions where this specific property exists
         change-points (->> props
                            (mapcat (fn [[start ranges]]
                                      (for [[end prop-map] ranges
                                            :when (contains? prop-map property)]
                                        [start end])))
                            (mapcat identity)
                            (filter #(and (> % pos) (<= % effective-limit)))
                            (sort)
                            (distinct))]
     (first change-points))))

(defn previous-single-property-change
  "Return position where PROPERTY previously changes before POS.

  Like previous-property-change but only considers a specific property.
  Returns nil if PROPERTY doesn't change after LIMIT.

  Usage: (previous-single-property-change 10 'face)
         (previous-single-property-change 10 'face 0)
  Returns: Position or nil"
  ([pos property] (previous-single-property-change pos property nil))
  ([pos property limit]
   (let [db @rfdb/app-db
         buffer-id (current-buffer)
         props (get-in db [:buffers buffer-id :text-properties] {})
         effective-limit (or limit 0)
         ;; Find positions where this specific property exists
         change-points (->> props
                            (mapcat (fn [[start ranges]]
                                      (for [[end prop-map] ranges
                                            :when (contains? prop-map property)]
                                        [start end])))
                            (mapcat identity)
                            (filter #(and (< % pos) (>= % effective-limit)))
                            (sort)
                            (reverse))]
     (first change-points))))

;; =============================================================================
;; Search Functions
;; =============================================================================

(defn- case-fold-search?
  "Return true if search should be case-insensitive.
   Emacs convention: case-insensitive if search string is all lowercase."
  [string]
  (= string (str/lower-case string)))

(defn- set-point-internal
  "Internal helper to set point position consistently.
   Updates window-tree cursor-position, buffer :point, and :ui :cursor-position."
  [pos]
  (let [db @rfdb/app-db
        active-window-id (:active-window-id db)
        active-window (db/find-window-in-tree (:window-tree db) active-window-id)
        buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) buffer-id)
        line-col (buf/point-to-line-col active-buffer pos)]
    (swap! rfdb/app-db
           (fn [current-db]
             (let [new-tree (db/update-window-in-tree (:window-tree current-db) active-window-id
                                                       #(assoc % :cursor-position line-col))]
               (-> current-db
                   (assoc :window-tree new-tree)
                   (assoc-in [:buffers buffer-id :point] pos)
                   (assoc-in [:ui :cursor-position] pos)))))))

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
           (set-point-internal match-end)
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
         (set-point-internal match-index)
         match-index)))))

;; =============================================================================
;; Buffer Mutation Functions
;; =============================================================================

(defn insert
  "Insert TEXT at point.

  NOTE: Uses direct WASM calls and swap! to update app-db because dispatch-sync
  doesn't work when called from inside an event handler.

  Usage: (insert \"hello\")
  Returns: nil (side effect only)
  Throws: If buffer is read-only and inhibit-read-only is not set"
  [text]
  (let [text-str (str text)]
    (when (pos? (count text-str))
      (let [db @rfdb/app-db
            active-window-id (:active-window-id db)
            active-window (db/find-window-in-tree (:window-tree db) active-window-id)
            buffer-id (:buffer-id active-window)
            buffer (get-in db [:buffers buffer-id])
            is-read-only? (:is-read-only? buffer false)]
        ;; Check read-only status
        (when (and is-read-only? (not (dyn/inhibit-read-only?)))
          (throw (js/Error. "Buffer is read-only")))
        (let [wasm-instance (:wasm-instance buffer)
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
    nil)))

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

(defn delete-char
  "Delete N characters after point.
  If N is negative, delete characters before point.

  Usage: (delete-char 1)  ; delete one character forward
         (delete-char -1) ; delete one character backward
  Returns: nil (side effect only)"
  ([] (delete-char 1))
  ([n]
   (let [pos (point)]
     (if (>= n 0)
       (delete-region pos (+ pos n))
       (delete-region (+ pos n) pos)))
   nil))

(defn insert-at
  "Insert TEXT at specified position POS.

  Usage: (insert-at 10 \"hello\")
  Returns: nil (side effect only)"
  [pos text]
  (let [db @rfdb/app-db
        active-window-id (:active-window-id db)
        active-window (db/find-window-in-tree (:window-tree db) active-window-id)
        buffer-id (:buffer-id active-window)
        buffer (get-in db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)
        text-str (str text)]
    (when (and wasm-instance (pos? (count text-str)))
      (.insert ^js wasm-instance pos text-str)
      (let [new-text (.getText ^js wasm-instance)
            lines (clojure.string/split new-text #"\n" -1)
            line-count (count lines)
            new-pos (+ pos (count text-str))
            new-line-col (buf/point-to-line-col {:wasm-instance wasm-instance} new-pos)]
        (swap! rfdb/app-db
               (fn [current-db]
                 (let [window-tree (:window-tree current-db)
                       new-window-tree (db/update-window-in-tree window-tree active-window-id
                                                                  #(assoc % :cursor-position new-line-col))]
                   (-> current-db
                       (assoc :window-tree new-window-tree)
                       (assoc-in [:buffers buffer-id :cache :text] new-text)
                       (assoc-in [:buffers buffer-id :cache :line-count] line-count)))))))
    nil))

(defn replace-text
  "Replace text from START for LENGTH characters with NEW-TEXT.
   This is an atomic delete+insert operation.

  Usage: (replace-text 10 5 \"hello\")  ; Replace 5 chars at pos 10 with \"hello\"
  Returns: nil (side effect only)"
  [start length new-text]
  (let [db @rfdb/app-db
        active-window-id (:active-window-id db)
        active-window (db/find-window-in-tree (:window-tree db) active-window-id)
        buffer-id (:buffer-id active-window)
        buffer (get-in db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)
        text-str (str new-text)]
    (when wasm-instance
      ;; Delete then insert
      (when (pos? length)
        (.delete ^js wasm-instance start length))
      (when (pos? (count text-str))
        (.insert ^js wasm-instance start text-str))
      ;; Update cache and cursor
      (let [new-text (.getText ^js wasm-instance)
            lines (clojure.string/split new-text #"\n" -1)
            line-count (count lines)
            new-pos (+ start (count text-str))
            new-line-col (buf/point-to-line-col {:wasm-instance wasm-instance} new-pos)]
        (swap! rfdb/app-db
               (fn [current-db]
                 (let [window-tree (:window-tree current-db)
                       new-window-tree (db/update-window-in-tree window-tree active-window-id
                                                                  #(assoc % :cursor-position new-line-col))]
                   (-> current-db
                       (assoc :window-tree new-window-tree)
                       (assoc-in [:buffers buffer-id :cache :text] new-text)
                       (assoc-in [:buffers buffer-id :cache :line-count] line-count)))))))
    nil))

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

(defn create-special-buffer
  "Create a special buffer with NAME, initial CONTENT, and OPTIONS.

  Options:
  - :major-mode - Major mode keyword (default :fundamental-mode)
  - :read-only - Whether buffer is read-only (default false)
  - :properties - Map of additional buffer properties

  Usage: (create-special-buffer \"*Occur*\" \"content\" {:major-mode :occur-mode :read-only true})
  Returns: Buffer ID"
  [name content options]
  (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer (or content "")))]
    (when wasm-instance
      (let [buffer-id (db/next-buffer-id (:buffers @rfdb/app-db))
            base-buffer (db/create-buffer buffer-id name wasm-instance)
            lines (str/split-lines (or content ""))
            line-count (count lines)
            enhanced-buffer (merge base-buffer
                                   {:major-mode (or (:major-mode options) :fundamental-mode)
                                    :is-read-only? (boolean (:read-only options))
                                    :cache {:text (or content "")
                                            :line-count line-count}}
                                   (:properties options))]
        (swap! rfdb/app-db assoc-in [:buffers buffer-id] enhanced-buffer)
        buffer-id))))

(defn set-buffer-property
  "Set a property on a buffer.

  Usage: (set-buffer-property buffer-id :occur-highlights [...])
  Returns: nil"
  [buffer-id prop value]
  (swap! rfdb/app-db assoc-in [:buffers buffer-id prop] value)
  nil)

(defn get-buffer-property
  "Get a property from a buffer.

  Usage: (get-buffer-property buffer-id :occur-highlights)
  Returns: Property value or nil"
  [buffer-id prop]
  (get-in @rfdb/app-db [:buffers buffer-id prop]))

(defn- update-window-buffer
  "Update the buffer-id in the active window.
  Helper for switch-to-buffer that mimics :window/show-buffer event."
  [tree active-window-id buffer-id]
  (cond
    (nil? tree) nil
    (and (= (:type tree) :leaf)
         (= (:id tree) active-window-id))
    (assoc tree :buffer-id buffer-id)
    ;; Split nodes can be :split, :hsplit, or :vsplit
    (or (= (:type tree) :split)
        (= (:type tree) :hsplit)
        (= (:type tree) :vsplit))
    (assoc tree
           :first (update-window-buffer (:first tree) active-window-id buffer-id)
           :second (update-window-buffer (:second tree) active-window-id buffer-id))
    :else tree))

(defn switch-to-buffer
  "Switch to buffer NAME-OR-ID (create if doesn't exist).

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler.

  Usage: (switch-to-buffer \"*scratch*\") or (switch-to-buffer 1)
  Returns: nil (side effect only)"
  [name-or-id]
  (let [db @rfdb/app-db
        ;; Handle both buffer names (strings) and buffer IDs (numbers)
        buffer-id (if (number? name-or-id)
                    ;; Direct buffer ID - verify it exists
                    (when (get-in db [:buffers name-or-id]) name-or-id)
                    ;; Buffer name - look up or create
                    (let [existing-buffer (buf/get-buffer db name-or-id)]
                      (or (:id existing-buffer) (create-buffer name-or-id))))]
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

(defn buffer-list-ids
  "Return list of all buffer IDs.

  Usage: (buffer-list-ids)
  Returns: Vector of integers"
  []
  (let [db @rfdb/app-db
        buffers (:buffers db)]
    (vec (keys buffers))))

(defn buffer-size-of
  "Return size of BUFFER-ID in characters.

  Usage: (buffer-size-of buffer-id)
  Returns: Integer"
  [buffer-id]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])
        wasm (:wasm-instance buffer)]
    (if wasm
      (try (.-length wasm) (catch :default _ 0))
      0)))

(defn buffer-mode-of
  "Return major mode of BUFFER-ID.

  Usage: (buffer-mode-of buffer-id)
  Returns: Keyword"
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :major-mode] :fundamental-mode))

(defn buffer-modified-p-of
  "Return non-nil if BUFFER-ID has been modified.

  Usage: (buffer-modified-p-of buffer-id)
  Returns: Boolean"
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :is-modified?] false))

(defn buffer-read-only-p-of
  "Return non-nil if BUFFER-ID is read-only.

  Usage: (buffer-read-only-p-of buffer-id)
  Returns: Boolean"
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :is-read-only?] false))

(defn buffer-file-name-of
  "Return file path of BUFFER-ID.

  Usage: (buffer-file-name-of buffer-id)
  Returns: String or nil"
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :file-path]))

(defn buffer-info
  "Return map with info about BUFFER-ID.

  Usage: (buffer-info buffer-id)
  Returns: {:id :name :size :mode :modified? :read-only? :file}"
  [buffer-id]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])]
    (when buffer
      {:id buffer-id
       :name (:name buffer "")
       :size (buffer-size-of buffer-id)
       :mode (:major-mode buffer :fundamental-mode)
       :modified? (:is-modified? buffer false)
       :read-only? (:is-read-only? buffer false)
       :file (or (:file-path buffer) "")})))

(defn all-buffer-info
  "Return list of buffer info maps for all buffers.

  Usage: (all-buffer-info)
  Returns: Vector of {:id :name :size :mode :modified? :read-only? :file}"
  []
  (let [db @rfdb/app-db
        buffers (:buffers db)]
    (vec (map (fn [[id _buffer]]
                (buffer-info id))
              buffers))))

(defn buffer-live-p
  "Return non-nil if OBJECT is a buffer which has not been killed.

  OBJECT can be a buffer ID (number), buffer name (string), or nil.
  Returns true if buffer exists, nil otherwise.

  Usage: (buffer-live-p 1) or (buffer-live-p \"*scratch*\")
  Returns: Boolean or nil"
  [object]
  (let [db @rfdb/app-db
        buffers (:buffers db)]
    (cond
      (nil? object) nil
      (number? object) (when (contains? buffers object) true)
      (string? object) (when (some #(= (:name %) object) (vals buffers)) true)
      :else nil)))

(defn generate-new-buffer-name
  "Return a buffer name based on NAME that is not in use.

  If NAME is not in use, returns NAME unchanged.
  Otherwise, appends <2>, <3>, etc. until a unique name is found.

  Usage: (generate-new-buffer-name \"untitled\")
  Returns: String"
  [name]
  (let [db @rfdb/app-db
        existing-names (set (map :name (vals (:buffers db))))]
    (if (not (contains? existing-names name))
      name
      (loop [n 2]
        (let [candidate (str name "<" n ">")]
          (if (not (contains? existing-names candidate))
            candidate
            (recur (inc n))))))))

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
  "Split active window horizontally (side by side).

  Usage: (split-window-horizontally)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/split :horizontal])
  (run-hooks 'window-configuration-change-hook)
  nil)

(defn split-window-vertically
  "Split active window vertically (stacked).

  Usage: (split-window-vertically)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/split :vertical])
  (run-hooks 'window-configuration-change-hook)
  nil)

(defn delete-window
  "Delete the current window.

  Usage: (delete-window)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/delete])
  (run-hooks 'window-configuration-change-hook)
  nil)

(defn delete-other-windows
  "Delete all windows except active one.

  Usage: (delete-other-windows)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/delete-others])
  (run-hooks 'window-configuration-change-hook)
  nil)

(defn other-window
  "Select the next window in cyclic order.

  Usage: (other-window 1)
  Returns: nil (side effect only)"
  [count]
  (rf/dispatch-sync [:window/next-window])
  nil)

(defn selected-window
  "Return the currently selected window.

  Usage: (selected-window)
  Returns: Window ID"
  []
  (:active-window-id @rfdb/app-db))

(defn window-buffer
  "Return the buffer displayed in WINDOW.

  Usage: (window-buffer)
         (window-buffer window)
  Returns: Buffer ID"
  ([] (window-buffer (selected-window)))
  ([window]
   (let [db @rfdb/app-db
         win (db/find-window-in-tree (:window-tree db) window)]
     (:buffer-id win))))

(defn window-list
  "Return list of all windows.

  Usage: (window-list)
  Returns: Vector of window maps with :id, :buffer-id, and :dimensions"
  []
  (let [db @rfdb/app-db]
    (db/get-all-leaf-windows (:window-tree db))))

(defn window-edges
  "Return edges of WINDOW as (LEFT TOP RIGHT BOTTOM).

  Usage: (window-edges)
         (window-edges window-id)
  Returns: Map with :left, :top, :right, :bottom coordinates"
  ([] (window-edges (selected-window)))
  ([window-id]
   (let [db @rfdb/app-db
         win (db/find-window-in-tree (:window-tree db) window-id)
         dims (:dimensions win)]
     (when dims
       {:left (:x dims)
        :top (:y dims)
        :right (+ (:x dims) (:width dims))
        :bottom (+ (:y dims) (:height dims))}))))

(defn select-window
  "Select WINDOW, making it the active window.

  Usage: (select-window window-id)
  Returns: nil (side effect only)"
  [window-id]
  (swap! rfdb/app-db assoc :active-window-id window-id :cursor-owner window-id)
  nil)

(defn windmove-find-window
  "Find window in DIRECTION from current window.
  DIRECTION is one of :left, :right, :up, :down.
  Returns window map or nil."
  [direction]
  (let [all-windows (window-list)
        current-window (first (filter #(= (:id %) (selected-window)) all-windows))
        current-edges (window-edges (:id current-window))
        ;; Helper to check overlap
        overlaps-h? (fn [w]
                      (let [e (window-edges (:id w))]
                        (and (< (:left current-edges) (:right e))
                             (< (:left e) (:right current-edges)))))
        overlaps-v? (fn [w]
                      (let [e (window-edges (:id w))]
                        (and (< (:top current-edges) (:bottom e))
                             (< (:top e) (:bottom current-edges)))))
        candidates
        (case direction
          :left
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(< (:right (window-edges (:id %))) (:left current-edges)))
               (filter overlaps-v?))
          :right
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(> (:left (window-edges (:id %))) (:right current-edges)))
               (filter overlaps-v?))
          :up
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(< (:bottom (window-edges (:id %))) (:top current-edges)))
               (filter overlaps-h?))
          :down
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(> (:top (window-edges (:id %))) (:bottom current-edges)))
               (filter overlaps-h?))
          nil)]
    (when (seq candidates)
      (case direction
        :left  (apply max-key #(:right (window-edges (:id %))) candidates)
        :right (apply min-key #(:left (window-edges (:id %))) candidates)
        :up    (apply max-key #(:bottom (window-edges (:id %))) candidates)
        :down  (apply min-key #(:top (window-edges (:id %))) candidates)))))

(defn windmove-left
  "Select the window to the left of the current one.

  Usage: (windmove-left)
  Returns: nil (side effect only)"
  []
  (if-let [target (windmove-find-window :left)]
    (select-window (:id target))
    (message "No window left"))
  nil)

(defn windmove-right
  "Select the window to the right of the current one.

  Usage: (windmove-right)
  Returns: nil (side effect only)"
  []
  (if-let [target (windmove-find-window :right)]
    (select-window (:id target))
    (message "No window right"))
  nil)

(defn windmove-up
  "Select the window above the current one.

  Usage: (windmove-up)
  Returns: nil (side effect only)"
  []
  (if-let [target (windmove-find-window :up)]
    (select-window (:id target))
    (message "No window above"))
  nil)

(defn windmove-down
  "Select the window below the current one.

  Usage: (windmove-down)
  Returns: nil (side effect only)"
  []
  (if-let [target (windmove-find-window :down)]
    (select-window (:id target))
    (message "No window below"))
  nil)

;; =============================================================================
;; Window Configuration (for winner-mode)
;; =============================================================================

(defn current-window-configuration
  "Return the current window configuration.

  Usage: (current-window-configuration)
  Returns: Window tree structure (opaque to packages)"
  []
  (:window-tree @rfdb/app-db))

(defn set-window-configuration
  "Restore a saved window configuration.

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler.

  Usage: (set-window-configuration config)
  Returns: nil (side effect only)"
  [config]
  ;; Directly update db to bypass potential dispatch-sync issues
  (let [current-active (:active-window-id @rfdb/app-db)
        all-windows (db/get-all-leaf-windows config)
        active-in-new? (some #(= (:id %) current-active) all-windows)
        new-active-id (if active-in-new?
                        current-active
                        (:id (first all-windows)))]
    (swap! rfdb/app-db
           (fn [db]
             (-> db
                 (assoc :window-tree config)
                 (assoc :active-window-id new-active-id)
                 (assoc :cursor-owner new-active-id)))))
  nil)

(defn window-configuration-equal?
  "Check if two window configurations are structurally equal.
  Ignores cursor position and viewport, only compares structure and buffers.

  Usage: (window-configuration-equal? config1 config2)
  Returns: boolean"
  [tree1 tree2]
  (cond
    (and (nil? tree1) (nil? tree2)) true
    (or (nil? tree1) (nil? tree2)) false
    (not= (:type tree1) (:type tree2)) false
    (= (:type tree1) :leaf)
    (and (= (:id tree1) (:id tree2))
         (= (:buffer-id tree1) (:buffer-id tree2)))
    :else
    (and (window-configuration-equal? (:first tree1) (:first tree2))
         (window-configuration-equal? (:second tree1) (:second tree2)))))

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
  Returns: String (file path) or nil

  Note: In Emacs this returns the full path. The :file-path field
  stores the full path string, while :file-handle stores the
  FileSystemFileHandle object (for FS Access API operations)."
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (:file-path buffer)))

;; =============================================================================
;; Buffer Modes
;; =============================================================================

(defn set-major-mode
  "Set major mode for current buffer.

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler (e.g., from a command like dired).

  Usage: (set-major-mode 'clojure-mode)
  Returns: nil (side effect only)"
  [mode-symbol]
  (let [buffer-id (current-buffer)]
    ;; Use swap! directly since dispatch-sync doesn't work inside event handlers
    (swap! rfdb/app-db assoc-in [:buffers buffer-id :major-mode] mode-symbol)
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

;; Storage for minibuffer callbacks (for read-from-minibuffer async API)
(defonce ^:private minibuffer-callbacks (atom {}))
(defonce ^:private callback-counter (atom 0))

(defn read-from-minibuffer
  "Read input from minibuffer with PROMPT, calling CALLBACK with the result.

  This is the async version of read-string, suitable for packages.
  CALLBACK is a function of one argument (the user's input string).

  Usage: (read-from-minibuffer \"Enter name: \" (fn [input] (do-something input)))
  Returns: nil (async operation)"
  [prompt callback]
  (let [callback-id (swap! callback-counter inc)]
    ;; Store the callback
    (swap! minibuffer-callbacks assoc callback-id callback)
    ;; Activate minibuffer with our callback event
    (rf/dispatch [:minibuffer/activate
                  {:prompt prompt
                   :on-confirm [:lisp/minibuffer-callback callback-id]
                   :on-cancel [:lisp/minibuffer-callback callback-id]}])
    nil))

;; Register the callback handler event
(rf/reg-event-fx
 :lisp/minibuffer-callback
 (fn [{:keys [db]} [_ callback-id input]]
   ;; Get and remove the callback
   (when-let [callback (get @minibuffer-callbacks callback-id)]
     (swap! minibuffer-callbacks dissoc callback-id)
     ;; Call the callback with the input
     ;; NOTE: Callback may use swap! to update db, so we read fresh db afterwards
     (callback input))
   ;; Return fresh db state (callback may have modified it via swap!)
   ;; Don't use the original db, as that would overwrite callback's changes
   {:db @rfdb/app-db
    :fx [[:dispatch [:minibuffer/deactivate]]]}))

(rf/reg-event-fx
 :lisp/minibuffer-cancel
 (fn [{:keys [db]} [_ callback-id]]
   ;; Just remove the callback without calling it
   (swap! minibuffer-callbacks dissoc callback-id)
   {:db db
    :fx [[:dispatch [:minibuffer/deactivate]]
         [:dispatch [:echo/message "Cancelled"]]]}))

;; =============================================================================
;; Completion System (Issue #108 - Vertico Prerequisites)
;; =============================================================================

(defn all-completions
  "Return all completions of STRING in COLLECTION.

  COLLECTION can be:
  - A list/vector of strings
  - The symbol 'commands (all registered commands)
  - The symbol 'buffers (all buffer names)

  Optional PREDICATE filters results.

  Usage: (all-completions \"for\" [\"forward\" \"backward\" \"format\"])
         (all-completions \"\" 'commands)
  Returns: List of matching completions"
  ([string collection] (all-completions string collection nil))
  ([string collection predicate]
   (let [candidates (cond
                      (= collection 'commands)
                      (let [db @rfdb/app-db]
                        (keys (get-in db [:commands] {})))

                      (= collection 'buffers)
                      (let [db @rfdb/app-db]
                        (map :name (vals (:buffers db))))

                      (sequential? collection)
                      collection

                      :else [])
         prefix (str/lower-case (str string))
         matches (->> candidates
                      (map str)
                      (filter #(str/starts-with? (str/lower-case %) prefix)))]
     (if predicate
       (filter predicate matches)
       (vec matches)))))

(defn try-completion
  "Try to complete STRING in COLLECTION.

  Returns:
  - t if STRING is already a complete valid match
  - Completed string if unique completion exists
  - nil if no completions

  Usage: (try-completion \"forw\" [\"forward\" \"backward\"])
  Returns: \"forward\" or t or nil"
  ([string collection] (try-completion string collection nil))
  ([string collection predicate]
   (let [matches (all-completions string collection predicate)]
     (cond
       (empty? matches) nil
       ;; Check for exact match first (before unique completion)
       (some #(= (str/lower-case %) (str/lower-case string)) matches) true
       (= 1 (count matches)) (first matches)
       :else (first matches)))))

(defn test-completion
  "Return t if STRING is a valid completion in COLLECTION.

  Usage: (test-completion \"forward\" [\"forward\" \"backward\"])
  Returns: t or nil"
  ([string collection] (test-completion string collection nil))
  ([string collection predicate]
   (let [candidates (all-completions "" collection predicate)]
     (when (some #(= (str/lower-case %) (str/lower-case string)) candidates)
       true))))

(defn minibuffer-completion-table
  "Return the completion table for current minibuffer.

  Returns list of current completion candidates or nil if no completions.

  Usage: (minibuffer-completion-table)
  Returns: List of strings or nil"
  []
  (let [db @rfdb/app-db]
    (minibuffer/get-completions db)))

(defn completion-metadata-get
  "Get metadata property PROP from current minibuffer completion.

  Metadata properties include:
  - :category - completion category (:command, :buffer, :file, etc.)
  - :annotation-function - function to annotate candidates
  - :group-function - function to group candidates

  Usage: (completion-metadata-get :category)
  Returns: Property value or nil"
  [prop]
  (let [db @rfdb/app-db
        metadata (minibuffer/get-completion-metadata db)]
    (get metadata prop)))

(defn completing-read
  "Read a string from minibuffer with completion.

  PROMPT is the prompt string.
  COLLECTION is the completion table (list of candidates).
  Optional PREDICATE filters candidates.
  Optional REQUIRE-MATCH if non-nil, only allow valid completions.
  Optional INITIAL-INPUT is initial text in minibuffer.

  Note: This is a synchronous stub. Full implementation needs async support.

  Usage: (completing-read \"Command: \" [\"forward\" \"backward\"])
  Returns: Selected string"
  ([prompt collection]
   (completing-read prompt collection nil nil nil))
  ([prompt collection predicate]
   (completing-read prompt collection predicate nil nil))
  ([prompt collection predicate require-match]
   (completing-read prompt collection predicate require-match nil))
  ([prompt collection predicate require-match initial-input]
   (let [candidates (if (sequential? collection) collection [])]
     (rf/dispatch-sync [:minibuffer/activate
                        {:prompt prompt
                         :completions candidates
                         :initial-input (or initial-input "")
                         :require-match require-match
                         :on-confirm [:minibuffer/deactivate]
                         :on-cancel [:minibuffer/deactivate]}])
     ;; Return empty string - full impl needs callback/promise
     "")))

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

(defn define-key-for-mode
  "Bind KEY-SEQUENCE to COMMAND in MODE's keymap.

  Issue #139: Mode-specific keybindings for packages like Dired.

  Usage: (define-key-for-mode 'dired-mode \"n\" 'dired-next-line)
  Returns: nil (side effect only)"
  [mode key-sequence command]
  (rf/dispatch-sync [:keymap/set-mode-key mode key-sequence command])
  nil)

(defn key-binding
  "Look up KEY-SEQUENCE and return bound command.

  Usage: (key-binding \"C-c C-c\")
  Returns: Command symbol or nil"
  [key-sequence]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)]
    (or (get-in db [:buffers buffer-id :local-keymap key-sequence])
        (get-in db [:keymaps :global :bindings key-sequence]))))

(defn simulate-key
  "Simulate a key press as if the user typed it.
   Used for keyboard macro playback.

  Usage: (simulate-key \"C-x C-s\")
  Returns: nil"
  [key-sequence]
  (rf/dispatch-sync [:handle-key-sequence key-sequence])
  nil)

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

(defn find-file
  "Visit file at PATH.

  Issue #139: Used by dired-find-file to open files.
  If the file is in a granted directory (FS Access API), opens it directly.
  Otherwise, may prompt for file access.

  Usage: (find-file \"/home/user/file.txt\")
  Returns: nil"
  [path]
  ;; Dispatch the find-file event which handles FS Access API integration
  (rf/dispatch-sync [:find-file/from-path path])
  nil)

;; =============================================================================
;; File System Write Operations (Issue #139)
;; =============================================================================

(defn fs-create-directory
  "Create a new directory at PATH.

  Requires the parent directory to be in a granted directory
  (via grant-directory-access command).

  ON-SUCCESS and ON-ERROR are optional event vectors to dispatch.

  Usage: (fs-create-directory \"/projects/new-dir\")
         (fs-create-directory \"/projects/new-dir\"
                              [:dired/refresh-after-op]
                              [:dired/handle-error])
  Returns: nil (async operation)"
  ([path]
   (fs-create-directory path nil nil))
  ([path on-success]
   (fs-create-directory path on-success nil))
  ([path on-success on-error]
   (rf/dispatch [:fs-access/create-directory-at-path path on-success on-error])
   nil))

(defn fs-delete
  "Delete a file or directory at PATH.

  If RECURSIVE is true, deletes directory contents recursively.
  Requires the path to be in a granted directory.

  ON-SUCCESS and ON-ERROR are optional event vectors to dispatch.

  Usage: (fs-delete \"/projects/file.txt\")
         (fs-delete \"/projects/old-dir\" true)
  Returns: nil (async operation)"
  ([path]
   (fs-delete path false nil nil))
  ([path recursive?]
   (fs-delete path recursive? nil nil))
  ([path recursive? on-success]
   (fs-delete path recursive? on-success nil))
  ([path recursive? on-success on-error]
   (rf/dispatch [:fs-access/delete-at-path path recursive? on-success on-error])
   nil))

(defn fs-copy-file
  "Copy a file from SOURCE-PATH to DEST-PATH.

  Both paths must be within the same granted directory.

  ON-SUCCESS and ON-ERROR are optional event vectors to dispatch.

  Usage: (fs-copy-file \"/projects/a.txt\" \"/projects/b.txt\")
  Returns: nil (async operation)"
  ([source-path dest-path]
   (fs-copy-file source-path dest-path nil nil))
  ([source-path dest-path on-success]
   (fs-copy-file source-path dest-path on-success nil))
  ([source-path dest-path on-success on-error]
   (rf/dispatch [:fs-access/copy-file-path source-path dest-path on-success on-error])
   nil))

(defn fs-rename
  "Rename/move a file or directory from SOURCE-PATH to DEST-PATH.

  Both paths must be within the same granted directory.
  Note: Directory rename is not yet fully supported.

  ON-SUCCESS and ON-ERROR are optional event vectors to dispatch.

  Usage: (fs-rename \"/projects/old.txt\" \"/projects/new.txt\")
  Returns: nil (async operation)"
  ([source-path dest-path]
   (fs-rename source-path dest-path nil nil))
  ([source-path dest-path on-success]
   (fs-rename source-path dest-path on-success nil))
  ([source-path dest-path on-success on-error]
   (rf/dispatch [:fs-access/move-path source-path dest-path on-success on-error])
   nil))

(defn fs-path-accessible?
  "Check if PATH is within a granted directory.

  Usage: (fs-path-accessible? \"/projects/file.txt\")
  Returns: Boolean"
  [path]
  (let [granted (get-in @rfdb/app-db [:fs-access :granted-directories] {})]
    (some (fn [[grant-path _]]
            (str/starts-with? path grant-path))
          granted)))

(defn insert-directory
  "Insert directory listing for DIR at point.

  This is the core primitive for Dired buffer generation.
  Formats entries in ls -l style.

  Uses File System Access API directory cache if available,
  falls back to mock filesystem otherwise.

  Optional MARKS is a map of {filename -> mark-char} where mark-char
  is '*' for marked or 'D' for flagged for deletion.

  Usage: (insert-directory \"/home/user\")
         (insert-directory \"/home/user\" {\"file.txt\" \"*\"})
  Returns: nil (side effect: inserts text)"
  ([directory] (insert-directory directory {}))
  ([directory marks]
   (let [;; Normalize directory path for cache lookup (remove trailing slash)
         cache-key (if (and (seq directory) (str/ends-with? directory "/"))
                     (subs directory 0 (dec (count directory)))
                     directory)
         ;; Try to get entries from FS Access cache first
         fs-cache (get-in @rfdb/app-db [:fs-access :directory-cache] {})
         fs-entries (get fs-cache cache-key)
         ;; Convert FS Access entries to dired format, or use mock filesystem
         entries (if fs-entries
                   ;; Convert FS Access format {:name "x" :kind "file"|"directory"}
                   (map (fn [{:keys [name kind]}]
                          {:name name
                           :type (if (= kind "directory") :directory :file)
                           :size "-"
                           :modified ""})
                        fs-entries)
                   ;; Fall back to mock filesystem
                   (get mock-filesystem directory []))
         header (str "  " directory ":\n")
         pad-left (fn [s width]
                    (let [s (str s)
                          pad (- width (count s))]
                      (if (pos? pad)
                        (str (apply str (repeat pad " ")) s)
                        s)))
         format-entry (fn [{:keys [name type size modified]}]
                        (let [mark-char (get marks name " ")
                              type-char (if (= type :directory) "d" "-")
                              perms "rwxr-xr-x"]
                          (str mark-char " " type-char perms "  "
                               (pad-left size 6) "  "
                               modified "  " name)))
         lines (map format-entry entries)
         content (str header (str/join "\n" lines) "\n")]
     (insert content))))

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

;; =============================================================================
;; Indirect Buffers (Issue #100)
;; =============================================================================

(defn buffer-base-buffer
  "Return the base buffer of indirect buffer BUFFER.
  If BUFFER is not indirect, return nil.

  Usage: (buffer-base-buffer buffer-id)
         (buffer-base-buffer \"buffer-name\")
  Returns: Base buffer ID or nil"
  ([] (buffer-base-buffer (current-buffer)))
  ([buffer-or-name]
   (let [buffer-id (if (string? buffer-or-name)
                     (get-buffer buffer-or-name)
                     buffer-or-name)
         buffer (get-in @rfdb/app-db [:buffers buffer-id])]
     (:base-buffer-id buffer))))

(defn make-indirect-buffer
  "Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.

  An indirect buffer shares the text of its base buffer, but has its own
  values of point, mark, narrowing, major mode, minor modes, and local
  variables.

  BASE-BUFFER may be a buffer or a buffer name.
  If CLONE is non-nil, copy the current values of variables from base buffer.

  Usage: (make-indirect-buffer \"*scratch*\" \"scratch-indirect\")
         (make-indirect-buffer base-id \"indirect\" t)
  Returns: New buffer ID"
  ([base-buffer name] (make-indirect-buffer base-buffer name nil))
  ([base-buffer name clone]
   (let [base-id (if (string? base-buffer)
                   (get-buffer base-buffer)
                   base-buffer)
         ;; No double indirection - if base is indirect, use base's base
         actual-base-id (or (buffer-base-buffer base-id) base-id)
         base-buf (get-in @rfdb/app-db [:buffers actual-base-id])]
     (when (and actual-base-id base-buf)
       (let [buffer-id (db/next-buffer-id (:buffers @rfdb/app-db))
             ;; Share the WASM instance with the base buffer
             wasm-instance (:wasm-instance base-buf)
             ;; Create indirect buffer with shared text but independent state
             indirect-buffer (-> (db/create-buffer buffer-id name wasm-instance)
                                 (assoc :base-buffer-id actual-base-id)
                                 ;; Share undo list with base (per Emacs semantics)
                                 (assoc :undo-stack (:undo-stack base-buf))
                                 ;; Clone local vars if requested
                                 (cond-> clone
                                   (assoc :local-vars (:local-vars base-buf)
                                          :local-vars-set (:local-vars-set base-buf)
                                          :major-mode (:major-mode base-buf)
                                          :minor-modes (:minor-modes base-buf))))]
         (swap! rfdb/app-db assoc-in [:buffers buffer-id] indirect-buffer)
         buffer-id)))))

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

;; =============================================================================
;; Overlays (for highlighting, annotations)
;; =============================================================================

(defn make-overlay
  "Create an overlay from START to END in current buffer.

  Usage: (make-overlay start end)
         (make-overlay start end &optional buffer front-advance rear-advance)
  Returns: Overlay ID (integer)"
  ([start end]
   (make-overlay start end nil nil nil))
  ([start end _buffer]
   (make-overlay start end nil nil nil))
  ([start end _buffer _front-advance]
   (make-overlay start end nil nil nil))
  ([start end _buffer _front-advance _rear-advance]
   (let [buffer-id (current-buffer)
         next-id (get-in @rfdb/app-db [:buffers buffer-id :next-overlay-id] 1)
         overlay {:id next-id
                  :start (min start end)
                  :end (max start end)
                  :face nil
                  :priority 0
                  :evaporate false}]
     (swap! rfdb/app-db
            (fn [db]
              (-> db
                  (assoc-in [:buffers buffer-id :overlays next-id] overlay)
                  (update-in [:buffers buffer-id :next-overlay-id] (fnil inc 1)))))
     next-id)))

(defn delete-overlay
  "Delete OVERLAY.

  Usage: (delete-overlay overlay-id)
  Returns: nil"
  [overlay-id]
  (let [buffer-id (current-buffer)]
    (swap! rfdb/app-db update-in [:buffers buffer-id :overlays] dissoc overlay-id)
    nil))

(defn overlay-put
  "Set property PROP to VALUE on OVERLAY.

  Usage: (overlay-put overlay-id 'face 'hi-yellow)
  Returns: VALUE"
  [overlay-id prop value]
  (let [buffer-id (current-buffer)
        prop-key (if (symbol? prop) (keyword (name prop)) prop)]
    (swap! rfdb/app-db assoc-in [:buffers buffer-id :overlays overlay-id prop-key] value)
    value))

(defn overlay-get
  "Get property PROP from OVERLAY.

  Usage: (overlay-get overlay-id 'face)
  Returns: Property value or nil"
  [overlay-id prop]
  (let [buffer-id (current-buffer)
        prop-key (if (symbol? prop) (keyword (name prop)) prop)]
    (get-in @rfdb/app-db [:buffers buffer-id :overlays overlay-id prop-key])))

(defn overlays-in
  "Return list of overlays that overlap region START to END.

  Usage: (overlays-in start end)
  Returns: Vector of overlay IDs"
  [start end]
  (let [buffer-id (current-buffer)
        overlays (get-in @rfdb/app-db [:buffers buffer-id :overlays] {})
        actual-start (min start end)
        actual-end (max start end)]
    (vec (keep (fn [[id ov]]
                 (when (and (< (:start ov) actual-end)
                            (> (:end ov) actual-start))
                   id))
               overlays))))

(defn remove-overlays
  "Remove all overlays in region from START to END.

  Usage: (remove-overlays start end)
  Returns: nil"
  ([start end]
   (remove-overlays start end nil nil))
  ([start end _name _value]
   (let [buffer-id (current-buffer)
         to-remove (overlays-in start end)]
     (doseq [id to-remove]
       (delete-overlay id))
     nil)))

(defn overlay-start
  "Return start position of OVERLAY.

  Usage: (overlay-start overlay-id)
  Returns: Integer"
  [overlay-id]
  (let [buffer-id (current-buffer)]
    (get-in @rfdb/app-db [:buffers buffer-id :overlays overlay-id :start])))

(defn overlay-end
  "Return end position of OVERLAY.

  Usage: (overlay-end overlay-id)
  Returns: Integer"
  [overlay-id]
  (let [buffer-id (current-buffer)]
    (get-in @rfdb/app-db [:buffers buffer-id :overlays overlay-id :end])))

;; =============================================================================
;; Additional primitives for packages
;; =============================================================================

(defn goto-line
  "Move cursor to LINE (1-indexed).

  Usage: (goto-line 10)
  Returns: nil"
  [line-num]
  (let [text (buffer-string)
        lines (clojure.string/split-lines text)
        target-line (max 1 (min line-num (count lines)))
        pos (reduce + (map #(inc (count %)) (take (dec target-line) lines)))]
    (goto-char pos)
    nil))

(defn symbol-at-point
  "Return the symbol at point as a string.

  Usage: (symbol-at-point)
  Returns: String or nil"
  []
  (let [text (buffer-string)
        pos (point)
        text-len (count text)]
    (when (and text (< pos text-len))
      (let [;; Find start of symbol
            start (loop [p pos]
                    (if (and (> p 0)
                             (let [ch (nth text (dec p))]
                               (re-matches #"[\w_-]" (str ch))))
                      (recur (dec p))
                      p))
            ;; Find end of symbol
            end (loop [p pos]
                  (if (and (< p text-len)
                           (let [ch (nth text p)]
                             (re-matches #"[\w_-]" (str ch))))
                    (recur (inc p))
                    p))
            sym (subs text start end)]
        (when (seq sym) sym)))))

(defn buffer-text-of
  "Return the text content of BUFFER-ID.

  Usage: (buffer-text-of buffer-id)
  Returns: String or nil"
  [buffer-id]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])
        wasm (:wasm-instance buffer)]
    (when wasm
      (try (.getText ^js wasm) (catch :default _ nil)))))

(defn split-window-below
  "Split current window horizontally (new window below).

  NOTE: Uses direct swap! to update app-db because dispatch-sync doesn't work
  when called from inside an event handler.

  Usage: (split-window-below)
  Returns: nil"
  []
  (let [db @rfdb/app-db
        active-window-id (:active-window-id db)
        window-tree (:window-tree db)
        active-window (db/find-window-in-tree window-tree active-window-id)
        new-window-id (db/next-window-id db)
        new-window (db/create-leaf-window new-window-id (:buffer-id active-window))
        split-id (inc new-window-id)
        split-node (db/create-split-window :hsplit split-id active-window new-window)
        replace-fn (fn replace-fn [tree]
                     (if (= (:id tree) active-window-id)
                       split-node
                       (cond
                         (= (:type tree) :leaf) tree
                         (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
                         (assoc tree
                                :first (replace-fn (:first tree))
                                :second (replace-fn (:second tree)))
                         :else tree)))
        new-tree (replace-fn window-tree)]
    (swap! rfdb/app-db
           (fn [db]
             (-> db
                 (assoc :window-tree new-tree)
                 (assoc :active-window-id new-window-id)
                 (assoc :cursor-owner new-window-id)
                 (assoc :next-window-id (+ new-window-id 2))))))
  (run-hooks 'window-configuration-change-hook)
  nil)

(defn switch-to-buffer-other-window
  "Display BUFFER in another window.

  Usage: (switch-to-buffer-other-window buffer-id)
  Returns: nil"
  [buffer-id]
  (rf/dispatch-sync [:window/split :vertical])
  (rf/dispatch-sync [:switch-buffer buffer-id])
  nil)

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
   'text-properties-at text-properties-at
   'add-text-properties add-text-properties
   'next-property-change next-property-change
   'previous-property-change previous-property-change
   'next-single-property-change next-single-property-change
   'previous-single-property-change previous-single-property-change
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
   'delete-char delete-char
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
   'buffer-list-ids buffer-list-ids
   'buffer-size-of buffer-size-of
   'buffer-mode-of buffer-mode-of
   'buffer-modified-p-of buffer-modified-p-of
   'buffer-read-only-p-of buffer-read-only-p-of
   'buffer-file-name-of buffer-file-name-of
   'buffer-info buffer-info
   'all-buffer-info all-buffer-info
   'buffer-live-p buffer-live-p
   'generate-new-buffer-name generate-new-buffer-name
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
   'split-window-vertically split-window-vertically
   'delete-window delete-window
   'delete-other-windows delete-other-windows
   'other-window other-window
   'selected-window selected-window
   'window-buffer window-buffer
   'split-window-below split-window-below
   'switch-to-buffer-other-window switch-to-buffer-other-window
   ;; Overlays
   'make-overlay make-overlay
   'delete-overlay delete-overlay
   'overlay-put overlay-put
   'overlay-get overlay-get
   'overlays-in overlays-in
   'remove-overlays remove-overlays
   'overlay-start overlay-start
   'overlay-end overlay-end
   ;; Additional primitives
   'goto-line goto-line
   'symbol-at-point symbol-at-point
   'buffer-text-of buffer-text-of
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
   'read-from-minibuffer read-from-minibuffer
   ;; Completion (Issue #108)
   'all-completions all-completions
   'try-completion try-completion
   'test-completion test-completion
   'minibuffer-completion-table minibuffer-completion-table
   'completion-metadata-get completion-metadata-get
   'completing-read completing-read
   ;; Keymaps
   'global-set-key global-set-key
   'local-set-key local-set-key
   'define-key-for-mode define-key-for-mode
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
   'find-file find-file
   'insert-directory insert-directory
   ;; FS Access write operations (Issue #139)
   'fs-create-directory fs-create-directory
   'fs-delete fs-delete
   'fs-copy-file fs-copy-file
   'fs-rename fs-rename
   'fs-path-accessible? fs-path-accessible?
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
   ;; Dynamic variables - initialized to nil, can be rebound via setq
   '*prefix-arg* nil
   '*last-command* nil
   '*this-command* nil
   ;; Hooks (#106)
   'add-hook add-hook
   'remove-hook remove-hook
   'run-hooks run-hooks
   'run-hook-with-args run-hook-with-args
   'run-hook-with-args-until-success run-hook-with-args-until-success
   'run-hook-with-args-until-failure run-hook-with-args-until-failure
   ;; inhibit-read-only
   'inhibit-read-only inhibit-read-only
   'with-inhibit-read-only with-inhibit-read-only
   ;; Additional buffer functions
   'get-buffer-create get-buffer-create
   'kill-buffer kill-buffer
   ;; Indirect buffers (Issue #100)
   'buffer-base-buffer buffer-base-buffer
   'make-indirect-buffer make-indirect-buffer
   'looking-at looking-at
   'forward-word forward-word
   'error lisp-error})
