(ns lexicon.core.ring
  "Ring (circular buffer) data structure - Emacs ring.el equivalent.

  A ring is a fixed-size circular buffer where new items push out old ones
  when the ring is full. Used for history rings (command history, kill ring,
  mark ring, input history, etc.).

  Implementation:
  A ring is represented as a map:
    {:size     N          ; Maximum capacity
     :elements [...]      ; Vector with newest at front (index 0)}

  This mirrors Emacs ring.el semantics:
  - ring-insert adds at the head (most recent position)
  - ring-ref with index 0 returns the most recent item
  - When full, oldest items are silently dropped

  Pure functions - no side effects or atoms. Packages wrap these
  in atoms for stateful usage.

  Based on Emacs lisp/emacs-lisp/ring.el")

;; =============================================================================
;; Ring Creation
;; =============================================================================

(defn make-ring
  "Create an empty ring with the given maximum size.
   Size must be a positive integer."
  [size]
  {:pre [(pos-int? size)]}
  {:size size
   :elements []})

;; =============================================================================
;; Ring Accessors
;; =============================================================================

(defn ring-size
  "Return the maximum capacity of RING."
  [ring]
  (:size ring))

(defn ring-length
  "Return the current number of elements in RING."
  [ring]
  (count (:elements ring)))

(defn ring-empty-p
  "Return true if RING contains no elements."
  [ring]
  (empty? (:elements ring)))

(defn ring-elements
  "Return all elements of RING as a vector.
   Most recent element is first (index 0)."
  [ring]
  (:elements ring))

;; =============================================================================
;; Ring Navigation
;; =============================================================================

(defn ring-ref
  "Return the element at INDEX in RING.
   Index 0 is the most recently inserted element.
   Returns nil if INDEX is out of bounds."
  [ring index]
  (get (:elements ring) index))

(defn ring-nth
  "Same as ring-ref. Return element at INDEX."
  [ring index]
  (ring-ref ring index))

;; =============================================================================
;; Ring Modification
;; =============================================================================

(defn ring-insert
  "Insert ITEM at the front of RING (most recent position).
   If RING is full, the oldest element is dropped.
   Returns the updated ring."
  [ring item]
  (let [max-size (:size ring)
        elements (:elements ring)
        new-elements (vec (take max-size (cons item elements)))]
    (assoc ring :elements new-elements)))

(defn ring-insert-at-beginning
  "Insert ITEM at the end of RING (oldest position).
   If RING is full, the newest element is dropped.
   Returns the updated ring."
  [ring item]
  (let [max-size (:size ring)
        elements (:elements ring)
        new-elements (if (>= (count elements) max-size)
                       (vec (concat (rest elements) [item]))
                       (conj elements item))]
    (assoc ring :elements new-elements)))

(defn ring-remove
  "Remove and return the element at INDEX from RING.
   Returns [removed-element updated-ring] or [nil ring] if index invalid."
  [ring index]
  (let [elements (:elements ring)]
    (if (and (>= index 0) (< index (count elements)))
      (let [removed (nth elements index)
            new-elements (vec (concat (take index elements)
                                      (drop (inc index) elements)))]
        [removed (assoc ring :elements new-elements)])
      [nil ring])))

(defn ring-remove-head
  "Remove and return the most recent element (head) from RING.
   Returns [removed-element updated-ring] or [nil ring] if empty."
  [ring]
  (ring-remove ring 0))

;; =============================================================================
;; Ring Search
;; =============================================================================

(defn ring-member
  "Return the index of ITEM in RING, or nil if not found.
   Uses = for comparison."
  [ring item]
  (let [elements (:elements ring)]
    (loop [i 0]
      (cond
        (>= i (count elements)) nil
        (= (nth elements i) item) i
        :else (recur (inc i))))))

(defn ring-contains?
  "Return true if RING contains ITEM."
  [ring item]
  (some? (ring-member ring item)))

;; =============================================================================
;; Ring Conversion
;; =============================================================================

(defn ring->list
  "Convert RING to a list (vector) of elements.
   Most recent first."
  [ring]
  (ring-elements ring))

(defn list->ring
  "Create a ring from a list of elements with the given max size.
   First element of list becomes most recent."
  [size lst]
  {:size size
   :elements (vec (take size lst))})

;; =============================================================================
;; Duplicate Prevention (Common Pattern)
;; =============================================================================

(defn ring-insert-no-dup
  "Insert ITEM at the front of RING, but only if it differs from the current head.
   Prevents consecutive duplicates in history rings.
   Returns the updated ring."
  [ring item]
  (let [elements (:elements ring)]
    (if (and (seq elements) (= (first elements) item))
      ring  ; Don't add duplicate
      (ring-insert ring item))))
