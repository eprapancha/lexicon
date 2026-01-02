# Lexicon Marker System Specification

**Version:** 1.0.0
**Status:** Design Document
**Last Updated:** 2026-01-02

## Table of Contents

1. [Overview](#overview)
2. [Marker Fundamentals](#marker-fundamentals)
3. [Marker Types](#marker-types)
4. [Marker Adjustment](#marker-adjustment)
5. [Overlays](#overlays)
6. [Special Markers](#special-markers)
7. [Marker API Reference](#marker-api-reference)
8. [Implementation Guidelines](#implementation-guidelines)
9. [Performance Considerations](#performance-considerations)

---

## Overview

**Markers** are persistent positions in a buffer that **automatically adjust** when text is inserted or deleted. Unlike raw integer positions, markers track their logical location through buffer modifications.

### Why Markers Matter

Without markers:
```clojure
;; Buffer: "hello world"
(def pos 6)  ;; Points to "w" in "world"

;; Insert " there" at position 5
;; Buffer: "hello there world"

;; pos is still 6, now points to "t" in "there" ❌
;; We wanted to track "w" in "world", now at position 12
```

With markers:
```clojure
;; Buffer: "hello world"
(def mark (create-marker 6))  ;; Points to "w" in "world"

;; Insert " there" at position 5
;; Buffer: "hello there world"

;; mark automatically adjusted to 12 ✅
```

### Design Principles

1. **Automatic adjustment**: Markers move with text changes
2. **Efficient**: Markers don't slow down editing
3. **Composable**: Overlays build on markers
4. **Undo-aware**: Markers restore correctly on undo
5. **Engine-level**: Core supports markers, not just CLJS layer

### Comparison to Emacs

| Feature | Emacs | Lexicon |
|---------|-------|---------|
| Basic markers | Yes ✅ | Yes ✅ |
| Insertion type | `:before`, `:after` | `:before`, `:after` ✅ |
| Buffer association | Marker → Buffer | Marker → Buffer ✅ |
| Overlays | Yes ✅ | Yes ✅ |
| Text properties | Yes | Partial (future) |
| Marker GC | Manual | Automatic (weak refs) |

---

## Marker Fundamentals

### Marker Structure

A marker is a map containing:

```clojure
{:id           "marker-123"      ;; Unique identifier
 :buffer-id    "buffer-456"      ;; Which buffer
 :position     42                ;; Current position (0-indexed)
 :insertion-type :after          ;; :before or :after
 :created-at   1704189600000     ;; Timestamp
 :metadata     {...}}            ;; Optional metadata
```

### Fields

| Field | Type | Description |
|-------|------|-------------|
| `:id` | string | Unique identifier (UUID) |
| `:buffer-id` | string | Buffer this marker belongs to |
| `:position` | int | Current position in buffer (0-indexed) |
| `:insertion-type` | keyword | How to move on insertion at position (`:before` or `:after`) |
| `:created-at` | timestamp | When marker was created |
| `:metadata` | map | User-defined data (overlay properties, etc.) |

### Position Semantics

Marker positions are **0-indexed**:
- Position 0: Before first character
- Position 1: After first character, before second
- Position N: After Nth character (end of buffer if N = length)

```
Buffer: "hello"
Positions: 0h1e2l3l4o5
           ^           ^
         Pos 0      Pos 5 (end)
```

### Buffer Association

Markers are **permanently associated** with a buffer:
- Created in specific buffer
- Invalid if buffer is killed
- Cannot be moved to different buffer

```clojure
(def marker (create-marker buffer-id 10))

;; marker is forever in buffer-id
;; If buffer-id is killed, marker becomes invalid
```

---

## Marker Types

### 1. Point (Cursor)

**Point** is a special marker representing the **cursor position**.

```clojure
;; Get point
(point buffer-id)  ;; => 42

;; Set point
(set-point buffer-id 100)

;; Point is just a special marker
(get-marker buffer-id :point)
;; => {:id :point :buffer-id "..." :position 42 :insertion-type :after}
```

**Properties**:
- One per buffer
- Always visible (rendered as cursor)
- Insertion type: `:after` (cursor follows typing)
- Clamped to buffer bounds: `[0, (buffer-size)]`

---

### 2. Mark (Region Anchor)

**Mark** is a special marker representing the **region start** (other end of selection from point).

```clojure
;; Set mark at current point
(set-mark buffer-id (point buffer-id))

;; Get mark
(mark buffer-id)  ;; => 10

;; Region is between mark and point
(region buffer-id)
;; => {:start 10 :end 42}  (if mark < point)
;; or {:start 42 :end 10}  (if point < mark)
```

**Properties**:
- One per buffer
- May be inactive (no active region)
- Insertion type: `:before` (mark stays at anchor)
- Used for region selection

---

### 3. Named Markers

User-created markers for specific purposes:

```clojure
(def bookmark (create-marker buffer-id 100
                :id :my-bookmark
                :insertion-type :after))

;; Later, jump to bookmark
(goto-char buffer-id (marker-position bookmark))
```

**Use Cases**:
- Bookmarks
- Jump history
- Completion boundaries
- Syntax highlighting regions

---

### 4. Anonymous Markers

Temporary markers without explicit IDs:

```clojure
(def temp (create-marker buffer-id 50))
;; Auto-generated ID: "marker-abc123"

;; Use and discard
(delete-marker temp)
```

**Use Cases**:
- Temporary positions during algorithms
- Undo/redo tracking
- Transient UI elements

---

## Marker Adjustment

### Insertion Type

When text is inserted **exactly at** marker position, insertion type determines behavior:

| Type | Behavior | Use Case |
|------|----------|----------|
| `:before` | Marker stays before insertion | Region start, mark |
| `:after` | Marker moves after insertion | Cursor, bookmark |

**Example**:
```clojure
;; Buffer: "hello world"
(def m1 (create-marker buffer-id 5 :insertion-type :before))
(def m2 (create-marker buffer-id 5 :insertion-type :after))

;; Both at position 5 (space between "hello" and "world")

;; Insert "XXX" at position 5
;; Buffer: "helloXXX world"

(marker-position m1)  ;; => 5  (:before stays put)
(marker-position m2)  ;; => 8  (:after moves past "XXX")
```

---

### Adjustment Rules

#### Insertion Before Marker

Text inserted **before** marker position → marker shifts right:

```clojure
;; Buffer: "hello world"
(def m (create-marker buffer-id 6))  ;; At "w"

;; Insert "XXX" at position 3
;; Buffer: "helXXXlo world"

(marker-position m)  ;; => 9 (shifted right by 3)
```

#### Insertion After Marker

Text inserted **after** marker position → marker stays put:

```clojure
;; Buffer: "hello world"
(def m (create-marker buffer-id 5))  ;; At space

;; Insert "XXX" at position 8
;; Buffer: "hello woXXXrld"

(marker-position m)  ;; => 5 (unchanged)
```

#### Deletion Before Marker

Text deleted **before** marker → marker shifts left:

```clojure
;; Buffer: "hello world"
(def m (create-marker buffer-id 10))  ;; At "d"

;; Delete "world" (positions 6-11)
;; Buffer: "hello "

(marker-position m)  ;; => 6 (clamped to buffer end)
```

#### Deletion Across Marker

Text deleted **across** marker → marker collapses to deletion point:

```clojure
;; Buffer: "hello world"
(def m (create-marker buffer-id 8))  ;; At "r"

;; Delete "lo wor" (positions 3-9)
;; Buffer: "helld"

(marker-position m)  ;; => 3 (collapsed to deletion start)
```

---

### Adjustment Algorithm

Pseudocode for marker adjustment:

```clojure
(defn adjust-marker-position [marker change]
  (let [{:keys [position insertion-type]} marker
        {:keys [type pos length]} change]
    (case type
      :insert
      (cond
        (< pos position)        (+ position length)   ;; Before: shift right
        (> pos position)        position              ;; After: unchanged
        (= pos position)
        (case insertion-type
          :before position                            ;; Stay at pos
          :after  (+ position length)))               ;; Move past insertion

      :delete
      (let [del-end (+ pos length)]
        (cond
          (< del-end position)   (- position length)  ;; Before: shift left
          (< pos position)       pos                  ;; Across: collapse
          :else                  position)))))        ;; After: unchanged
```

---

## Overlays

**Overlays** are regions in a buffer with associated properties (e.g., syntax highlighting, error underlining, completion popups).

### Overlay Structure

An overlay is a pair of markers defining a region:

```clojure
{:id         "overlay-123"
 :buffer-id  "buffer-456"
 :start      marker-1        ;; Start marker
 :end        marker-2        ;; End marker
 :properties {:face :comment
              :priority 10
              :evaporate? true}}
```

### Overlay Properties

Common overlay properties:

| Property | Type | Description |
|----------|------|-------------|
| `:face` | keyword | Face to apply (`:comment`, `:string`, etc.) |
| `:priority` | int | Overlay priority (higher = wins conflicts) |
| `:evaporate?` | bool | Delete overlay if region becomes empty |
| `:read-only` | bool | Prevent edits in region |
| `:invisible` | bool | Hide text in region |
| `:before-string` | string | Display before region |
| `:after-string` | string | Display after region |
| `:modification-hooks` | vector | Functions to call on edit |

### Overlay Lifecycle

1. **Creation**: Define start/end markers with properties
2. **Adjustment**: Markers auto-adjust as buffer changes
3. **Evaporation**: Optional auto-delete when region empty
4. **Deletion**: Explicit removal or buffer kill

**Example**:
```clojure
;; Highlight "hello" as comment
(def overlay (create-overlay buffer-id 0 5
               :face :comment
               :priority 10
               :evaporate? true))

;; Delete "hello"
;; Overlay auto-deleted because :evaporate? = true
```

---

### Overlay Use Cases

#### 1. Syntax Highlighting

```clojure
;; Highlight comment in buffer
(create-overlay buffer-id 10 30
  :face :comment
  :priority 10)
```

#### 2. Error Underlining

```clojure
;; Red underline for error
(create-overlay buffer-id 50 60
  :face :error
  :priority 50  ;; High priority
  :modification-hooks [on-error-region-edit])
```

#### 3. Completion Popup

```clojure
;; Show completion candidates at point
(create-overlay buffer-id (point buffer-id) (point buffer-id)
  :after-string (render-completion-popup candidates)
  :priority 100  ;; Very high
  :evaporate? true)
```

#### 4. Read-only Region

```clojure
;; Make region read-only
(create-overlay buffer-id 0 50
  :read-only true
  :face :read-only-face)
```

---

## Special Markers

### Point (Cursor)

```clojure
;; Get current point
(point)             ;; In current buffer
(point buffer-id)   ;; In specific buffer

;; Set point
(goto-char 100)           ;; In current buffer
(set-point buffer-id 100) ;; In specific buffer

;; Save and restore point
(save-excursion
  (goto-char 0)
  (insert! "header\n"))
;; Point restored to original position after insert
```

**Properties**:
- Insertion type: `:after`
- Always clamped to `[0, buffer-size]`
- Triggers `:buffer-switch-hook` when changed across buffers

---

### Mark (Region Anchor)

```clojure
;; Set mark at current point
(set-mark (point))

;; Push mark (save current mark on mark ring)
(push-mark)

;; Pop mark (restore previous mark)
(pop-mark)

;; Get region
(region)  ;; => {:start 10 :end 50 :active? true}

;; Exchange point and mark
(exchange-point-and-mark)
;; Point moves to mark, mark moves to old point
```

**Properties**:
- Insertion type: `:before`
- May be inactive (`:active?` = false)
- Used for region-based commands

---

### Mark Ring

Emacs-style **mark ring** for navigating previous locations:

```clojure
;; Push mark (save current position)
(push-mark)

;; Jump elsewhere
(goto-char 1000)

;; Return to previous mark
(pop-to-mark)  ;; Jump to mark and pop from ring
```

**Ring Structure**:
```clojure
{:buffer-id "buffer-123"
 :mark-ring ["marker-1" "marker-2" "marker-3" ...]  ;; Max 16 marks
 :mark-ring-max 16}
```

---

### Temporary Markers

Used internally during operations:

```clojure
(defn sort-lines [start end]
  (let [start-marker (create-marker (current-buffer) start :insertion-type :before)
        end-marker (create-marker (current-buffer) end :insertion-type :after)]

    ;; Sort lines (buffer changes, markers adjust)
    (sort-lines-between (marker-position start-marker)
                        (marker-position end-marker))

    ;; Cleanup
    (delete-marker start-marker)
    (delete-marker end-marker)))
```

---

## Marker API Reference

### `create-marker`

Creates a new marker at specified position.

**Signature**:
```clojure
(create-marker buffer-id position & options)
```

**Parameters**:
- `buffer-id` (string): Buffer to create marker in
- `position` (int): Initial position (0-indexed)
- `options` (keyword args):
  - `:id` (keyword/string): Explicit ID (default: auto-generated)
  - `:insertion-type` (keyword): `:before` or `:after` (default: `:after`)
  - `:metadata` (map): User data

**Returns**: Marker ID

**Example**:
```clojure
(create-marker "buffer-123" 50
  :id :my-bookmark
  :insertion-type :before
  :metadata {:type :user-bookmark :name "Chapter 1"})
```

---

### `delete-marker`

Deletes a marker.

**Signature**:
```clojure
(delete-marker marker-id)
```

**Parameters**:
- `marker-id` (keyword/string): Marker to delete

**Returns**: `true` if deleted, `false` if not found

**Example**:
```clojure
(delete-marker :my-bookmark)
```

---

### `marker-position`

Returns current position of marker.

**Signature**:
```clojure
(marker-position marker-id)
(marker-position marker-id buffer-id)
```

**Parameters**:
- `marker-id` (keyword/string): Marker to query
- `buffer-id` (optional): Buffer (defaults to current)

**Returns**: Position (int), or `nil` if marker doesn't exist

**Example**:
```clojure
(marker-position :point)  ;; => 42
```

---

### `set-marker-position`

Moves marker to new position.

**Signature**:
```clojure
(set-marker-position marker-id new-position)
```

**Parameters**:
- `marker-id` (keyword/string): Marker to move
- `new-position` (int): New position

**Returns**: New position

**Example**:
```clojure
(set-marker-position :my-bookmark 100)
```

---

### `marker-buffer`

Returns buffer ID that marker belongs to.

**Signature**:
```clojure
(marker-buffer marker-id)
```

**Returns**: Buffer ID (string), or `nil` if marker invalid

---

### `create-overlay`

Creates an overlay (region with properties).

**Signature**:
```clojure
(create-overlay buffer-id start end & properties)
```

**Parameters**:
- `buffer-id` (string): Buffer
- `start` (int): Start position
- `end` (int): End position
- `properties` (keyword args): Overlay properties

**Returns**: Overlay ID

**Example**:
```clojure
(create-overlay "buffer-123" 10 50
  :face :comment
  :priority 10
  :evaporate? true
  :read-only false)
```

---

### `delete-overlay`

Deletes an overlay.

**Signature**:
```clojure
(delete-overlay overlay-id)
```

**Returns**: `true` if deleted

---

### `overlays-at`

Returns all overlays at position.

**Signature**:
```clojure
(overlays-at buffer-id position)
```

**Returns**: Vector of overlay IDs (sorted by priority)

**Example**:
```clojure
(overlays-at "buffer-123" 25)
;; => ["overlay-1" "overlay-2"]
```

---

### `overlays-in`

Returns all overlays overlapping a region.

**Signature**:
```clojure
(overlays-in buffer-id start end)
```

**Returns**: Vector of overlay IDs

---

### `overlay-properties`

Returns properties of an overlay.

**Signature**:
```clojure
(overlay-properties overlay-id)
```

**Returns**: Map of properties

**Example**:
```clojure
(overlay-properties "overlay-123")
;; => {:face :comment :priority 10 :evaporate? true}
```

---

### `set-overlay-property`

Sets a property on an overlay.

**Signature**:
```clojure
(set-overlay-property overlay-id property value)
```

**Example**:
```clojure
(set-overlay-property "overlay-123" :face :error)
```

---

## Implementation Guidelines

### For Core Developers

#### 1. Marker Storage

Store markers efficiently in app-db:

```clojure
{:buffers
 {"buffer-123"
  {:markers
   {"marker-1" {:id "marker-1"
                :position 42
                :insertion-type :after
                :metadata {...}}
    "marker-2" {...}
    :point {:id :point :position 10 :insertion-type :after}  ;; Special
    :mark {:id :mark :position 5 :insertion-type :before}}   ;; Special
   :overlays
   {"overlay-1" {:id "overlay-1"
                 :start "marker-3"
                 :end "marker-4"
                 :properties {...}}}}}}
```

#### 2. Marker Adjustment on Edit

Hook into text operations:

```clojure
(defn insert-text [buffer-id pos text]
  ;; Apply text change
  (update-buffer-text buffer-id pos text)

  ;; Adjust all markers
  (adjust-markers buffer-id
    {:type :insert :pos pos :length (count text)}))

(defn adjust-markers [buffer-id change]
  (let [markers (get-markers buffer-id)]
    (doseq [[marker-id marker] markers]
      (let [new-pos (adjust-marker-position marker change)]
        (update-marker buffer-id marker-id :position new-pos)))))
```

#### 3. Overlay Evaporation

Check after each edit:

```clojure
(defn maybe-evaporate-overlays [buffer-id]
  (let [overlays (get-overlays buffer-id)]
    (doseq [[overlay-id overlay] overlays]
      (when (and (get-in overlay [:properties :evaporate?])
                 (= (marker-position (:start overlay))
                    (marker-position (:end overlay))))
        (delete-overlay overlay-id)))))
```

#### 4. Marker GC

Clean up markers from killed buffers:

```clojure
(defn kill-buffer [buffer-id]
  ;; Delete all markers
  (delete-markers buffer-id)

  ;; Delete all overlays
  (delete-overlays buffer-id)

  ;; Remove buffer
  (remove-buffer buffer-id))
```

---

### For Package Developers

#### 1. Using Markers for Bookmarks

```clojure
(defn set-bookmark [name]
  (let [pos (point)]
    (create-marker (current-buffer) pos
      :id (keyword "bookmark" name)
      :insertion-type :after
      :metadata {:type :bookmark :name name})))

(defn jump-to-bookmark [name]
  (let [marker-id (keyword "bookmark" name)
        pos (marker-position marker-id)]
    (if pos
      (goto-char pos)
      (message "Bookmark not found: %s" name))))
```

#### 2. Temporary Markers in Algorithms

```clojure
(defn transpose-words []
  (let [start1 (create-marker (current-buffer) (word-start))
        end1 (create-marker (current-buffer) (word-end))
        _ (forward-word)
        start2 (create-marker (current-buffer) (word-start))
        end2 (create-marker (current-buffer) (word-end))]

    ;; Swap words (markers adjust automatically)
    (swap-regions (marker-position start1) (marker-position end1)
                  (marker-position start2) (marker-position end2))

    ;; Cleanup
    (delete-marker start1)
    (delete-marker end1)
    (delete-marker start2)
    (delete-marker end2)))
```

#### 3. Syntax Highlighting with Overlays

```clojure
(defn fontify-region [start end]
  ;; Delete old overlays
  (delete-overlays-in start end)

  ;; Tokenize and create overlays
  (let [tokens (tokenize-region start end)]
    (doseq [{:keys [type start end]} tokens]
      (create-overlay (current-buffer) start end
        :face (token-type->face type)
        :priority 10
        :evaporate? true))))
```

#### 4. Completion Popup

```clojure
(defn show-completion-popup [candidates]
  (let [pos (point)
        popup-text (render-candidates candidates)]
    (create-overlay (current-buffer) pos pos
      :after-string popup-text
      :priority 100
      :evaporate? true
      :modification-hooks [hide-completion-on-edit])))
```

---

## Performance Considerations

### Marker Count

**Problem**: Large number of markers slows down text operations (each edit adjusts all markers).

**Mitigation**:
1. **Lazy adjustment**: Only adjust markers in visible region
2. **Batching**: Adjust markers once after batch edits
3. **GC**: Delete temporary markers promptly
4. **Indexing**: Use spatial index (interval tree) for overlays

### Overlay Rendering

**Problem**: Many overlays slow down rendering.

**Mitigation**:
1. **Viewport culling**: Only render overlays in visible region
2. **Priority sorting**: Limit to top N overlays per position
3. **Caching**: Cache rendered faces
4. **Throttling**: Limit overlay updates (e.g., 60 FPS max)

### Complexity Analysis

| Operation | Without Index | With Interval Tree |
|-----------|---------------|-------------------|
| Adjust 1 marker | O(1) | O(1) |
| Adjust N markers | O(N) | O(log N + k)* |
| Find overlays at pos | O(M) | O(log M + k) |
| Insert overlay | O(1) | O(log M) |
| Delete overlay | O(1) | O(log M) |

*k = number of overlapping markers/overlays

### Optimization Strategies

1. **Deferred adjustment**: During batch operations, defer marker updates:
   ```clojure
   (with-deferred-marker-adjustment
     (dotimes [i 1000]
       (insert! "x")))  ;; Adjust markers once at end
   ```

2. **Weak references**: Auto-GC unreferenced markers
3. **Dirty tracking**: Only re-render changed overlays
4. **Viewport focus**: Prioritize visible region

---

## Migration from Current Implementation

### Current State (Phase 6)

Lexicon currently has **basic position tracking**:
- Point stored as integer (not marker)
- No general marker support
- No overlays
- No undo tracking of positions

### Migration Path (Phase 7.5)

**Week 1: Marker Infrastructure**
1. Design marker data structure
2. Implement marker creation/deletion
3. Add marker adjustment algorithm
4. Hook into text operations

**Week 2: Special Markers**
1. Convert point to marker
2. Implement mark and mark ring
3. Add `save-excursion` and `save-restriction`
4. Update Core API

**Week 3: Overlays**
1. Implement overlay creation/deletion
2. Add overlay property system
3. Integrate with face rendering
4. Add evaporation logic

**Week 4: Integration & Testing**
1. Update undo system to track marker deltas
2. Add marker-based tests
3. Performance benchmarks
4. Convert existing code to use markers

---

## Future Enhancements

### 1. Text Properties (Phase 8+)

Character-level properties (alternative to overlays):
```clojure
(put-text-property start end :face :comment)
```

### 2. Marker Persistence

Save markers across sessions:
```clojure
(save-markers-to-file "bookmarks.edn")
(restore-markers-from-file "bookmarks.edn")
```

### 3. Overlay Visualizer

Debug tool to show all overlays:
```
[0-5]    :face :comment     priority 10
  [3-8]  :face :string      priority 20 ← higher priority
[10-15]  :face :keyword     priority 10
```

### 4. Multi-range Markers

Markers that span multiple regions:
```clojure
(create-multi-marker buffer-id [[0 5] [10 15] [20 25]]
  :face :search-match)
```

---

## References

1. **Emacs Lisp Manual**: [Markers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Markers.html)
2. **Emacs Lisp Manual**: [Overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html)
3. **Lexicon Core API**: [docs/core/core-api.md](./core-api.md)
4. **Lexicon Undo System**: [docs/core/undo.md](./undo.md)

---

## Appendix A: Marker Adjustment Examples

### Example 1: Insertion Before Marker

```
Buffer: "hello world"
Marker at position 6 (before "world")

Insert "there " at position 5:
Buffer: "hellothere  world"
         ^     ^
        pos 5  pos 11 (marker adjusted +6)
```

### Example 2: Insertion At Marker (`:before`)

```
Buffer: "hello world"
Marker at position 5, insertion-type = :before

Insert "XXX" at position 5:
Buffer: "helloXXX world"
         ^   ^
        pos 5 (marker stays)
```

### Example 3: Insertion At Marker (`:after`)

```
Buffer: "hello world"
Marker at position 5, insertion-type = :after

Insert "XXX" at position 5:
Buffer: "helloXXX world"
             ^   ^
            pos 8 (marker moved past "XXX")
```

### Example 4: Deletion Across Marker

```
Buffer: "hello world"
Marker at position 8 (at "r")

Delete range [5, 10]:
Buffer: "hellod"
         ^   ^
        pos 5 (marker collapsed to deletion start)
```

---

## Appendix B: Overlay Priority Resolution

When multiple overlays cover the same position:

```clojure
;; Overlays:
(create-overlay buffer-id 0 10 :face :comment :priority 10)
(create-overlay buffer-id 5 15 :face :string :priority 20)
(create-overlay buffer-id 7 12 :face :keyword :priority 15)

;; At position 8:
;; - :comment  (priority 10)
;; - :string   (priority 20) ← WINS (highest)
;; - :keyword  (priority 15)

;; Result: :face = :string
```

**Tie-breaking**: If priorities equal, most recently created overlay wins.

---

**End of Marker System Specification**
