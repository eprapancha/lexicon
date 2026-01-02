# Lexicon Undo System Specification

**Version:** 1.0.0
**Status:** Design Document
**Last Updated:** 2026-01-02

## Table of Contents

1. [Overview](#overview)
2. [Undo Model](#undo-model)
3. [Command-Grouped Undo](#command-grouped-undo)
4. [Undo Records](#undo-records)
5. [Marker Deltas](#marker-deltas)
6. [Undo Boundaries](#undo-boundaries)
7. [Undo Tree](#undo-tree)
8. [Undo API Reference](#undo-api-reference)
9. [Implementation Guidelines](#implementation-guidelines)
10. [Performance Considerations](#performance-considerations)

---

## Overview

Lexicon's undo system provides **reversible editing** with command-level granularity, marker tracking, and tree-based history navigation. The system is designed to:

1. **Group changes by command** (not character-by-character)
2. **Track marker positions** through undo/redo
3. **Preserve full edit history** (undo tree, not linear stack)
4. **Support selective undo** (undo specific changes without undoing later ones)
5. **Handle complex edits** (multi-region, recursive edits, keyboard macros)

### Design Principles

1. **Command-centric**: Undo operates on logical commands, not individual edits
2. **Marker-aware**: Undo/redo restores marker positions accurately
3. **Non-destructive**: Undo doesn't delete history (enables redo from any point)
4. **Composable**: Supports batching, boundaries, and nested undo groups
5. **Emacs-compatible**: Follows Emacs undo model (with tree extension)

### Comparison to Emacs

| Feature | Emacs | Lexicon |
|---------|-------|---------|
| Granularity | Command-grouped | Command-grouped ✅ |
| Undo model | Linear (with undo-in-region) | Tree (undo-tree.el inspired) |
| Marker tracking | Basic | Full delta tracking ✅ |
| Selective undo | `undo-only` in region | Tree navigation ✅ |
| Undo limit | `undo-limit` (byte count) | Configurable (entries + size) |
| Visualization | No | Tree visualizer (future) |

---

## Undo Model

### Conceptual Model

The undo system tracks **undo records** organized into a **tree structure**:

```
Initial state (root)
     ↓
  [Command 1]  ← Insert "hello"
     ↓
  [Command 2]  ← Delete 2 chars
     ├─────────────┐
     ↓             ↓
  [Command 3a]  [Command 3b]  ← Branch: two different edits tried
  Insert "!"     Insert "?"
     ↓
  [Command 4]
```

### Key Concepts

1. **Undo Record**: Single reversible change (or group of changes)
2. **Undo Entry**: One element in undo record (text change, marker delta, property change)
3. **Undo Boundary**: Marker separating logical undo units (commands)
4. **Undo Tree**: DAG (directed acyclic graph) of all edit states
5. **Current Node**: Current position in undo tree

### States

Every buffer has:
- **Current state**: Text content + markers + properties
- **Undo tree**: History of all changes
- **Current node**: Position in tree (which state we're at)
- **Redo branches**: Available redo paths from current node

---

## Command-Grouped Undo

### Problem with Character-Level Undo

**Bad**: Undo after typing "hello world" requires 11 undo operations (one per character)

**Good**: Undo after typing "hello world" requires 1 undo operation (one command)

### Command Grouping Strategy

All edits within a **single command execution** are grouped into one undo record:

```clojure
;; User types: "hello world"
;; Results in 11 self-insert-command executions

;; WITHOUT command grouping (11 undo records):
{:type :insert :pos 0 :text "h"}
{:type :insert :pos 1 :text "e"}
{:type :insert :pos 2 :text "l"}
...  ;; 11 separate undo records ❌

;; WITH command grouping (1 undo record per command):
{:type :group
 :command :self-insert-command
 :entries [{:type :insert :pos 0 :text "h"}]}

{:type :group
 :command :self-insert-command
 :entries [{:type :insert :pos 1 :text "e"}]}
...  ;; Still 11 records, but can be collapsed ✅
```

### Smart Grouping

Consecutive identical commands are **automatically merged**:

```clojure
;; 11 self-insert-command invocations become:
{:type :group
 :command :self-insert-command
 :count 11
 :entries [{:type :insert :pos 0 :text "hello world"}]}
```

**Merge Criteria**:
1. Same command (`:self-insert-command`)
2. Consecutive (no other commands in between)
3. Same buffer
4. No explicit boundary inserted
5. Within merge window (default: 20 identical commands)

### Boundary Rules

Undo boundaries are **automatically inserted**:

1. **After each command** (except during merging)
2. **Before non-editing commands** (navigation, search)
3. **On explicit request** (`undo-boundary`)
4. **After idle timeout** (default: 5 seconds)
5. **Before buffer switch**

---

## Undo Records

### Record Structure

An undo record is a map containing:

```clojure
{:id          "undo-record-123"     ;; Unique ID
 :type        :group                 ;; :group, :boundary, :branch-point
 :command-id  :self-insert-command   ;; Command that created this record
 :timestamp   1704189600000          ;; When created (ms since epoch)
 :buffer-id   "buffer-456"           ;; Which buffer
 :entries     [...]                  ;; List of undo entries (see below)
 :point-before 10                    ;; Cursor position before
 :point-after  21                    ;; Cursor position after
 :parent      "undo-record-122"      ;; Parent node in tree
 :children    ["undo-record-124"]    ;; Child nodes (branches)
 :metadata    {...}}                 ;; Optional metadata
```

### Undo Entry Types

Each undo record contains a list of **undo entries**:

#### 1. Text Insertion

```clojure
{:type :insert
 :pos 10              ;; Position where text was inserted
 :text "hello"        ;; Text that was inserted
 :length 5}           ;; Length of insertion
```

**Reversal**: Delete 5 characters at position 10

---

#### 2. Text Deletion

```clojure
{:type :delete
 :pos 10              ;; Position where text was deleted
 :text "world"        ;; Text that was deleted
 :length 5}           ;; Length of deletion
```

**Reversal**: Insert "world" at position 10

---

#### 3. Text Replacement

```clojure
{:type :replace
 :pos 10              ;; Position of replacement
 :old-text "hello"    ;; Text that was replaced
 :new-text "goodbye"  ;; Text that replaced it
 :old-length 5
 :new-length 7}
```

**Reversal**: Replace "goodbye" with "hello" at position 10

---

#### 4. Marker Delta

```clojure
{:type :marker
 :marker-id "mark-789"     ;; Which marker
 :old-pos 10               ;; Position before change
 :new-pos 15               ;; Position after change
 :reason :text-insertion}  ;; Why it moved
```

**Reversal**: Set marker position to 10

---

#### 5. Property Change

```clojure
{:type :property
 :pos 10              ;; Position (or range start)
 :end 20              ;; Range end (or nil for point property)
 :property :face      ;; Property name
 :old-value :default  ;; Old value
 :new-value :comment} ;; New value
```

**Reversal**: Set property `:face` to `:default` in range [10, 20]

---

#### 6. Buffer Property

```clojure
{:type :buffer-property
 :property :read-only
 :old-value false
 :new-value true}
```

**Reversal**: Set buffer property `:read-only` to `false`

---

### Entry Ordering

Entries within a record are ordered **chronologically** (first edit to last). Reversal applies them in **reverse order**:

```clojure
;; Record entries (forward):
[{:type :delete :pos 10 :text "old"}
 {:type :insert :pos 10 :text "new"}
 {:type :marker :marker-id "point" :old-pos 10 :new-pos 13}]

;; Reversal (backward):
;; 1. Set marker to position 10
;; 2. Delete "new" at position 10
;; 3. Insert "old" at position 10
```

---

## Marker Deltas

### Problem

When undoing text changes, **markers must move too**:

```
Initial: "hello|world"  (| = marker at position 5)

Insert " there": "hello there|world"
                 marker now at position 11

Undo: "hello|world"
      marker must return to position 5 ✅
```

### Solution: Marker Delta Tracking

Every text change records **how markers moved**:

```clojure
;; Insert " there" at position 5
{:type :group
 :entries [{:type :insert :pos 5 :text " there" :length 6}
           {:type :marker :marker-id "point" :old-pos 5 :new-pos 11}
           {:type :marker :marker-id "mark" :old-pos 8 :new-pos 14}]}
```

### Marker Movement Rules

When text is inserted at position `P` with length `L`:

- Markers **before** `P`: No change
- Markers **at** `P`: Move to `P + L` (insertion point semantics)
- Markers **after** `P`: Move by `+L`

When text is deleted at position `P` with length `L`:

- Markers **before** `P`: No change
- Markers **in** range `[P, P+L)`: Move to `P` (collapse to deletion point)
- Markers **after** `P + L`: Move by `-L`

### Special Markers

Some markers have special semantics:

| Marker | Behavior | Example |
|--------|----------|---------|
| `point` | Always moves with insertion | Cursor follows typing |
| `mark` | Stays at original position | Region selection anchor |
| `overlay-start` | Adjusts with text changes | Syntax highlight start |
| `overlay-end` | Adjusts with text changes | Syntax highlight end |

### Marker Tracking During Undo

When undoing:
1. **Apply text changes in reverse**
2. **Restore marker positions** from deltas
3. **Validate marker positions** (clamp to buffer bounds)

```clojure
(defn undo-entry [buffer entry]
  (case (:type entry)
    :insert
    (delete-text buffer (:pos entry) (:length entry))

    :delete
    (insert-text buffer (:pos entry) (:text entry))

    :marker
    (set-marker-position buffer (:marker-id entry) (:old-pos entry))
    ;; ^ Restore marker position
    ))
```

---

## Undo Boundaries

### Purpose

Boundaries define **logical undo units**. Without boundaries, undo operates on individual edits (too granular).

### Boundary Insertion

Boundaries are **automatically** inserted:

1. **After each command** (default)
2. **Before buffer switch**
3. **After idle timeout** (5 seconds of inactivity)
4. **On explicit request** (`undo-boundary`)

### Explicit Boundaries

Packages can insert boundaries manually:

```clojure
(api/undo-boundary)

;; Example: Group multiple commands as one undo unit
(api/undo-boundary)
(api/insert! "(\n")
(api/indent-line)
(api/insert! ")\n")
(api/undo-boundary)

;; Now undoing once removes all 3 edits
```

### Boundary Suppression

During batch operations, suppress boundaries:

```clojure
(api/without-undo-boundaries
  (dotimes [i 1000]
    (api/insert! (str "Line " i "\n"))))

;; Only ONE undo operation for all 1000 insertions
```

### Merge Windows

Consecutive identical commands are merged within a **merge window**:

```clojure
;; Default: 20 consecutive self-insert-commands merge
(api/self-insert-command \h)
(api/self-insert-command \e)
...  ;; 20 times
;; Results in 1 undo record

;; 21st command starts new record
(api/self-insert-command \!)  ;; New undo record
```

**Configuration**:
```clojure
(api/set-undo-merge-window! :self-insert-command 50)  ;; Merge up to 50
(api/set-undo-merge-window! :delete-char 20)
```

---

## Undo Tree

### Linear Undo Problem

Traditional undo is **linear** (stack):

```
[1] → [2] → [3] → [4]
              ↑
           Current

Undo: [1] → [2] → [3]
                    ↑

Edit: [1] → [2] → [3] → [5]
                          ↑
                    [4] lost forever! ❌
```

### Tree Undo Solution

Lexicon uses **tree-based undo** (like `undo-tree.el`):

```
        [1]
         ↓
        [2]
         ↓
        [3] ← Current
       ↙   ↘
     [4]   [5]
           ↓
          [6]
```

**Benefits**:
1. **Never lose history**: Both branches [4] and [5] preserved
2. **Navigate freely**: Move to any previous state
3. **Visualize history**: See tree structure (future feature)

### Tree Navigation

```clojure
;; Undo: Move to parent
(api/undo)  ;; [3] → [2]

;; Redo: Move to most recent child
(api/redo)  ;; [2] → [3]

;; Switch branch: Choose different child
(api/undo-tree-switch-branch 1)  ;; [3] → [5]

;; Jump to specific node
(api/undo-tree-goto "undo-record-123")
```

### Tree Structure

Each node has:
- **Parent**: Previous state (at most 1)
- **Children**: Possible next states (0 or more)
- **Timestamp**: When created
- **Command**: What created it

```clojure
{:id "node-3"
 :parent "node-2"
 :children ["node-4" "node-5"]  ;; Branch point
 :timestamp 1704189600000
 :command :self-insert-command
 :entries [...]}
```

### Current Node Tracking

Buffer tracks **current position** in tree:

```clojure
;; In app-db
{:buffers
 {"buffer-123"
  {:undo-tree {:root "node-0"
               :current "node-3"   ;; ← Current position
               :nodes {
                 "node-0" {...}
                 "node-1" {...}
                 "node-2" {...}
                 "node-3" {...}  ;; Current state
                 "node-4" {...}  ;; Redo branch 1
                 "node-5" {...}  ;; Redo branch 2
               }}}}
```

---

## Undo API Reference

### `undo`

Undoes the last command.

**Signature**:
```clojure
(undo)
(undo n)  ;; Undo n commands
```

**Parameters**:
- `n` (optional): Number of commands to undo (default: 1)

**Returns**: Number of commands actually undone

**Behavior**:
- Moves to parent node in undo tree
- Applies reverse of all entries in current node
- Updates point to `:point-before`
- If at root, shows "No further undo information"

**Example**:
```clojure
(api/undo)     ;; Undo last command
(api/undo 5)   ;; Undo last 5 commands
```

---

### `redo`

Redoes the last undone command.

**Signature**:
```clojure
(redo)
(redo n)  ;; Redo n commands
```

**Parameters**:
- `n` (optional): Number of commands to redo (default: 1)

**Returns**: Number of commands actually redone

**Behavior**:
- Moves to most recent child node in undo tree
- If multiple children (branch point), uses most recent
- Applies all entries in child node
- Updates point to `:point-after`
- If no children, shows "No further redo information"

**Example**:
```clojure
(api/redo)     ;; Redo last undone command
(api/redo 3)   ;; Redo 3 commands
```

---

### `undo-only`

Undoes without creating redo entries (linear undo mode).

**Signature**:
```clojure
(undo-only)
(undo-only n)
```

**Behavior**:
- Like `undo`, but marks current branch as "dead"
- Subsequent edits won't create branch point
- Emulates traditional linear undo

**Use Case**: Prefer simple undo model over tree

---

### `undo-tree-visualize`

Shows undo tree graphically (future feature).

**Signature**:
```clojure
(undo-tree-visualize)
```

**Behavior**:
- Opens `*Undo Tree*` buffer
- Displays ASCII art tree:
  ```
      o  [1] Initial
      |
      o  [2] Insert "hello"
      |
      o  [3] Delete 2 chars
     / \
    o   o  [4] Insert "!"  [5] Insert "?" ← Current
    |
    o  [6] Delete "!"
  ```
- Navigate tree with arrow keys
- Press `RET` to jump to node

---

### `undo-boundary`

Inserts an explicit undo boundary.

**Signature**:
```clojure
(undo-boundary)
```

**Returns**: `true` if boundary inserted, `false` if already at boundary

**Use Case**: Group multiple edits into one undo unit

**Example**:
```clojure
(api/undo-boundary)
(api/insert! "line 1\n")
(api/insert! "line 2\n")
(api/insert! "line 3\n")
(api/undo-boundary)

;; Undo once removes all 3 lines
```

---

### `without-undo-boundaries`

Executes code with automatic boundaries suppressed.

**Signature**:
```clojure
(without-undo-boundaries & body)
```

**Example**:
```clojure
(api/without-undo-boundaries
  (dotimes [i 1000]
    (api/insert! "x")))

;; All 1000 insertions are one undo unit
```

---

### `undo-tree-save`

Saves undo tree to persistent storage (future feature).

**Signature**:
```clojure
(undo-tree-save filename)
```

**Use Case**: Preserve undo history across sessions

---

### `undo-tree-restore`

Restores undo tree from file (future feature).

**Signature**:
```clojure
(undo-tree-restore filename)
```

---

## Implementation Guidelines

### For Core Developers

#### 1. Recording Undo Entries

Every mutating operation must record undo entry:

```clojure
(defn insert-text! [buffer-id pos text]
  ;; Record undo BEFORE applying change
  (record-undo buffer-id
    {:type :insert
     :pos pos
     :text text
     :length (count text)})

  ;; Apply change
  (apply-insert buffer-id pos text)

  ;; Record marker deltas
  (doseq [marker (affected-markers buffer-id pos)]
    (record-undo buffer-id
      {:type :marker
       :marker-id (:id marker)
       :old-pos (:pos marker)
       :new-pos (adjust-marker-pos marker pos (count text))})))
```

#### 2. Grouping Edits

Use command context to group edits:

```clojure
(defn execute-command [command-id & args]
  ;; Start new undo group
  (start-undo-group command-id)

  ;; Execute command (may record multiple entries)
  (apply-command command-id args)

  ;; End undo group (insert boundary)
  (end-undo-group)
  (undo-boundary))
```

#### 3. Merging Consecutive Commands

Check if current command can merge with previous:

```clojure
(defn can-merge-undo? [buffer-id command-id]
  (let [prev (last-undo-record buffer-id)]
    (and prev
         (= (:command-id prev) command-id)
         (< (- (current-time) (:timestamp prev)) 5000)  ;; 5 sec window
         (< (:merge-count prev 0) (merge-window command-id)))))

(defn execute-command [command-id & args]
  (if (can-merge-undo? (current-buffer) command-id)
    (merge-into-current-undo-group command-id args)
    (do
      (start-undo-group command-id)
      (apply-command command-id args)
      (end-undo-group)
      (undo-boundary))))
```

#### 4. Tree Node Creation

Create child node when editing after undo:

```clojure
(defn create-undo-node [buffer-id entries]
  (let [tree (get-undo-tree buffer-id)
        current-node-id (:current tree)
        new-node-id (generate-id)
        new-node {:id new-node-id
                  :parent current-node-id
                  :children []
                  :timestamp (current-time)
                  :command *current-command*
                  :entries entries}]

    ;; Add new node to tree
    (update-tree buffer-id
      [:nodes new-node-id] new-node)

    ;; Link to parent
    (update-tree buffer-id
      [:nodes current-node-id :children]
      conj new-node-id)

    ;; Set as current
    (update-tree buffer-id
      [:current] new-node-id)))
```

#### 5. Applying Undo

Reverse entries in reverse order:

```clojure
(defn apply-undo [buffer-id]
  (let [tree (get-undo-tree buffer-id)
        current-node (get-in tree [:nodes (:current tree)])
        entries (:entries current-node)]

    ;; Apply in reverse order
    (doseq [entry (reverse entries)]
      (apply-undo-entry buffer-id entry))

    ;; Move to parent
    (update-tree buffer-id [:current] (:parent current-node))

    ;; Restore point
    (set-point buffer-id (:point-before current-node))))
```

---

### For Package Developers

#### 1. Respecting Undo Boundaries

Don't insert boundaries unless necessary:

```clojure
;; ❌ BAD: Inserting too many boundaries
(defn insert-lines [lines]
  (doseq [line lines]
    (api/insert! line)
    (api/undo-boundary)))  ;; Each line is separate undo! Bad!

;; ✅ GOOD: One boundary for whole operation
(defn insert-lines [lines]
  (api/undo-boundary)
  (doseq [line lines]
    (api/insert! line))
  (api/undo-boundary))
```

#### 2. Atomic Operations

Group related edits:

```clojure
(defn surround-region [start end left right]
  (api/without-undo-boundaries
    (api/goto-char end)
    (api/insert! right)
    (api/goto-char start)
    (api/insert! left)))

;; Undoing once removes both delimiters
```

#### 3. Marker-aware Editing

When manipulating markers, let core track deltas:

```clojure
;; ❌ WRONG: Manual marker adjustment
(defn insert-and-move-mark [text]
  (let [mark-pos (api/mark)]
    (api/insert! text)
    (api/set-mark (+ mark-pos (count text)))))  ;; Defeats undo tracking!

;; ✅ CORRECT: Let core adjust markers
(defn insert-and-move-mark [text]
  (api/insert! text))  ;; Core automatically records marker deltas
```

#### 4. Testing Undo

Always test undo/redo:

```clojure
(deftest test-surround-region-undo
  (with-temp-buffer
    (insert! "hello")
    (let [start 0
          end 5]
      (surround-region start end "(" ")")
      (is (= "(hello)" (buffer-string)))

      ;; Undo should remove both delimiters
      (undo)
      (is (= "hello" (buffer-string)))

      ;; Redo should restore both
      (redo)
      (is (= "(hello)" (buffer-string))))))
```

---

## Performance Considerations

### Memory Usage

Undo trees can grow **very large** for long editing sessions. Mitigation strategies:

#### 1. Undo Limits

Impose size limits:

```clojure
;; Configuration
{:undo {:max-entries 10000        ;; Max undo records per buffer
        :max-size-bytes 50000000  ;; Max 50MB per buffer
        :max-age-ms 86400000}}    ;; Max 24 hours old
```

#### 2. Garbage Collection

Periodically prune old branches:

```clojure
(defn prune-undo-tree [buffer-id]
  (let [tree (get-undo-tree buffer-id)
        now (current-time)
        cutoff (- now (get-in config [:undo :max-age-ms]))]

    ;; Remove nodes older than cutoff
    (update-tree buffer-id [:nodes]
      (fn [nodes]
        (into {}
          (filter #(> (:timestamp (val %)) cutoff) nodes))))))
```

#### 3. Compression

Compress large text deltas:

```clojure
(defn create-undo-entry [type pos text]
  {:type type
   :pos pos
   :text (if (> (count text) 1000)
           (compress text)  ;; LZ4 or gzip
           text)
   :compressed? (> (count text) 1000)})
```

### Computational Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Record undo | O(1) | Append to current group |
| Undo | O(n) | n = entries in record (usually small) |
| Redo | O(n) | n = entries in record |
| Tree traversal | O(d) | d = depth from current to target |
| Boundary insert | O(1) | Mark end of group |
| GC prune | O(N) | N = total nodes (periodic, amortized) |

### Optimization Strategies

1. **Lazy entry creation**: Only materialize undo entries when needed
2. **Entry deduplication**: Merge identical consecutive entries
3. **Batch boundaries**: Group rapid commands automatically
4. **Incremental GC**: Prune during idle time, not during editing

---

## Migration from Current Implementation

### Current State (Phase 6)

Lexicon currently has **basic undo**:
- Character-level granularity (too fine)
- No marker tracking
- Linear stack (no tree)
- Limited to 100 entries

### Migration Path (Phase 7.4)

**Week 1: Undo Record Infrastructure**
1. Design undo record schema
2. Implement undo tree data structure
3. Add undo recording to text operations
4. Add marker delta tracking

**Week 2: Command Grouping**
1. Implement undo boundary system
2. Add command-level grouping
3. Implement merge windows
4. Add boundary suppression

**Week 3: Tree Navigation**
1. Implement undo/redo with tree
2. Add branch switching
3. Add tree traversal APIs
4. Implement tree visualization (basic)

**Week 4: Testing & Optimization**
1. Unit tests for all entry types
2. Integration tests for complex scenarios
3. Performance benchmarks
4. GC and pruning

---

## Future Enhancements

### 1. Persistent Undo (Phase 8+)

Save undo tree to disk:
- Survive editor restarts
- Large trees stored externally
- On-demand loading of old branches

### 2. Undo Tree Visualization

Interactive tree browser:
- ASCII art in buffer
- Click nodes to jump
- Show diffs between nodes
- Color-code branches

### 3. Selective Undo

Undo specific changes:
```clojure
(api/undo-tree-undo-node "node-5")  ;; Undo just this change
```

### 4. Collaborative Undo

Multi-user editing:
- Per-user undo trees
- Merge conflict resolution
- Shared vs local undo

### 5. Undo Checkpoints

Named savepoints:
```clojure
(api/undo-checkpoint "before-refactor")
(do-big-refactor)
(api/undo-to-checkpoint "before-refactor")  ;; Undo everything since
```

---

## References

1. **Emacs Lisp Manual**: [Undo](https://www.gnu.org/software/emacs/manual/html_node/elisp/Undo.html)
2. **undo-tree.el**: [Tree-based undo system](https://www.emacswiki.org/emacs/UndoTree)
3. **Lexicon Core API**: [docs/core/core-api.md](./core-api.md)
4. **Lexicon Markers**: [docs/core/markers.md](./markers.md)

---

## Appendix A: Entry Type Quick Reference

| Type | Reversal | Marker Impact | Example |
|------|----------|---------------|---------|
| `:insert` | Delete at pos | Markers after pos shift right | Type "hello" |
| `:delete` | Insert text at pos | Markers after pos shift left | Backspace |
| `:replace` | Replace new with old | Both shifts | Replace "hi" → "hello" |
| `:marker` | Restore old pos | Direct marker move | Cursor jump |
| `:property` | Restore old value | None | Font-lock face |
| `:buffer-property` | Restore old value | None | Read-only flag |

---

## Appendix B: Undo Tree Examples

### Example 1: Simple Linear History

```
User types "hello", undoes, types "hi"

Tree:
  [0] Empty buffer
   ↓
  [1] Insert "h"
   ↓
  [2] Insert "e"
   ↓
  [3] Insert "l"
   ↓
  [4] Insert "l"
   ↓
  [5] Insert "o"

After undo (back to [0]):
  [0] Empty buffer ← current
   ↓
  [1] Insert "h"
   ... (still exists, can redo)

After typing "hi":
  [0] Empty buffer
  ↙  ↘
 [1]  [6] Insert "h"  ← branch point
  ↓    ↓
 ...  [7] Insert "i"  ← current
```

### Example 2: Complex Branching

```
          [0]
           ↓
          [1]
           ↓
          [2]
         ↙  ↘
       [3]  [4]
        ↓    ↓
       [5]  [6]
            ↙ ↘
          [7] [8]
```

Navigation:
- From [2], user can reach [3] or [4] (redo branches)
- From [6], user can reach [7] or [8]
- Undo from [8] goes to [6], then [4], then [2], etc.

---

**End of Undo System Specification**
