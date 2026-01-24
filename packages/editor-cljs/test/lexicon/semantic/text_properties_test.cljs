(ns lexicon.semantic.text-properties-test
  "Semantic tests for text properties - CORE primitive needed by all packages.

  Text properties attach invisible metadata to buffer text ranges.
  Used by: completion UI, syntax highlighting, org-mode, dired, magit, etc.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - affects all packages"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;; Emacs-style t for true
(def t true)

;;; =============================================================================
;;; Text Properties - Core Primitive Tests
;;; =============================================================================

(deftest ^:critical text-properties-invisible
  "CRITICAL: Text with invisible property is not rendered but remains in buffer.

  Semantic Guarantee:
  - Buffer text includes invisible content (for search, etc.)
  - Visible/rendered text excludes invisible content
  - Invisible property is range-based, not character-based

  Why this matters:
  - Dired hides file details without deleting them
  - Org-mode hides markup (**, [[]], etc.)
  - Completion UI hides metadata

  Implementation Note:
  - :properties map on text ranges in buffer
  - Rendering layer respects 'invisible property
  - NOT implemented yet - this test will fail"
  (testing "Invisible text hidden from display"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      ;; Make "World" invisible
      (helpers/put-text-property 6 11 'invisible t)

      ;; Buffer still contains full text
      (is (= "Hello World" (helpers/buffer-text))
          "Buffer text includes invisible content")

      ;; But visible/rendered text excludes it
      (is (= "Hello " (helpers/visible-text))
          "Visible text excludes invisible regions")))

  (testing "Invisible property is per-region not per-char"
    (with-test-buffer "*test*"
      (helpers/insert "AAABBBCCC")
      ;; Make middle section invisible
      (helpers/put-text-property 3 6 'invisible t)

      (is (= "AAACCC" (helpers/visible-text))
          "Middle region hidden as a unit"))))

(deftest ^:critical text-properties-survive-edits
  "CRITICAL: Text properties move with text during buffer edits.

  Semantic Guarantee:
  - Insert before property range: property range shifts forward
  - Insert within property range: property expands
  - Delete within property range: property shrinks
  - Properties are preserved across undo/redo

  Why this matters:
  - Org-mode folding survives edits
  - Syntax highlighting remains correct
  - Dired filename properties stay aligned

  Implementation Note:
  - Properties stored as intervals, not absolute positions
  - WASM gap buffer must track property ranges
  - NOT implemented yet"
  (testing "Property range adjusts on insert before"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/put-text-property 6 11 'face 'bold)

      ;; Insert before the property range
      (helpers/goto-char 0)
      (helpers/insert "XXX")

      ;; Property should shift by 3 positions
      (is (= 'bold (helpers/get-text-property 9 'face))
          "Property at start of range shifted")
      (is (= 'bold (helpers/get-text-property 13 'face))
          "Property at end of range shifted")))

  (testing "Property expands when text inserted within range"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/put-text-property 6 11 'face 'bold)

      ;; Insert within the bold region
      (helpers/goto-char 8)
      (helpers/insert "XXX")

      ;; New text should inherit the property
      (is (= 'bold (helpers/get-text-property 8 'face))
          "Newly inserted text inherits property")))

  (testing "Property shrinks when text deleted from range"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/put-text-property 6 11 'face 'bold)

      ;; Delete part of the bold region
      (helpers/delete-region 6 8)

      ;; Property range should shrink
      (is (= 'bold (helpers/get-text-property 6 'face))
          "Property still present at start")
      (is (nil? (helpers/get-text-property 10 'face))
          "Property range shortened"))))

(deftest ^:high text-property-keymaps
  "HIGH: Keymap property enables region-specific key bindings.

  Semantic Guarantee:
  - Text with keymap property has local key bindings
  - Bindings only active when point is in that region
  - Property keymaps override mode keymaps

  Why this matters:
  - Dired makes directory names clickable
  - Org-mode links respond to RET
  - Completion candidates respond to TAB

  Implementation Note:
  - Keymap lookup checks text properties at point
  - Property keymap has higher precedence than mode keymap
  - NOT implemented yet"
  (testing "Keys work differently in property region"
    (with-test-buffer "*test*"
      ;; Create a special keymap for one region
      (let [special-map (helpers/make-keymap)]
        (helpers/define-key special-map "x" :special-x-command)

        (helpers/insert "Normal Special Normal")
        (helpers/put-text-property 7 14 'keymap special-map)

        ;; Outside the region, normal binding
        (helpers/goto-char 2)
        (is (= :self-insert (helpers/lookup-key "x"))
            "Normal key binding outside region")

        ;; Inside the region, special binding
        (helpers/goto-char 10)
        (is (= :special-x-command (helpers/lookup-key "x"))
            "Special binding inside property region")))))

(deftest ^:high text-property-mouse-face
  "HIGH: Mouse-face property for hover highlighting.

  Semantic Guarantee:
  - Text with mouse-face property highlights on mouse-over
  - Different from normal face property
  - Used for interactive elements

  Why this matters:
  - Clickable links show feedback
  - Completion candidates highlight on hover
  - Dired file names highlight

  Implementation Note:
  - Rendering layer detects mouse position
  - Applies mouse-face style when hovering
  - NOT implemented yet - requires mouse tracking"
  (testing "Mouse-face property exists and can be queried"
    (with-test-buffer "*test*"
      (helpers/insert "Click me")
      (helpers/put-text-property 0 8 'mouse-face 'highlight)

      (is (= 'highlight (helpers/get-text-property 4 'mouse-face))
          "Mouse-face property set correctly"))))

(deftest ^:high text-property-help-echo
  "HIGH: Help-echo property for tooltips.

  Semantic Guarantee:
  - Text with help-echo property shows tooltip on hover
  - Tooltip text can be string or function
  - Used for contextual help

  Why this matters:
  - Dired shows \"click to visit\" on filenames
  - Org-mode shows link destinations
  - Completion shows candidate documentation

  Implementation Note:
  - UI layer displays tooltip from property
  - NOT implemented yet"
  (testing "Help-echo property exists"
    (with-test-buffer "*test*"
      (helpers/insert "Hover me")
      (helpers/put-text-property 0 8 'help-echo "This is a tooltip")

      (is (= "This is a tooltip" (helpers/get-text-property 4 'help-echo))
          "Help-echo property set correctly"))))

(deftest ^:medium text-property-face
  "MEDIUM: Face property for styling (color, bold, etc).

  Semantic Guarantee:
  - Text with face property renders with that style
  - Face can be keyword (named face) or map (inline style)
  - Multiple faces can be combined

  Why this matters:
  - Syntax highlighting
  - Org-mode markup (bold, italic, etc.)
  - Dired executable files in green

  Implementation Note:
  - CSS classes or inline styles in rendering
  - Face inheritance system
  - NOT fully implemented"
  (testing "Face property for basic styling"
    (with-test-buffer "*test*"
      (helpers/insert "Bold text")
      (helpers/put-text-property 0 4 'face 'bold)

      (is (= 'bold (helpers/get-text-property 2 'face))
          "Face property set"))))

(deftest ^:medium text-properties-multiple
  "MEDIUM: Multiple properties on same text range.

  Semantic Guarantee:
  - Same text can have multiple properties
  - Properties are independent
  - Properties can overlap

  Why this matters:
  - Text can be both invisible AND have a face
  - Dired filenames have: face, keymap, mouse-face, help-echo

  Implementation Note:
  - Properties stored as map per range
  - NOT implemented yet"
  (testing "Multiple properties coexist"
    (with-test-buffer "*test*"
      (helpers/insert "Multi-property text")
      (helpers/put-text-property 0 18 'face 'bold)
      (helpers/put-text-property 0 18 'invisible t)
      (helpers/put-text-property 0 18 'help-echo "Tooltip")

      (is (= 'bold (helpers/get-text-property 5 'face)))
      (is (= t (helpers/get-text-property 5 'invisible)))
      (is (= "Tooltip" (helpers/get-text-property 5 'help-echo))))))

(deftest ^:medium text-properties-propertize
  "MEDIUM: Propertize function creates string with properties.

  Semantic Guarantee:
  - (propertize string prop1 val1 prop2 val2 ...) returns string with properties
  - Convenience function for creating property strings
  - Used heavily in completion, minibuffer prompts

  Implementation Note:
  - Helper function, not core primitive
  - Can be implemented once put-text-property exists
  - NOT implemented yet"
  (testing "Propertize creates property string"
    (let [s (helpers/propertize "Click" 'face 'link 'help-echo "Tooltip")]
      (is (= "Click" s) "String content unchanged")
      (is (= 'link (helpers/get-text-property-from-string s 0 'face)))
      (is (= "Tooltip" (helpers/get-text-property-from-string s 0 'help-echo))))))

;;; =============================================================================
;;; Property Query Functions
;;; =============================================================================

(deftest ^:high text-properties-at
  "HIGH: text-properties-at returns all properties at position.

  Semantic Guarantee:
  - Returns plist (property-list) of all properties at position
  - Returns empty list if no properties
  - Convenient for bulk property queries

  Implementation Note:
  - Helper built on get-text-property
  - NOT implemented yet"
  (testing "Get all properties at once"
    (with-test-buffer "*test*"
      (helpers/insert "Text")
      (helpers/put-text-property 0 4 'face 'bold)
      (helpers/put-text-property 0 4 'invisible t)

      (let [props (helpers/text-properties-at 2)]
        (is (contains? props 'face))
        (is (contains? props 'invisible))
        (is (= 'bold (get props 'face)))
        (is (= t (get props 'invisible)))))))

;;; =============================================================================
;;; Integration with Core Systems
;;; =============================================================================

(deftest ^:critical text-properties-persist-across-undo
  "CRITICAL: Text properties survive undo/redo.

  Semantic Guarantee:
  - Undo removes text AND its properties
  - Redo restores text AND its properties
  - Property changes themselves can be undone

  Why this matters:
  - Org-mode folding survives undo
  - Syntax highlighting remains consistent
  - Critical for reliable editing

  Implementation Note:
  - Undo system must track property changes
  - NOT implemented yet - will fail"
  (testing "Properties restored on undo"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/put-text-property 0 5 'face 'bold)

      ;; Delete the text
      (helpers/delete-region 0 5)
      (is (= "" (helpers/buffer-text)))

      ;; Undo the deletion
      (helpers/undo)
      (is (= "Hello" (helpers/buffer-text)))
      (is (= 'bold (helpers/get-text-property 2 'face))
          "Property restored with text"))))

(deftest ^:high text-properties-in-read-only-buffer
  "HIGH: Text properties work in read-only buffers.

  Semantic Guarantee:
  - Read-only buffers can have text properties
  - Properties can be added with inhibit-read-only
  - Dired, help, and info buffers all use this

  Why this matters:
  - Dired buffer is read-only but has clickable links
  - Help buffers have interactive cross-references

  Implementation Note:
  - Properties are metadata, not content
  - Should be modifiable even in read-only buffers
  - Needs inhibit-read-only mechanism"
  (testing "Properties in read-only buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Read only")
      (helpers/set-buffer-read-only t)

      ;; Should be able to add properties even when read-only
      (let [inhibit-read-only true]
        (helpers/put-text-property 0 9 'face 'bold))

      (is (= 'bold (helpers/get-text-property 4 'face))
          "Property set in read-only buffer"))))
