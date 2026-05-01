(ns lexicon.lisp.keymap-test
  "Lisp API tests for keymap remapping functionality (Issue #263).

  These tests use eval-lisp to verify the define-key and command-remapping
  functions work correctly for package developers who need to remap commands."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

(defn normalize-cmd
  "Normalize command to keyword for comparison.
  SCI may return keywords, strings, or symbols depending on context."
  [cmd]
  (cond
    (keyword? cmd) cmd
    (string? cmd) (keyword cmd)
    (symbol? cmd) (keyword (name cmd))
    :else cmd))

;; =============================================================================
;; define-key with [remap CMD] syntax (Issue #263)
;; =============================================================================

(deftest test-define-key-remap-basic
  (testing "define-key with [remap CMD] sets up command remapping"
    (lh/setup-test)

    ;; Set up a remap: [remap kill-line] -> :my-kill-line in global keymap
    ;; In ClojureScript syntax, symbols in vectors must be quoted
    (lh/eval-lisp! "(define-key 'global-map [remap 'kill-line] 'my-kill-line)")

    ;; Verify the remap is set up via command-remapping
    (let [result (lh/eval-lisp! "(command-remapping 'kill-line)")]
      (is (= :my-kill-line (normalize-cmd result))
          "command-remapping should return the remapped command"))))

(deftest test-define-key-remap-mode-specific
  (testing "define-key with [remap CMD] works for mode-specific keymaps"
    (lh/setup-test)

    ;; Set up a remap in a mode keymap
    ;; fundamental-mode is always active for buffers without a specific mode
    ;; In ClojureScript syntax, symbols in vectors must be quoted
    (lh/eval-lisp! "(define-key 'fundamental-mode-map [remap 'backward-char] 'my-backward)")
    (Thread/sleep 100)

    ;; Verify the remap is active (fundamental-mode is active by default)
    (let [result (lh/eval-lisp! "(command-remapping 'backward-char)")]
      (is (= :my-backward (normalize-cmd result))
          "Mode-specific remap should be active when mode is enabled"))))

(deftest test-define-key-regular-binding
  (testing "define-key still works for regular key bindings"
    (lh/setup-test)

    ;; Set up a regular key binding
    (lh/eval-lisp! "(define-key 'global-map \"C-c t\" 'test-command)")

    ;; Verify via key-binding
    (let [result (lh/eval-lisp! "(key-binding \"C-c t\")")]
      (is (= :test-command (normalize-cmd result))
          "Regular key binding should work via define-key"))))

;; =============================================================================
;; command-remapping lookup (Issue #263)
;; =============================================================================

(deftest test-command-remapping-returns-nil-when-no-remap
  (testing "command-remapping returns nil when no remap exists"
    (lh/setup-test)

    ;; Query a command that has no remap
    (let [result (lh/eval-lisp! "(command-remapping 'nonexistent-command)")]
      (is (nil? result)
          "command-remapping should return nil for unmapped commands"))))

(deftest test-command-remapping-with-keyword
  (testing "command-remapping accepts both symbols and keywords"
    (lh/setup-test)

    ;; Set up remap - use quoted symbol in vector
    (lh/eval-lisp! "(define-key :global [remap 'undo] :my-undo)")

    ;; Test with symbol
    (is (= :my-undo (normalize-cmd (lh/eval-lisp! "(command-remapping 'undo)")))
        "command-remapping should work with symbol argument")

    ;; Test with keyword (alternate syntax)
    (is (= :my-undo (normalize-cmd (lh/eval-lisp! "(command-remapping :undo)")))
        "command-remapping should work with keyword argument")))

;; =============================================================================
;; Keymap type variations
;; =============================================================================

(deftest test-define-key-keymap-variations
  (testing "define-key accepts various keymap specifications"
    (lh/setup-test)

    ;; Test :global keyword - use quoted symbol in vector
    (lh/eval-lisp! "(define-key :global [remap 'cmd1] 'new-cmd1)")
    (is (= :new-cmd1 (normalize-cmd (lh/eval-lisp! "(command-remapping 'cmd1)")))
        ":global keyword should work")

    ;; Test 'global-map symbol
    (lh/eval-lisp! "(define-key 'global-map [remap 'cmd2] 'new-cmd2)")
    (is (= :new-cmd2 (normalize-cmd (lh/eval-lisp! "(command-remapping 'cmd2)")))
        "'global-map symbol should work")

    ;; Test :global-map keyword
    (lh/eval-lisp! "(define-key :global-map [remap 'cmd3] 'new-cmd3)")
    (is (= :new-cmd3 (normalize-cmd (lh/eval-lisp! "(command-remapping 'cmd3)")))
        ":global-map keyword should work")))
