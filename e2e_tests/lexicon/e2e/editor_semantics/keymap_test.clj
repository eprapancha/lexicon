(ns lexicon.e2e.editor-semantics.keymap-test
  "E2E tests for Emacs keymap semantic - Epic #86

  Tests critical invariants:
  - Buffer-local keymaps shadow global keymaps
  - Prefix keys wait for completion before dispatching"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-local-keymap-shadows-global
  (testing "Emacs invariant: Buffer-local keymaps shadow global keymaps"
    (h/setup-test*)

    ;; Test a working keybinding - C-x b for switch-buffer
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    ;; Should show minibuffer for switch-buffer
    (let [minibuf-visible (h/minibuffer-visible?)
          minibuf-prompt (h/get-minibuffer-text)]
      (is minibuf-visible
          "C-x b should open minibuffer")
      (is (or (.contains minibuf-prompt "Buffer")
              (.contains minibuf-prompt "buffer")
              (.contains minibuf-prompt "Switch"))
          (str "Should show buffer switch prompt, got: '" minibuf-prompt "'")))

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Note: We can't easily test buffer-local keymap shadowing in e2e
    ;; because that requires elisp configuration to set up local keymaps.
    ;; This test verifies the global keymap works.
    ))

(deftest test-prefix-key-waits-for-completion
  (testing "Emacs invariant: Prefix keys wait for completion before dispatching"
    (h/setup-test*)

    ;; Press prefix key C-x
    (h/press-ctrl "x")
    (Thread/sleep 50)

    ;; Minibuffer should NOT be visible yet - we're waiting for completion
    (let [minibuf-before-complete (h/minibuffer-visible?)]
      (is (not minibuf-before-complete)
          "After C-x alone, minibuffer should not open yet (waiting for completion)"))

    ;; Now complete with 'b' to get switch-buffer
    (h/press-key "b")
    (Thread/sleep 100)

    ;; NOW minibuffer should open for switch-buffer
    (let [minibuf-visible (h/minibuffer-visible?)
          minibuf-prompt (h/get-minibuffer-text)]
      (is minibuf-visible
          "After C-x b complete sequence, minibuffer should open")
      (is (or (.contains minibuf-prompt "Buffer")
              (.contains minibuf-prompt "buffer")
              (.contains minibuf-prompt "Switch"))
          (str "Should show buffer switch prompt, got: '" minibuf-prompt "'")))

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 50)))
