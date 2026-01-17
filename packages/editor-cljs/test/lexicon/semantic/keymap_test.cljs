(ns lexicon.semantic.keymap-test
  "Emacs semantic compatibility tests for keymap resolution.

  Tests from Epic #86, Issue #87 - Core Invariants"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical local-keymap-shadows-global-keymap
  (testing "Emacs invariant: Buffer-local keymaps shadow global keymaps"
    (h/reset-editor-db!)
    ;; Set up global keymap with binding
    (h/set-global-key "C-x C-f" :find-file)
    ;; Create buffer with local keymap that shadows it
    (let [buf (h/create-buffer "test")]
      (h/set-buffer-local-key buf "C-x C-f" :my-custom-find)
      ;; In this buffer, C-x C-f should resolve to local binding
      (is (= :my-custom-find (h/lookup-key buf "C-x C-f"))
          "Local keymap should shadow global keymap"))
    ;; In a different buffer without local binding, global should work
    (let [buf2 (h/create-buffer "test2")]
      (is (= :find-file (h/lookup-key buf2 "C-x C-f"))
          "Global keymap should work when no local binding"))))

(deftest ^:critical prefix-key-waits-for-completion
  (testing "Emacs invariant: Prefix keys wait for completion before dispatching"
    (h/reset-editor-db!)
    ;; Set up prefix key (C-x) with multiple completions
    (h/set-global-key "C-x C-f" :find-file)
    (h/set-global-key "C-x C-s" :save-buffer)
    (h/set-global-key "C-x b" :switch-buffer)
    ;; Press just the prefix - should NOT dispatch yet
    (h/press-key-sequence "C-x")
    (is (h/in-prefix-state?) "After prefix key, should be waiting for completion")
    (is (nil? (h/last-invoked-command)) "Prefix alone should not invoke command")
    ;; Complete the sequence
    (h/press-key-sequence "C-f")
    (is (not (h/in-prefix-state?)) "After complete sequence, should exit prefix state")
    (is (= :find-file (h/last-invoked-command)) "Complete sequence should invoke command")))
