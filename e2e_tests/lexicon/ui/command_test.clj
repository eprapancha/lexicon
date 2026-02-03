(ns lexicon.ui.command-test
  "E2E tests for Emacs command system semantic - Epic #86

  Tests critical invariants:
  - Commands are inspectable entities with metadata
  - All command invocations go through the dispatcher"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-command-invocation-through-mx
  (testing "Emacs invariant: Commands can be invoked through M-x dispatcher"
    (h/setup-test*)

    ;; Invoke M-x to open command dispatcher
    (h/press-meta "x")
    (Thread/sleep 100)

    ;; Type a command name (fundamental-mode exists as a command)
    (h/type-in-minibuffer "fundamental-mode")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Command should have executed (no error in echo area)
    (let [echo-text (h/get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          (str "Command should execute without error, got: '" echo-text "'")))

    ;; Try another command - line-number-mode
    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "line-number-mode")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Should execute successfully
    (let [echo-text (h/get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          (str "Command should execute through M-x dispatcher, got: '" echo-text "'")))))

(deftest test-keybindings-invoke-commands
  (testing "Emacs invariant: Keybindings invoke commands through the same dispatcher"
    (h/setup-test*)

    ;; C-g invokes keyboard-quit command
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Should show "Quit" message (standard keyboard-quit behavior)
    (let [echo-text (h/get-echo-area-text)]
      (is (or (.contains echo-text "Quit")
              (.contains echo-text "quit"))
          (str "C-g should invoke keyboard-quit command, got: '" echo-text "'")))

    ;; C-x b invokes switch-to-buffer command (opens minibuffer)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    ;; Minibuffer should open (command was dispatched)
    (let [minibuf-visible (try
                            (e/visible? h/*driver* {:css ".minibuffer-input"})
                            (catch Exception _ false))]
      (is minibuf-visible
          "C-x b should invoke switch-to-buffer command through dispatcher"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 50)))
