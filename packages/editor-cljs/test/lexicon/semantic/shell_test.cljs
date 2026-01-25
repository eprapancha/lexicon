(ns lexicon.semantic.shell-test
  "Semantic tests for shell mode.

  Emacs source: lisp/shell.el (4,348 LOC), lisp/eshell/ (14,467 LOC)
  Status: 0% implemented

  Key features:
  - M-x shell opens shell buffer
  - Command execution with output
  - Directory tracking
  - Input/output handling

  Related: Issue #112 (Shell & Eshell), Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Shell Buffer
;;; =============================================================================

(deftest ^:high shell-opens-buffer
  "HIGH: M-x shell creates a shell buffer.

  Emacs Semantics (shell.el):
  - Creates buffer named *shell*
  - Starts inferior shell process
  - Enables shell-mode

  Why this matters:
  - Primary command-line interface"
  (testing "shell command creates buffer"
    (let [buf (helpers/shell)]
      (is (some? buf)
          "Shell buffer should be created")
      (is (= "*shell*" (helpers/buffer-name))
          "Buffer should be named *shell*"))))

(deftest ^:high shell-executes-commands
  "HIGH: Commands execute and output displays.

  Emacs Semantics:
  - RET sends input to shell
  - Output appears in buffer
  - Prompt displayed after command

  Why this matters:
  - Core shell functionality"
  (testing "command output appears"
    (with-test-buffer "*shell*"
      (helpers/shell-send-input "echo hello")
      ;; Output should contain 'hello'
      (is (clojure.string/includes? (helpers/buffer-string) "hello")
          "Command output should appear"))))

(deftest ^:medium shell-directory-tracking
  "MEDIUM: Shell tracks current directory.

  Emacs Semantics:
  - cd commands update default-directory
  - Prompt may show directory
  - File operations use tracked directory

  Why this matters:
  - find-file relative to shell dir"
  (testing "directory tracking works"
    (with-test-buffer "*shell*"
      (helpers/shell-send-input "cd /tmp")
      (is (= "/tmp" (helpers/default-directory))
          "Directory should be tracked"))))

;;; =============================================================================
;;; Shell Mode
;;; =============================================================================

(deftest ^:medium shell-mode-keybindings
  "MEDIUM: Shell mode has appropriate keybindings.

  Key bindings:
  - RET: send input
  - C-c C-c: interrupt
  - C-c C-z: stop
  - M-p/M-n: history navigation

  Why this matters:
  - Efficient shell interaction"
  (testing "shell mode bindings exist"
    (with-test-buffer "*shell*"
      (helpers/enable-major-mode :shell-mode)
      (is (some? (helpers/lookup-key "RET"))
          "RET should be bound"))))

(deftest ^:low shell-input-history
  "LOW: Shell maintains input history.

  Emacs Semantics:
  - M-p recalls previous input
  - M-n recalls next input
  - History persists across commands

  Why this matters:
  - Efficient command re-use"
  (testing "history navigation works"
    (with-test-buffer "*shell*"
      (helpers/shell-send-input "first")
      (helpers/shell-send-input "second")
      (helpers/shell-previous-input)
      (is (= "second" (helpers/shell-current-input))
          "Should recall previous input"))))
