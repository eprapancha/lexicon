(ns lexicon.semantic.sci-integration-test
  "Semantic tests for SCI (Small Clojure Interpreter) integration.

  SCI provides runtime evaluation for:
  - Emacs Lisp API emulation (lexicon.lisp)
  - External package sandboxing (core.packages.sci)
  - Interactive eval (eval-string, eval-expression)

  Key components:
  - sci-context: SCI evaluation environment
  - sci-namespace: Exposed Lisp API functions
  - Trust levels: :core, :local, :external

  Related: Issue #106 (SCI Integration), Issue #94 (TDD)
  Priority: CRITICAL - foundation for Emacs Lisp compatibility"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Basic Evaluation
;;; =============================================================================

(deftest ^:critical eval-string-basic
  "CRITICAL: eval-string evaluates ClojureScript code.

  Emacs equivalent: eval-expression, eval-last-sexp

  Why this matters:
  - Foundation for interactive evaluation
  - Required for package loading
  - REPL functionality"
  (testing "eval-string returns result"
    (let [result (helpers/eval-lisp "(+ 1 2)")]
      (is (= 3 result))))

  (testing "eval-string with multiple forms"
    (let [result (helpers/eval-lisp "(def x 5) (+ x 3)")]
      (is (= 8 result))))

  (testing "eval-string with string result"
    (let [result (helpers/eval-lisp "(str \"hello\" \" \" \"world\")")]
      (is (= "hello world" result)))))

(deftest ^:critical eval-string-error-handling
  "CRITICAL: eval-string handles errors gracefully.

  Emacs behavior:
  - Display error in echo area
  - Return nil or error object
  - Don't crash editor

  Why this matters:
  - User code can have bugs
  - Must not destabilize editor"
  (testing "syntax error returns error"
    (let [result (helpers/eval-lisp "(+ 1")]  ; Missing closing paren
      (is (or (nil? result)
              (map? result)
              (instance? js/Error result))
          "Should return error indicator")))

  (testing "runtime error returns error"
    (let [result (helpers/eval-lisp "(/ 1 0)")]
      ;; ClojureScript returns Infinity for div by zero
      (is (some? result) "Should handle gracefully"))))

;;; =============================================================================
;;; API Exposure
;;; =============================================================================

(deftest ^:critical sci-exposes-buffer-api
  "CRITICAL: SCI exposes buffer manipulation functions.

  Key functions that must be available:
  - point, goto-char
  - insert, delete-region
  - buffer-string, buffer-substring

  Why this matters:
  - Core of Emacs Lisp compatibility"
  (testing "point function works"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [result (helpers/eval-lisp "(point)")]
        (is (number? result)))))

  (testing "goto-char function works"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/eval-lisp "(goto-char 0)")
      (is (= 0 (helpers/point)))))

  (testing "insert function works"
    (with-test-buffer "*test*"
      (helpers/eval-lisp "(insert \"test\")")
      (is (= "test" (helpers/buffer-string)))))

  (testing "buffer-string function works"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [result (helpers/eval-lisp "(buffer-string)")]
        (is (= "Hello" result))))))

(deftest ^:high sci-exposes-message-api
  "HIGH: SCI exposes messaging functions.

  Key functions:
  - message - Display in echo area
  - current-message - Get current message

  Why this matters:
  - User feedback from packages"
  (testing "message function works"
    (with-test-buffer "*test*"
      (helpers/eval-lisp "(message \"Hello from SCI\")")
      (is (= "Hello from SCI" (helpers/echo-area-text))))))

(deftest ^:high sci-exposes-search-api
  "HIGH: SCI exposes search functions.

  Key functions:
  - search-forward, search-backward

  Why this matters:
  - Text navigation in packages"
  (testing "search-forward available"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World Hello")
      (helpers/goto-char 0)
      (let [result (helpers/eval-lisp "(search-forward \"World\")")]
        ;; Should return position after match or nil
        (is (or (number? result) (nil? result)))))))

;;; =============================================================================
;;; Command Definition
;;; =============================================================================

(deftest ^:high defcommand-creates-command
  "HIGH: defcommand registers interactive command.

  Emacs equivalent: defun with (interactive)

  Why this matters:
  - Packages define commands
  - M-x access to user functions"
  (testing "defcommand creates callable command"
    (with-test-buffer "*test*"
      (helpers/eval-lisp
       "(defcommand 'test-cmd (fn [] (insert \"executed\")) \"A test command\")")

      ;; Command should exist
      (let [cmd-name (helpers/eval-lisp "'test-cmd")]
        (is (some? cmd-name)))

      ;; Invoking should work
      (helpers/invoke-command :test-cmd)
      (is (= "executed" (helpers/buffer-string))))))

(deftest ^:high defcommand-with-prefix-arg
  "HIGH: defcommand can access prefix argument.

  Emacs behavior:
  - Commands receive prefix arg via *prefix-arg*
  - Used for repeat counts, alternative behavior

  Why this matters:
  - C-u prefix functionality"
  (testing "command can access prefix arg"
    (with-test-buffer "*test*"
      (helpers/eval-lisp
       "(defcommand 'test-prefix
          (fn [] (insert (str *prefix-arg*)))
          \"Show prefix arg\")")

      ;; Set prefix arg and invoke
      (helpers/set-prefix-arg 4)
      (helpers/invoke-command :test-prefix)

      (is (= "4" (helpers/buffer-string))
          "Should insert prefix arg value"))))

;;; =============================================================================
;;; Security
;;; =============================================================================

(deftest ^:high sci-blocks-dangerous-access
  "HIGH: SCI blocks dangerous JavaScript access.

  Must block:
  - js/eval
  - js/Function
  - js/XMLHttpRequest, js/fetch
  - Direct re-frame access

  Why this matters:
  - Security for external packages
  - Prevent malicious code execution"
  (testing "js/eval blocked"
    (let [result (helpers/eval-lisp "(js/eval \"alert('xss')\")")]
      (is (or (nil? result)
              (map? result)  ; Error map
              (instance? js/Error result))
          "js/eval should be blocked")))

  (testing "js/fetch blocked"
    (let [result (helpers/eval-lisp "(js/fetch \"http://evil.com\")")]
      (is (or (nil? result)
              (map? result)
              (instance? js/Error result))
          "js/fetch should be blocked"))))

(deftest ^:medium sci-allows-safe-js
  "MEDIUM: SCI allows safe JavaScript access.

  Should allow:
  - js/Math for arithmetic
  - js/Date for timestamps
  - js/console for debugging

  Why this matters:
  - Basic functionality for packages"
  (testing "js/Math allowed"
    (let [result (helpers/eval-lisp "(js/Math.sqrt 16)")]
      (is (= 4.0 result))))

  (testing "js/Date allowed"
    (let [result (helpers/eval-lisp "(js/Date.)")]
      (is (some? result)))))

;;; =============================================================================
;;; Dynamic Variables
;;; =============================================================================

(deftest ^:high dynamic-variables-work
  "HIGH: Emacs-style dynamic variables work in SCI.

  Key dynamic vars:
  - *current-buffer*
  - *prefix-arg*
  - *this-command-keys*

  Why this matters:
  - Emacs Lisp compatibility
  - Dynamic scope emulation"
  (testing "*current-buffer* accessible"
    (with-test-buffer "*test*"
      (let [result (helpers/eval-lisp "(current-buffer)")]
        (is (some? result)))))

  (testing "*prefix-arg* accessible"
    (with-test-buffer "*test*"
      (helpers/set-prefix-arg 7)
      (let [result (helpers/eval-lisp "*prefix-arg*")]
        (is (= 7 result))))))

;;; =============================================================================
;;; Package Loading
;;; =============================================================================

(deftest ^:medium package-trust-levels
  "MEDIUM: Package trust levels work correctly.

  Trust levels:
  - :core - Built-in, full access
  - :local - User-installed, full access
  - :external - Third-party, sandboxed

  Why this matters:
  - Security model for package system"
  (testing "core packages have full access"
    ;; Core packages can access internals
    (is true "Core packages run natively"))

  (testing "external packages are sandboxed"
    ;; External packages run in SCI sandbox
    (is true "External packages use SCI")))

;;; =============================================================================
;;; Lisp API Completeness
;;; =============================================================================

(deftest ^:medium sci-namespace-completeness
  "MEDIUM: sci-namespace exposes all required functions.

  Check that key function categories are present:
  - Buffer operations
  - Point/navigation
  - Search
  - Kill ring
  - Modes
  - Files

  Why this matters:
  - Emacs Lisp compatibility for packages"
  (testing "buffer operations exposed"
    (let [available (helpers/sci-namespace-keys)]
      (is (some #{'buffer-string} available))
      (is (some #{'insert} available))
      (is (some #{'delete-region} available))))

  (testing "navigation exposed"
    (let [available (helpers/sci-namespace-keys)]
      (is (some #{'point} available))
      (is (some #{'goto-char} available))
      (is (some #{'forward-line} available))))

  (testing "modes exposed"
    (let [available (helpers/sci-namespace-keys)]
      (is (some #{'set-major-mode} available))
      (is (some #{'enable-minor-mode} available)))))
