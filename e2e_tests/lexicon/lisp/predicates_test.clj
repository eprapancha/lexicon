(ns lexicon.lisp.predicates-test
  "E2E tests for Emacs-style type predicates.

  Tests functionp, commandp, fboundp, boundp."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; functionp - Test if object is callable
;; =============================================================================

(deftest test-functionp-with-lambda
  (testing "functionp returns true for lambda"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(functionp (fn [] 42))")]
      (is (= true result)))))

(deftest test-functionp-with-non-function
  (testing "functionp returns false for non-functions"
    (lisp/setup-test)
    (is (= false (lisp/eval-lisp! "(functionp 42)")))
    (is (= false (lisp/eval-lisp! "(functionp \"string\")")))
    (is (= false (lisp/eval-lisp! "(functionp nil)")))))

(deftest test-functionp-with-bound-symbol
  (testing "functionp returns true for symbol bound to function"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq my-fn (fn [x] (* x 2)))")
    (is (= true (lisp/eval-lisp! "(functionp 'my-fn)")))))

;; =============================================================================
;; commandp - Test if object is an interactive command
;; =============================================================================

(deftest test-commandp-with-builtin-command
  (testing "commandp returns true for built-in commands"
    (lisp/setup-test)
    ;; save-buffer is a built-in command
    (is (= true (lisp/eval-lisp! "(commandp 'save-buffer)")))))

(deftest test-commandp-with-non-command
  (testing "commandp returns nil for non-commands"
    (lisp/setup-test)
    ;; In Emacs, commandp returns nil (not false) for non-commands
    (is (nil? (lisp/eval-lisp! "(commandp 'nonexistent-command)")))
    (is (nil? (lisp/eval-lisp! "(commandp 42)")))
    (is (nil? (lisp/eval-lisp! "(commandp nil)")))))

(deftest test-commandp-with-keyword
  (testing "commandp works with keyword input"
    (lisp/setup-test)
    (is (= true (lisp/eval-lisp! "(commandp :save-buffer)")))))

;; =============================================================================
;; boundp - Test if symbol has a value
;; =============================================================================

(deftest test-boundp-with-bound-variable
  (testing "boundp returns true for bound variables"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq my-var 123)")
    (is (= true (lisp/eval-lisp! "(boundp 'my-var)")))))

(deftest test-boundp-with-unbound-variable
  (testing "boundp returns false for unbound variables"
    (lisp/setup-test)
    (is (= false (lisp/eval-lisp! "(boundp 'definitely-not-bound)")))))

(deftest test-boundp-with-nil-value
  (testing "boundp returns true even when value is nil"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq nil-var nil)")
    (is (= true (lisp/eval-lisp! "(boundp 'nil-var)")))))

;; =============================================================================
;; fboundp - Test if symbol has a function definition
;; =============================================================================

(deftest test-fboundp-with-defined-function
  (testing "fboundp returns true for defined functions"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq my-func (fn [] 42))")
    (is (= true (lisp/eval-lisp! "(fboundp 'my-func)")))))

(deftest test-fboundp-with-command
  (testing "fboundp returns true for commands"
    (lisp/setup-test)
    ;; Commands are also function-bound
    (is (= true (lisp/eval-lisp! "(fboundp 'save-buffer)")))))

(deftest test-fboundp-with-undefined
  (testing "fboundp returns nil for undefined symbols"
    (lisp/setup-test)
    (is (nil? (lisp/eval-lisp! "(fboundp 'undefined-function)")))))

;; =============================================================================
;; autoload - Lazy loading placeholder
;; =============================================================================

(deftest test-autoload-basic
  (testing "autoload registers a function for lazy loading"
    (lisp/setup-test)
    ;; autoload should return nil
    (is (nil? (lisp/eval-lisp! "(autoload 'my-autoloaded-fn \"my-module\")")))))

(deftest test-autoloadp-with-autoloaded
  (testing "autoloadp returns true for autoloaded symbols"
    (lisp/setup-test)
    (lisp/eval-lisp! "(autoload 'lazy-fn \"lazy-module\")")
    (is (= true (lisp/eval-lisp! "(autoloadp 'lazy-fn)")))))

(deftest test-autoloadp-with-regular-function
  (testing "autoloadp returns nil for non-autoloaded symbols"
    (lisp/setup-test)
    (is (nil? (lisp/eval-lisp! "(autoloadp 'regular-fn)")))
    (lisp/eval-lisp! "(setq regular-fn (fn [] 42))")
    (is (nil? (lisp/eval-lisp! "(autoloadp 'regular-fn)")))))

(deftest test-autoload-interactive
  (testing "autoload with interactive flag registers command"
    (lisp/setup-test)
    (lisp/eval-lisp! "(autoload 'my-command \"my-module\" \"My command\" t)")
    ;; Should be registered as a command
    (is (= true (lisp/eval-lisp! "(commandp 'my-command)")))))
