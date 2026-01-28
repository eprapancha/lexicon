(ns lexicon.lisp.error-handling-test
  "E2E tests for Emacs-style error handling primitives.

  Tests condition-case, signal, error, and unwind-protect.
  These are critical for robust package loading and error recovery."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; signal and error functions
;; =============================================================================

(deftest test-signal-raises-error
  (testing "signal raises an error that can be caught"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(condition-case nil (signal 'test-error \"test message\") (test-error \"caught\"))")]
      (is (:success result))
      (is (= "caught" (:result result))))))

(deftest test-error-function
  (testing "error function raises error with 'error symbol"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(condition-case nil (error \"something went wrong\") (error \"caught\"))")]
      (is (:success result))
      (is (= "caught" (:result result))))))

;; =============================================================================
;; condition-case basics
;; =============================================================================

(deftest test-condition-case-no-error
  (testing "condition-case returns bodyform result when no error"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(condition-case err (+ 1 2) (error \"should not run\"))")]
      (is (= 3 result)))))

(deftest test-condition-case-catches-error
  (testing "condition-case catches matching error type"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(condition-case err
                                     (signal 'file-error \"not found\")
                                   (file-error \"file error caught\"))")]
      (is (= "file error caught" result)))))

(deftest test-condition-case-error-data-binding
  (testing "condition-case binds error data to var"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(condition-case err
                                     (signal 'my-error \"the message\")
                                   (my-error (second err)))")]
      (is (= "the message" result)))))

(deftest test-condition-case-multiple-handlers
  (testing "condition-case tries handlers in order"
    (lisp/setup-test)
    ;; file-error handler should match
    (let [result (lisp/eval-lisp! "(condition-case err
                                     (signal 'file-error \"test\")
                                   (network-error \"network\")
                                   (file-error \"file\")
                                   (error \"generic\"))")]
      (is (= "file" result)))
    ;; generic error handler should match for unknown error
    (let [result (lisp/eval-lisp! "(condition-case err
                                     (signal 'unknown-error \"test\")
                                   (file-error \"file\")
                                   (error \"generic\"))")]
      (is (= "generic" result)))))

(deftest test-condition-case-nil-var
  (testing "condition-case with nil var still catches errors"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(condition-case nil
                                     (error \"boom\")
                                   (error \"caught without binding\"))")]
      (is (= "caught without binding" result)))))

;; =============================================================================
;; unwind-protect
;; =============================================================================

(deftest test-unwind-protect-normal-exit
  (testing "unwind-protect runs cleanup on normal exit"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq cleanup-ran nil)")
    (lisp/eval-lisp! "(unwind-protect
                        (+ 1 2)
                      (setq cleanup-ran t))")
    (is (= true (lisp/eval-lisp! "cleanup-ran")))))

(deftest test-unwind-protect-on-error
  (testing "unwind-protect runs cleanup even on error"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq cleanup-ran nil)")
    (lisp/eval-lisp "(condition-case nil
                       (unwind-protect
                           (error \"boom\")
                         (setq cleanup-ran t))
                     (error nil))")
    (is (= true (lisp/eval-lisp! "cleanup-ran")))))

(deftest test-unwind-protect-returns-body-value
  (testing "unwind-protect returns body value, not cleanup value"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(unwind-protect
                                    \"body-result\"
                                  \"cleanup-result\")")]
      (is (= "body-result" result)))))

;; =============================================================================
;; Integration: condition-case + unwind-protect
;; =============================================================================

(deftest test-condition-case-with-unwind-protect
  (testing "condition-case and unwind-protect work together"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq resource-released nil)")
    (let [result (lisp/eval-lisp! "(condition-case err
                                     (unwind-protect
                                         (do
                                           (signal 'my-error \"test\")
                                           \"should not reach\")
                                       (setq resource-released t))
                                   (my-error \"error handled\"))")]
      (is (= "error handled" result))
      (is (= true (lisp/eval-lisp! "resource-released"))))))
