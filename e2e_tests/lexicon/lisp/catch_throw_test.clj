(ns lexicon.lisp.catch-throw-test
  "E2E tests for Emacs-style catch/throw* non-local exits.

  Unlike condition-case/signal (error handling), catch/throw* is for
  control flow - jumping out of nested computations.

  Note: We use throw* instead of throw to avoid shadowing Clojure's throw."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic catch/throw
;; =============================================================================

(deftest test-catch-no-throw
  (testing "catch returns body value when no throw"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'done (+ 1 2))")]
      (is (= 3 result)))))

(deftest test-catch-with-throw
  (testing "catch returns thrown value"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'done (throw* 'done 42))")]
      (is (= 42 result)))))

(deftest test-throw-exits-early
  (testing "throw exits immediately, skipping remaining code"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq reached-end nil)")
    (lisp/eval-lisp! "(catch* 'done
                        (throw* 'done \"early\")
                        (setq reached-end t))")
    (is (= nil (lisp/eval-lisp! "reached-end")))))

(deftest test-throw-from-nested-code
  (testing "throw exits from deeply nested code"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'found
                                     (dotimes [i 10]
                                       (when (= i 5)
                                         (throw* 'found i)))
                                     nil)")]
      (is (= 5 result)))))

;; =============================================================================
;; Tag matching
;; =============================================================================

(deftest test-throw-matches-correct-tag
  (testing "throw only matches catch with same tag"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'outer
                                     (catch* 'inner
                                       (throw* 'outer \"outer-value\")
                                       \"inner-result\")
                                     \"outer-result\")")]
      (is (= "outer-value" result)))))

(deftest test-nested-catch-same-tag
  (testing "throw matches nearest catch with same tag"
    (lisp/setup-test)
    ;; Inner catch* should catch the throw, so outer catch* returns normally
    ;; with the inner catch*'s result as the last evaluated form
    (let [result (lisp/eval-lisp! "(catch* 'tag
                                     (catch* 'tag
                                       (throw* 'tag \"inner\")))")]
      ;; Inner catch* caught the throw and returned "inner"
      ;; Outer catch* had normal completion, returning "inner"
      (is (= "inner" result)))))

;; =============================================================================
;; Integration with condition-case
;; =============================================================================

(deftest test-throw-propagates-through-condition-case
  (testing "throw propagates through condition-case (not caught as error)"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'done
                                     (condition-case nil
                                         (throw* 'done \"escaped\")
                                       (error \"caught-error\")))")]
      (is (= "escaped" result)))))

(deftest test-condition-case-still-catches-errors
  (testing "condition-case still catches real errors when catch/throw used"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'done
                                     (condition-case nil
                                         (error \"boom\")
                                       (error \"caught\")))")]
      (is (= "caught" result)))))

;; =============================================================================
;; Integration with unwind-protect
;; =============================================================================

(deftest test-throw-triggers-unwind-protect
  (testing "throw still runs unwind-protect cleanup"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq cleanup-ran nil)")
    (lisp/eval-lisp! "(catch* 'done
                        (unwind-protect
                            (throw* 'done \"value\")
                          (setq cleanup-ran t)))")
    (is (= true (lisp/eval-lisp! "cleanup-ran")))))

;; =============================================================================
;; Edge cases
;; =============================================================================

(deftest test-throw-nil-value
  (testing "throw can return nil"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(catch* 'done (throw* 'done nil))")]
      (is (= nil result)))))

(deftest test-catch-multiple-forms-in-body
  (testing "catch evaluates multiple body forms"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq x 0)")
    (let [result (lisp/eval-lisp! "(catch* 'done
                                     (setq x 1)
                                     (setq x 2)
                                     x)")]
      (is (= 2 result))
      (is (= 2 (lisp/eval-lisp! "x"))))))
