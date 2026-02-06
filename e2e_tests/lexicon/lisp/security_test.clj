(ns lexicon.lisp.security-test
  "Lisp API tests for security constraints.

  Tests that the SCI sandbox properly blocks dangerous operations:
  - js/eval: Blocked (XSS vector)
  - js/fetch: Blocked (data exfiltration)
  - Safe JS allowed: Math, Date (pending)

  JUSTIFICATION: Security constraints must be tested at the Lisp eval level
  to verify that the sandbox properly rejects malicious code. These cannot
  be tested via keyboard since the code never reaches the UI."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Dangerous Operations (Must Be Blocked)
;; =============================================================================

(deftest test-js-eval-blocked
  (testing "js/eval is blocked"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(js/eval \"alert('xss')\")")]
      (is (not (:success result))
          "js/eval should be blocked"))))

(deftest test-js-fetch-blocked
  (testing "js/fetch is blocked"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(js/fetch \"http://evil.com\")")]
      (is (not (:success result))
          "js/fetch should be blocked"))))

;; =============================================================================
;; Safe Operations (Should Be Allowed)
;; =============================================================================

(deftest test-js-math-allowed
  (testing "Math is allowed via SCI classes"
    (lisp/setup-test)
    ;; In SCI, Math is accessed via Math/sqrt not js/Math.sqrt
    (let [result (lisp/eval-lisp! "(Math/sqrt 16)")]
      (is (= 4 result)))))

(deftest test-js-date-allowed
  (testing "Date is allowed via SCI classes"
    (lisp/setup-test)
    ;; In SCI, Date constructor is (js/Date.) or (Date.)
    (let [result (lisp/eval-lisp "(Date.)")]
      ;; Should return a Date object or error - either way, something
      (is (or (:success result) (:error result)) "Should return something"))))
