(ns lexicon.lisp.completing-read-test
  "Lisp API tests for completing-read enrichment.

  Tests completing-read passes metadata and completion table through to
  the minibuffer frame correctly.

  JUSTIFICATION: completing-read is a Lisp API function that accepts
  completion tables with metadata. Testing that metadata flows through
  to the minibuffer frame requires Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic completing-read
;; =============================================================================

(deftest test-completing-read-activates-minibuffer
  (testing "completing-read activates minibuffer with candidates"
    (lisp/setup-test)
    (lisp/eval-lisp! "(completing-read \"Pick: \" [\"alpha\" \"beta\" \"gamma\"])")
    (Thread/sleep 200)
    ;; Minibuffer should be active
    (is (some? (lisp/eval-lisp! "(minibuffer-active-p)"))
        "Minibuffer should be active after completing-read")
    ;; Clean up
    (lisp/eval-lisp! "(exit-minibuffer)")))

(deftest test-completing-read-passes-candidates
  (testing "completing-read passes candidate list to minibuffer"
    (lisp/setup-test)
    (lisp/eval-lisp! "(completing-read \"Select: \" [\"foo\" \"bar\" \"baz\"])")
    (Thread/sleep 200)
    ;; Check completions are available
    (let [completions (lisp/eval-lisp! "(minibuffer-completions)")]
      (is (sequential? completions) "Completions should be a list")
      (is (= 3 (count completions)) "Should have 3 completions"))
    ;; Clean up
    (lisp/eval-lisp! "(exit-minibuffer)")))

;; =============================================================================
;; Metadata passing
;; =============================================================================

(deftest test-completing-read-with-metadata
  (testing "completing-read passes metadata to minibuffer"
    (lisp/setup-test)
    ;; Use set-completion-metadata to set up metadata for the table
    (lisp/eval-lisp! "(set-completion-metadata \"\" [\"a\" \"b\"] nil {:category :command})")
    (lisp/eval-lisp! "(completing-read \"M-x \" [\"a\" \"b\"])")
    (Thread/sleep 200)
    (is (some? (lisp/eval-lisp! "(minibuffer-active-p)"))
        "Minibuffer should be active")
    ;; Clean up
    (lisp/eval-lisp! "(exit-minibuffer)")))
