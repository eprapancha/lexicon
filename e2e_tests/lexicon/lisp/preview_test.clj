(ns lexicon.lisp.preview-test
  "Lisp API tests for the preview framework.

  Tests preview state function constructors and lifecycle.

  JUSTIFICATION: Preview state functions are Lisp API entities that
  interact with window configuration, cursor position, and overlays.
  Testing the lifecycle requires Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Preview Constructors
;; =============================================================================

(deftest test-make-jump-preview-exists
  (testing "make-jump-preview returns a function"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp!
                  "(fn? (make-jump-preview (fn [c] (goto-char 0))))" )]
      (is (true? result) "make-jump-preview should return a function"))))

(deftest test-make-buffer-preview-exists
  (testing "make-buffer-preview returns a function"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp!
                  "(fn? (make-buffer-preview (fn [c] c)))" )]
      (is (true? result) "make-buffer-preview should return a function"))))

(deftest test-make-insertion-preview-exists
  (testing "make-insertion-preview returns a function"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp!
                  "(fn? (make-insertion-preview))" )]
      (is (true? result) "make-insertion-preview should return a function"))))

;; =============================================================================
;; Jump Preview Lifecycle
;; =============================================================================

(deftest test-jump-preview-setup-and-exit
  (testing "Jump preview saves and restores cursor position"
    (lisp/setup-test)
    ;; Insert some text and position cursor
    (lisp/eval-lisp! "(insert \"line one\\nline two\\nline three\")")
    (lisp/eval-lisp! "(goto-char 5)")
    (let [original-point (lisp/eval-lisp! "(point)")]
      ;; Create and invoke a jump preview
      (lisp/eval-lisp!
       "(setq my-preview (make-jump-preview (fn [c] (goto-char 0))))")
      ;; Setup phase
      (lisp/eval-lisp! "(my-preview 'setup nil)")
      ;; Preview should navigate away
      (lisp/eval-lisp! "(my-preview 'preview \"test\")")
      ;; Exit should restore position
      (lisp/eval-lisp! "(my-preview 'exit nil)")
      (Thread/sleep 100)
      (is (= original-point (lisp/eval-lisp! "(point)"))
          "Exit should restore original point"))))
