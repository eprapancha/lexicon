(ns lexicon.project-test
  "E2E tests for project management and cross-reference - user-visible behavior.

  Emacs source: lisp/progmodes/project.el, lisp/progmodes/xref.el

  Note: project.el and xref are Lisp APIs. E2E tests focus on
  user-visible behavior. API-specific tests are pending E2E implementation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Code Typing for xref Context
;; =============================================================================

(deftest test-user-types-code-with-definitions
  (testing "User can type code with definitions for xref"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types code with definitions
    (h/type-text "(defn my-func [] nil)")
    (h/press-key "Enter")
    (h/type-text "(my-func)")
    (Thread/sleep 100)

    (is (= "(defn my-func [] nil)\n(my-func)"
           (h/get-buffer-text*))
        "User can type code with definitions")))

;; =============================================================================
;; Project Root Detection - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-project-root-detection
  (testing "git repo detected as project"
    ;; project-root is a Lisp function
    (is true "PENDING: project-root - needs E2E implementation")))

;; =============================================================================
;; Project Find File - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-project-find-file
  (testing "project-find-file lists project files"
    ;; project-find-file is a Lisp function
    (is true "PENDING: project-find-file - needs E2E implementation")))

;; =============================================================================
;; Xref - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-xref-find-definitions
  (testing "xref finds definition"
    ;; xref-find-definitions is a Lisp function
    (is true "PENDING: xref-find-definitions - needs E2E implementation")))

(deftest ^:skip test-xref-find-references
  (testing "xref finds references"
    ;; xref-find-references is a Lisp function
    (is true "PENDING: xref-find-references - needs E2E implementation")))
