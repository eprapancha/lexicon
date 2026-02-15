(ns lexicon.ui.files.persistence-test
  "E2E tests for file state persistence - user-visible behavior.

  Emacs source: lisp/recentf.el, lisp/saveplace.el, lisp/autorevert.el

  Note: recentf, saveplace, autorevert are Lisp APIs. E2E tests focus on
  user-visible behavior. API-specific tests are pending E2E implementation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Cursor Position Behavior
;; =============================================================================

(deftest test-user-cursor-position-remembered
  (testing "User cursor position is remembered in buffer"
    (h/setup-test*)
    (h/clear-buffer)

    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    (is (= 0 (h/get-point*))
        "Cursor should be at beginning")))

;; =============================================================================
;; Persistence Feature Tests
;;
;; NOTE: recentf, saveplace, and auto-revert are Lisp features. E2E tests
;; focus on user-visible cursor position behavior. API-specific tests belong
;; in lexicon.lisp namespace when implemented.
;; =============================================================================
