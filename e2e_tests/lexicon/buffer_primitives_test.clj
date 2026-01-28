(ns lexicon.buffer-primitives-test
  "E2E tests for buffer primitives - user-visible buffer behavior.

  Note: Buffer APIs (get-buffer-create, narrowing, buffer-local-value, etc.)
  are Lisp functions. E2E tests focus on user-visible behavior like typing
  updates the modified flag and undo works on typed text.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Buffer Modification - USER TYPING
;; =============================================================================

(deftest test-buffer-modified-on-user-typing
  (testing "Buffer becomes modified when user types"
    (h/setup-test*)
    (h/clear-buffer)

    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 100)

    (is (= "Hello" (h/get-buffer-text*))
        "User typing should update buffer")))

(deftest test-undo-after-user-typing
  (testing "Undo works on user-typed text"
    (h/setup-test*)
    (h/clear-buffer)

    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Undo via keyboard (C-/ is Emacs undo)
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Undo should remove user-typed text")))

(deftest test-user-cursor-movement
  (testing "User can move cursor"
    (h/setup-test*)
    (h/clear-buffer)

    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    (is (= 0 (h/get-point*))
        "Ctrl+A should move to beginning")))

;; =============================================================================
;; Buffer Lisp API Tests
;; =============================================================================
;;
;; Note: Buffer Lisp API tests (buffer-modified-p, erase-buffer, narrowing, etc.)
;; are in sci_integration_test.clj with other Lisp API tests.
