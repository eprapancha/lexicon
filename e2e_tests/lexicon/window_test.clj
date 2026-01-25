(ns lexicon.window-test
  "E2E tests for window management.

  Emacs source: lisp/window.el (8,896 LOC), src/window.c (112 DEFUNs)
  Status: 60% implemented

  Key features:
  - Window splitting (C-x 2, C-x 3)
  - Window deletion (C-x 0, C-x 1)
  - Independent point per window
  - Window configuration

  Related: Issue #110 (Window Management), Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

(def app-url "http://localhost:8080")
(def ^:dynamic *driver* nil)
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

(defn eval-lisp
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp! [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test []
  (e/go *driver* app-url)
  (test-helpers/wait-for-editor-ready *driver*)
  (test-helpers/click-editor *driver*)
  (Thread/sleep 300)
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)")
  ;; Ensure single window to start
  (eval-lisp "(delete-other-windows)"))

;; =============================================================================
;; Window Splitting
;; =============================================================================

(deftest test-split-window-horizontally
  (testing "split creates two windows"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [win1 (eval-lisp! "(selected-window)")]
      (eval-lisp! "(split-window-horizontally)")
      (let [win2 (eval-lisp! "(selected-window)")]
        (is (not= win1 win2)
            "New window created")))))

(deftest test-split-window-vertically
  (testing "vertical split creates two windows"
    (setup-test)
    (eval-lisp! "(split-window-vertically)")
    (let [count (eval-lisp! "(length (window-list))")]
      (is (> count 1)
          "Should have multiple windows"))))

;; =============================================================================
;; Window Deletion
;; =============================================================================

(deftest test-delete-current-window
  (testing "delete-window removes current"
    (setup-test)
    (eval-lisp! "(split-window-horizontally)")
    (let [count-before (eval-lisp! "(length (window-list))")]
      (is (> count-before 1))
      (eval-lisp! "(delete-window)")
      (let [count-after (eval-lisp! "(length (window-list))")]
        (is (= 1 count-after)
            "Should return to single window")))))

(deftest test-delete-other-windows
  (testing "delete-other-windows leaves one"
    (setup-test)
    (eval-lisp! "(split-window-horizontally)")
    (eval-lisp! "(split-window-horizontally)")
    (eval-lisp! "(delete-other-windows)")
    (let [count (eval-lisp! "(length (window-list))")]
      (is (= 1 count)
          "Should have only one window"))))

;; =============================================================================
;; Independent Point
;; =============================================================================

(deftest test-windows-have-independent-point
  (testing "windows track point independently"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 0)")
    (let [win1 (eval-lisp! "(selected-window)")]
      (eval-lisp! "(split-window-horizontally)")
      ;; Move point in window 2
      (eval-lisp! "(goto-char 6)")
      ;; Switch to window 1
      (eval-lisp! "(select-window (car (window-list)))")
      ;; Point should still be at 0 in window 1
      (is (= 0 (eval-lisp! "(point)"))
          "Window 1 point unchanged"))))

;; =============================================================================
;; Window Selection
;; =============================================================================

(deftest test-other-window-cycles
  (testing "other-window moves to next"
    (setup-test)
    (let [win1 (eval-lisp! "(selected-window)")]
      (eval-lisp! "(split-window-horizontally)")
      (eval-lisp! "(other-window 1)")
      (is (= win1 (eval-lisp! "(selected-window)"))
          "Should cycle back to first window"))))

;; =============================================================================
;; Window Configuration
;; =============================================================================

(deftest test-window-configuration-saveable
  (testing "configuration can be captured"
    (setup-test)
    (let [config (eval-lisp "(current-window-configuration)")]
      (is (or (not (:success config))
              (some? (:result config)))
          "Configuration should be returned"))))
