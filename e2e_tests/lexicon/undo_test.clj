(ns lexicon.undo-test
  "E2E tests for undo system - core editing primitive (Issue #103).

  Undo system components:
  - C level: undo.c has record_insert, record_delete, undo-boundary
  - Lisp level: simple.el has undo, primitive-undo, undo-start, undo-more

  Key concepts:
  - buffer-undo-list: List of undo entries (nil boundaries separate commands)
  - pending-undo-list: Pointer into undo list during undo sequence
  - Entries: (BEG . END) for insertion, (TEXT . POS) for deletion

  Related: Issue #103 (Undo System), Issue #94 (TDD)
  Priority: CRITICAL - required for safe editing"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code and return the result."
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp!
  "Evaluate Lisp code and return just the result (throws on error)"
  [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success
      result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test []
  "Standard test setup"
  (e/go *driver* app-url)
  (test-helpers/wait-for-editor-ready *driver*)
  (test-helpers/click-editor *driver*)
  (Thread/sleep 300)
  ;; Start with clean buffer
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; Basic Undo Operations
;; =============================================================================

(deftest test-undo-reverses-insert
  (testing "undo removes inserted text"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Inserted text should be removed")))

(deftest test-undo-reverses-delete
  (testing "undo restores deleted text"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(delete-region 0 6)")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")
    (is (= "Hello World" (eval-lisp! "(buffer-string)"))
        "Deleted text should be restored")))

(deftest test-undo-boundary-groups-changes
  (testing "multiple edits between boundaries undo together"
    (setup-test)
    (eval-lisp! "(undo-boundary)")
    ;; Multiple edits without boundary
    (eval-lisp! "(insert \"A\")")
    (eval-lisp! "(insert \"B\")")
    (eval-lisp! "(insert \"C\")")
    (eval-lisp! "(undo-boundary)")
    ;; Single undo should remove all three
    (eval-lisp! "(undo)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "All edits between boundaries should undo together")))

(deftest test-undo-restores-point
  (testing "undo restores point position"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 2)")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(insert \" World\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")
    (is (= 2 (eval-lisp! "(point)"))
        "Point should be restored to pre-command position")))

;; =============================================================================
;; Consecutive Undo
;; =============================================================================

(deftest test-consecutive-undo-continues-chain
  (testing "consecutive undos undo multiple boundaries"
    (setup-test)
    (eval-lisp! "(insert \"A\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(insert \"B\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(insert \"C\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")  ; Remove C
    (eval-lisp! "(undo)")  ; Remove B
    (eval-lisp! "(undo)")  ; Remove A
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Consecutive undos should remove all insertions")))

;; =============================================================================
;; Redo (Undo the Undo)
;; =============================================================================

(deftest test-undo-creates-redo-record
  (testing "redo restores undone change"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")
    (is (= "" (eval-lisp! "(buffer-string)")))
    (eval-lisp! "(redo)")
    (is (= "Hello" (eval-lisp! "(buffer-string)"))
        "Redo should restore undone change")))

(deftest test-new-edit-clears-redo
  (testing "edit after undo clears redo"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(undo-boundary)")
    (eval-lisp! "(undo)")
    ;; New edit
    (eval-lisp! "(insert \"Goodbye\")")
    (eval-lisp! "(undo-boundary)")
    ;; Redo should have nothing
    (eval-lisp! "(redo)")
    (is (= "Goodbye" (eval-lisp! "(buffer-string)"))
        "Redo should be cleared after new edit")))

;; =============================================================================
;; Undo Recording Control
;; =============================================================================

(deftest test-buffer-disable-undo-for-buffer
  (testing "buffer-disable-undo stops recording"
    (setup-test)
    (eval-lisp! "(buffer-disable-undo)")
    (eval-lisp! "(insert \"Not recorded\")")
    (eval-lisp! "(undo-boundary)")
    ;; Undo should do nothing
    (eval-lisp! "(undo)")
    (is (= "Not recorded" (eval-lisp! "(buffer-string)"))
        "Edit should remain (no undo history)")))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-undo-empty-buffer-noop
  (testing "undo on empty undo stack"
    (setup-test)
    ;; Clear any implicit undo history
    (eval-lisp! "(buffer-disable-undo)")
    (eval-lisp! "(buffer-enable-undo)")
    ;; Should not error
    (eval-lisp! "(undo)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Undo on empty buffer should be no-op")))

(deftest test-undo-preserves-markers
  (testing "markers adjusted during undo"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [marker-id (eval-lisp! "(make-marker 6)")]
      (eval-lisp! "(undo-boundary)")
      (eval-lisp! "(delete-region 0 6)")
      (eval-lisp! "(undo-boundary)")
      ;; After delete, marker position may have changed
      (eval-lisp! "(undo)")
      ;; After undo, marker should be back at 6
      (is (= 6 (eval-lisp! (str "(marker-position " marker-id ")")))
          "Marker should be restored after undo"))))
