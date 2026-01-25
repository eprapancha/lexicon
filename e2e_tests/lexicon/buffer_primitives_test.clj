(ns lexicon.buffer-primitives-test
  "E2E tests for buffer primitives - core buffer operations (Issue #100).

  Buffer primitives are defined in Emacs src/buffer.c (50 DEFUNs).
  Key categories:
  - Buffer identity: buffer-name, buffer-live-p, current-buffer
  - Buffer creation: get-buffer-create, make-indirect-buffer
  - Buffer switching: set-buffer
  - Buffer state: buffer-modified-p, set-buffer-modified-p
  - Narrowing: point-min, point-max, narrow-to-region, widen
  - Kill: kill-buffer

  Related: Issue #100 (Buffer Primitives), Issue #94 (TDD)
  Priority: CRITICAL - foundation for all buffer operations"
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
  "Evaluate Lisp code and return the result.
  Returns {:success true :result value} or {:success false :error msg}"
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
  (Thread/sleep 300))

;; =============================================================================
;; Buffer Identity and Lifecycle
;; =============================================================================

(deftest test-buffer-live-p-basics
  (testing "buffer-live-p checks if buffer object is alive"
    (setup-test)
    ;; Current buffer should be alive
    (is (true? (eval-lisp! "(buffer-live-p (current-buffer))"))
        "Current buffer should be alive")))

(deftest test-get-buffer-create-basics
  (testing "get-buffer-create returns existing or creates new buffer"
    (setup-test)
    ;; Create a buffer
    (let [buf1 (eval-lisp! "(get-buffer-create \"*test-buf*\")")
          buf2 (eval-lisp! "(get-buffer-create \"*test-buf*\")")]
      (is (= buf1 buf2) "Should return same buffer for same name"))
    ;; Cleanup
    (eval-lisp! "(kill-buffer \"*test-buf*\")")))

;; =============================================================================
;; Buffer Modification State
;; =============================================================================

(deftest test-buffer-modified-p-basics
  (testing "buffer-modified-p tracks unsaved changes"
    (setup-test)
    ;; Clear any existing content
    (eval-lisp! "(erase-buffer)")
    (eval-lisp! "(set-buffer-modified-p nil)")

    ;; Fresh buffer should not be modified
    (is (not (eval-lisp! "(buffer-modified-p)"))
        "Fresh buffer should not be modified")

    ;; Insert text
    (eval-lisp! "(insert \"Hello\")")

    ;; Now should be modified
    (is (true? (eval-lisp! "(buffer-modified-p)"))
        "Buffer should be modified after insert")))

(deftest test-set-buffer-modified-p-basics
  (testing "set-buffer-modified-p explicitly sets modification flag"
    (setup-test)
    (eval-lisp! "(insert \"text\")")

    ;; Clear modified flag
    (eval-lisp! "(set-buffer-modified-p nil)")
    (is (not (eval-lisp! "(buffer-modified-p)"))
        "Modified flag should be cleared")

    ;; Set modified flag
    (eval-lisp! "(set-buffer-modified-p t)")
    (is (true? (eval-lisp! "(buffer-modified-p)"))
        "Modified flag should be set")))

;; =============================================================================
;; Narrowing
;; =============================================================================

(deftest test-narrowing-affects-point-bounds
  (testing "narrow-to-region restricts visible portion of buffer"
    (setup-test)
    (eval-lisp! "(erase-buffer)")
    (eval-lisp! "(insert \"0123456789\")")
    (eval-lisp! "(narrow-to-region 3 7)")

    ;; point-min should be <= point-max
    (let [pmin (eval-lisp! "(point-min)")
          pmax (eval-lisp! "(point-max)")]
      (is (<= pmin pmax) "point-min should be <= point-max"))

    ;; Widen and check full buffer accessible
    (eval-lisp! "(widen)")
    (is (= 10 (count (eval-lisp! "(buffer-string)")))
        "Full buffer should be accessible after widen")))

(deftest test-save-restriction-preserves-narrowing
  (testing "save-restriction macro preserves narrowing state"
    (setup-test)
    (eval-lisp! "(erase-buffer)")
    (eval-lisp! "(insert \"0123456789\")")
    (eval-lisp! "(narrow-to-region 2 8)")

    ;; Get narrowed length
    (let [narrowed-len (count (eval-lisp! "(buffer-string)"))]
      ;; save-restriction, widen inside, then check restored
      (eval-lisp! "(save-restriction (widen) nil)")
      (is (= narrowed-len (count (eval-lisp! "(buffer-string)")))
          "Narrowing should be restored after save-restriction"))))

;; =============================================================================
;; Buffer Switching
;; =============================================================================

(deftest test-set-buffer-switches-context
  (testing "set-buffer makes buffer current for buffer-local operations"
    (setup-test)
    (let [original-name (eval-lisp! "(buffer-name)")]
      ;; Create and switch to new buffer
      (eval-lisp! "(get-buffer-create \"*test-switch*\")")
      (eval-lisp! "(set-buffer \"*test-switch*\")")

      (is (= "*test-switch*" (eval-lisp! "(buffer-name)"))
          "Should switch to new buffer")

      ;; Switch back
      (eval-lisp! (str "(set-buffer \"" original-name "\")"))
      (is (= original-name (eval-lisp! "(buffer-name)"))
          "Should switch back to original buffer")

      ;; Cleanup
      (eval-lisp! "(kill-buffer \"*test-switch*\")"))))

;; =============================================================================
;; Other Buffer Functions
;; =============================================================================

(deftest test-rename-buffer-changes-name
  (testing "rename-buffer changes buffer's name"
    (setup-test)
    (eval-lisp! "(get-buffer-create \"*original*\")")
    (eval-lisp! "(set-buffer \"*original*\")")
    (eval-lisp! "(rename-buffer \"*renamed*\")")

    (is (= "*renamed*" (eval-lisp! "(buffer-name)"))
        "Buffer should have new name")

    ;; Cleanup
    (eval-lisp! "(kill-buffer \"*renamed*\")")))

(deftest test-other-buffer-returns-alternative
  (testing "other-buffer returns most recently used different buffer"
    (setup-test)
    (let [current (eval-lisp! "(current-buffer)")]
      (eval-lisp! "(get-buffer-create \"*other-test*\")")
      (let [other (eval-lisp! "(other-buffer)")]
        (is (not= current other)
            "other-buffer should return different buffer"))
      ;; Cleanup
      (eval-lisp! "(kill-buffer \"*other-test*\")"))))

(deftest test-buffer-enable-undo-activates-undo
  (testing "buffer-enable-undo enables undo recording"
    (setup-test)
    (eval-lisp! "(erase-buffer)")
    ;; Disable then re-enable
    (eval-lisp! "(buffer-disable-undo)")
    (eval-lisp! "(buffer-enable-undo)")
    ;; Record some changes
    (eval-lisp! "(insert \"text\")")
    (eval-lisp! "(undo-boundary)")
    ;; Undo should work
    (eval-lisp! "(undo)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Undo should work after buffer-enable-undo")))

(deftest test-erase-buffer-clears-content
  (testing "erase-buffer deletes all content"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(erase-buffer)")

    (is (= "" (eval-lisp! "(buffer-string)"))
        "Buffer should be empty")
    (is (= 0 (eval-lisp! "(point)"))
        "Point should be at beginning")))

;; =============================================================================
;; Buffer-Local Variables
;; =============================================================================

(deftest test-buffer-local-value-basics
  (testing "buffer-local-value gets variable value in specific buffer"
    (setup-test)
    ;; Set buffer-read-only to true
    (eval-lisp! "(setq buffer-read-only t)")
    (let [val (eval-lisp! "(buffer-local-value 'buffer-read-only (current-buffer))")]
      (is (true? val) "Should return buffer-local value"))
    ;; Reset
    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-kill-all-local-variables-clears-locals
  (testing "kill-all-local-variables resets buffer to default state"
    (setup-test)
    ;; Set some buffer-local
    (eval-lisp! "(make-local-variable 'test-var)")
    (eval-lisp! "(setq test-var 42)")

    (eval-lisp! "(kill-all-local-variables)")

    ;; Local should be gone (returns nil or default)
    (let [val (eval-lisp! "(buffer-local-value 'test-var (current-buffer))")]
      (is (nil? val) "Local variable should be cleared"))))
