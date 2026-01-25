(ns lexicon.kill-ring-test
  "E2E tests for kill ring - core editing primitive (Issue #104).

  Kill ring is a circular list storing killed/copied text for yanking.
  Emacs implementation: lisp/simple.el (kill-new, kill-append, current-kill, yank)

  Key variables:
  - kill-ring: The list of killed strings
  - kill-ring-max: Maximum size (default 120 in Emacs)
  - kill-ring-yank-pointer: Points to next item for yank-pop

  Related: Issue #97 (Kill ring not functioning), Issue #104 (TDD)
  Priority: CRITICAL - required for basic editing workflow"
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
;; Kill Ring Core Operations
;; =============================================================================

(deftest test-kill-region-adds-to-kill-ring
  (testing "kill-region saves text and deletes"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    ;; Kill region should save "Hello" and delete it
    (eval-lisp! "(kill-region 0 5)")
    (is (= " World" (eval-lisp! "(buffer-string)"))
        "Text should be deleted")
    (is (= "Hello" (eval-lisp! "(current-kill 0)"))
        "Killed text should be on kill ring")))

(deftest test-yank-inserts-from-kill-ring
  (testing "yank inserts last killed text"
    (setup-test)
    (eval-lisp! "(insert \"World\")")
    ;; Add something to kill ring
    (eval-lisp! "(kill-new \"Hello \")")
    ;; Yank at beginning
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(yank)")
    (is (= "Hello World" (eval-lisp! "(buffer-string)"))
        "Yanked text should be inserted")))

(deftest test-yank-pop-rotates-kill-ring
  (testing "yank-pop rotates through kills"
    (setup-test)
    ;; Add multiple items to kill ring
    (eval-lisp! "(kill-new \"First\")")
    (eval-lisp! "(kill-new \"Second\")")
    (eval-lisp! "(kill-new \"Third\")")
    ;; Yank most recent
    (eval-lisp! "(yank)")
    (is (= "Third" (eval-lisp! "(buffer-string)")))
    ;; Pop to previous
    (eval-lisp! "(yank-pop)")
    (is (= "Second" (eval-lisp! "(buffer-string)")))
    ;; Pop again
    (eval-lisp! "(yank-pop)")
    (is (= "First" (eval-lisp! "(buffer-string)")))))

(deftest test-kill-new-adds-to-front
  (testing "kill-new prepends to kill ring"
    (setup-test)
    (eval-lisp! "(kill-new \"First\")")
    (eval-lisp! "(kill-new \"Second\")")
    (is (= "Second" (eval-lisp! "(current-kill 0)"))
        "Most recent kill at front")
    (is (= "First" (eval-lisp! "(current-kill 1)"))
        "Previous kill accessible")))

(deftest test-kill-append-concatenates
  (testing "kill-append concatenates with last kill"
    (setup-test)
    (eval-lisp! "(kill-new \"Hello\")")
    (eval-lisp! "(kill-append \" World\" nil)")
    (is (= "Hello World" (eval-lisp! "(current-kill 0)"))
        "Text appended to last kill")))

(deftest test-current-kill-rotates
  (testing "current-kill with rotation"
    (setup-test)
    (eval-lisp! "(kill-new \"First\")")
    (eval-lisp! "(kill-new \"Second\")")
    (eval-lisp! "(kill-new \"Third\")")
    ;; Rotate to second
    (let [kill (eval-lisp! "(current-kill 1)")]
      (is (= "Second" kill)))
    ;; Now yank should get second (pointer rotated)
    (eval-lisp! "(yank)")
    (is (= "Second" (eval-lisp! "(buffer-string)")))))

;; =============================================================================
;; Kill Commands
;; =============================================================================

(deftest test-kill-line-basic
  (testing "kill-line kills to end of line"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(kill-line)")
    (is (= "\nWorld" (eval-lisp! "(buffer-string)"))
        "Killed to end of first line")
    (is (= "Hello" (eval-lisp! "(current-kill 0)"))
        "Killed text on ring"))

  (testing "kill-line at end of line kills newline"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\")")
    (eval-lisp! "(goto-char 5)")  ; After "Hello", before newline
    (eval-lisp! "(kill-line)")
    (is (= "HelloWorld" (eval-lisp! "(buffer-string)"))
        "Newline was killed")))

(deftest test-kill-word-basic
  (testing "kill-word kills one word forward"
    (setup-test)
    (eval-lisp! "(insert \"Hello World Goodbye\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(kill-word 1)")
    (is (= " World Goodbye" (eval-lisp! "(buffer-string)")))
    (is (= "Hello" (eval-lisp! "(current-kill 0)")))))

(deftest test-backward-kill-word-basic
  (testing "backward-kill-word kills one word backward"
    (setup-test)
    (eval-lisp! "(insert \"Hello World Goodbye\")")
    (eval-lisp! "(goto-char 11)")  ; After "World"
    (eval-lisp! "(backward-kill-word 1)")
    (is (= "Hello  Goodbye" (eval-lisp! "(buffer-string)")))
    (is (= "World" (eval-lisp! "(current-kill 0)")))))

;; =============================================================================
;; Copy (Non-destructive Kill)
;; =============================================================================

(deftest test-copy-region-as-kill
  (testing "copy-region-as-kill saves without deleting"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(copy-region-as-kill 0 5)")
    (is (= "Hello World" (eval-lisp! "(buffer-string)"))
        "Text should NOT be deleted")
    (is (= "Hello" (eval-lisp! "(current-kill 0)"))
        "Text should be on kill ring")))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-kill-ring-yank-pointer-reset
  (testing "Pointer resets after new kill"
    (setup-test)
    (eval-lisp! "(kill-new \"First\")")
    (eval-lisp! "(kill-new \"Second\")")
    ;; Rotate to first
    (eval-lisp! "(current-kill 1)")
    ;; Add new kill
    (eval-lisp! "(kill-new \"Third\")")
    ;; Yank should get newest, not where pointer was
    (eval-lisp! "(yank)")
    (is (= "Third" (eval-lisp! "(buffer-string)")))))
