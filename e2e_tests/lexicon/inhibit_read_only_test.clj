(ns lexicon.inhibit-read-only-test
  "E2E tests for inhibit-read-only - CORE primitive for mode self-management.

  Read-only buffers block user edits but modes must update their own content.
  Used by: Dired, Help, Info, Magit, all special buffers.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - without this, no mode can manage read-only buffers"
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
;; Read-Only and Inhibit-Read-Only - Core Primitive Tests
;; =============================================================================

(deftest test-read-only-blocks-user-edits
  (testing "Insert fails in read-only buffer"
    (setup-test)
    (eval-lisp! "(insert \"Initial content\")")
    (eval-lisp! "(setq buffer-read-only t)")

    (let [result (eval-lisp "(insert \"More\")")]
      (is (not (:success result))
          "Insert blocked in read-only buffer")))

  (testing "Delete fails in read-only buffer"
    (setup-test)
    (eval-lisp! "(insert \"Delete me\")")
    (eval-lisp! "(setq buffer-read-only t)")

    (let [result (eval-lisp "(delete-region 0 6)")]
      (is (not (:success result))
          "Delete blocked in read-only buffer")))

  (testing "Navigation works in read-only buffer"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; These should work fine
    (eval-lisp! "(goto-char 5)")
    (is (= 5 (eval-lisp! "(point)")))

    (eval-lisp! "(forward-char 2)")
    (is (= 7 (eval-lisp! "(point)"))
        "Cursor movement works in read-only buffer")

    ;; Clean up
    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-inhibit-read-only-allows-edits
  (testing "Insert succeeds with inhibit-read-only"
    (setup-test)
    (eval-lisp! "(insert \"Initial\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Should work with inhibit
    (eval-lisp! "(let ((inhibit-read-only t))
                   (insert \" Modified\"))")

    (is (= "Initial Modified" (eval-lisp! "(buffer-string)"))
        "Edit succeeded despite read-only")

    (eval-lisp! "(setq buffer-read-only nil)"))

  (testing "Delete succeeds with inhibit-read-only"
    (setup-test)
    (eval-lisp! "(insert \"Delete this part\")")
    (eval-lisp! "(setq buffer-read-only t)")

    (eval-lisp! "(let ((inhibit-read-only t))
                   (delete-region 7 16))")

    (is (= "Delete " (eval-lisp! "(buffer-string)"))
        "Delete succeeded with inhibit")

    (eval-lisp! "(setq buffer-read-only nil)"))

  (testing "Protection restored after scope"
    (setup-test)
    (eval-lisp! "(insert \"Test\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Edit in inhibited scope
    (eval-lisp! "(let ((inhibit-read-only t))
                   (insert \" OK\"))")

    ;; Protection restored
    (let [result (eval-lisp "(insert \" FAIL\")")]
      (is (not (:success result))
          "Read-only protection restored after scope"))

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-nested-inhibit-read-only
  (testing "Nested inhibit scopes work"
    (setup-test)
    (eval-lisp! "(setq buffer-read-only t)")

    (eval-lisp! "(let ((inhibit-read-only t))
                   (insert \"Outer \")
                   (let ((inhibit-read-only t))
                     (insert \"Inner \"))
                   (insert \"Outer again\"))")

    (is (= "Outer Inner Outer again" (eval-lisp! "(buffer-string)"))
        "Nested inhibit worked correctly")

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-inhibit-with-errors
  (testing "Protection restored despite error"
    (setup-test)
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Error in inhibited scope
    (let [result (eval-lisp
                  "(let ((inhibit-read-only t))
                     (insert \"Before error\")
                     (error \"Test error\"))")]
      (is (not (:success result)) "Error propagated"))

    ;; Protection should be restored
    (let [result (eval-lisp "(insert \" Should fail\")")]
      (is (not (:success result))
          "Read-only protection restored after error"))

    (eval-lisp! "(setq buffer-read-only nil)")))

;; =============================================================================
;; Integration with Modes
;; =============================================================================

(deftest test-dired-refresh-pattern
  (testing "Dired refresh updates read-only buffer"
    (setup-test)
    ;; Simulate Dired buffer setup
    (eval-lisp! "(insert \"Initial listing\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Simulate Dired refresh
    (eval-lisp! "(let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert \"Refreshed listing\"))")

    (is (= "Refreshed listing" (eval-lisp! "(buffer-string)")))
    (is (true? (eval-lisp! "buffer-read-only"))
        "Buffer still read-only after refresh")

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-help-buffer-pattern
  (testing "Help buffer link insertion"
    (setup-test)
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Simulate help content generation
    (eval-lisp! "(let ((inhibit-read-only t))
                   (insert \"See also: \")
                   (let ((start (point)))
                     (insert \"save-excursion\")
                     (put-text-property start (point) 'face 'link)))")

    (is (= "See also: save-excursion" (eval-lisp! "(buffer-string)")))
    (is (= :link (eval-lisp! "(get-text-property 10 'face)"))
        "Link property added to read-only buffer")

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-info-buffer-pattern
  (testing "Info node switching"
    (setup-test)
    (eval-lisp! "(insert \"Node 1 content\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Simulate node navigation
    (eval-lisp! "(let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert \"Node 2 content\"))")

    (is (= "Node 2 content" (eval-lisp! "(buffer-string)"))
        "Node content replaced in read-only buffer")

    (eval-lisp! "(setq buffer-read-only nil)")))

;; =============================================================================
;; Read-Only Indicator and Query
;; =============================================================================

(deftest test-buffer-read-only-query
  (testing "Query read-only status"
    (setup-test)
    (is (not (eval-lisp! "buffer-read-only"))
        "New buffer not read-only")

    (eval-lisp! "(setq buffer-read-only t)")
    (is (true? (eval-lisp! "buffer-read-only"))
        "Buffer now read-only")

    (eval-lisp! "(setq buffer-read-only nil)")
    (is (not (eval-lisp! "buffer-read-only"))
        "Buffer writable again")))

(deftest test-toggle-read-only
  (testing "Toggle flips state"
    (setup-test)
    (is (not (eval-lisp! "buffer-read-only")))

    (eval-lisp! "(toggle-read-only)")
    (is (true? (eval-lisp! "buffer-read-only")) "Toggled to read-only")

    (eval-lisp! "(toggle-read-only)")
    (is (not (eval-lisp! "buffer-read-only")) "Toggled back to writable")))

;; =============================================================================
;; Special Cases and Edge Cases
;; =============================================================================

(deftest test-read-only-with-text-properties
  (testing "Properties modifiable with inhibit"
    (setup-test)
    (eval-lisp! "(insert \"Highlight me\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Should work with inhibit
    (eval-lisp! "(let ((inhibit-read-only t))
                   (put-text-property 0 9 'face 'bold))")

    (is (= :bold (eval-lisp! "(get-text-property 4 'face)"))
        "Property set in read-only buffer")

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-read-only-error-message
  (testing "Read-only error has helpful message"
    (setup-test)
    (eval-lisp! "(setq buffer-read-only t)")

    (let [result (eval-lisp "(insert \"Should fail\")")]
      (is (not (:success result)) "Should have failed")
      (is (re-find #"(?i)read" (or (:error result) ""))
          "Error message mentions read-only"))

    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-inhibit-read-only-performance
  (testing "Many edits with inhibit don't slow down"
    (setup-test)
    (eval-lisp! "(setq buffer-read-only t)")

    ;; 100 insertions (reduced from 1000 for e2e test speed)
    (let [start-time (System/currentTimeMillis)]
      (eval-lisp! "(let ((inhibit-read-only t))
                     (dotimes (i 100)
                       (insert \"x\")))")

      (let [elapsed (- (System/currentTimeMillis) start-time)]
        (is (< elapsed 5000)
            "100 inhibited edits took less than 5s")))

    (eval-lisp! "(setq buffer-read-only nil)")))
