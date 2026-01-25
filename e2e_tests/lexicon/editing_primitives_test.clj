(ns lexicon.editing-primitives-test
  "E2E tests for editing primitives - core text manipulation (Issue #105).

  Editing primitives in Emacs src/editfns.c (~70 DEFUNs).
  Key categories:
  - Text insertion: insert, insert-before-markers
  - Text deletion: delete-region, delete-and-extract-region
  - Text extraction: buffer-substring, buffer-string
  - Point movement: goto-char, point
  - Position predicates: bobp, eobp, bolp, eolp
  - Character access: char-after, char-before
  - Line positions: line-beginning-position, line-end-position

  Related: Issue #105 (Editing Primitives), Issue #94 (TDD)
  Priority: CRITICAL - foundation for all text editing"
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
;; Text Insertion
;; =============================================================================

(deftest test-insert-basic
  (testing "insert adds text at point"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (is (= "Hello" (eval-lisp! "(buffer-string)")))
    (is (= 5 (eval-lisp! "(point)")) "Point at end of insertion"))

  (testing "insert at middle of text"
    (setup-test)
    (eval-lisp! "(insert \"HelloWorld\")")
    (eval-lisp! "(goto-char 5)")
    (eval-lisp! "(insert \" \")")
    (is (= "Hello World" (eval-lisp! "(buffer-string)")))))

(deftest test-insert-before-markers-semantics
  (testing "markers stay before inserted text"
    (setup-test)
    (eval-lisp! "(insert \"AB\")")
    (let [marker-id (eval-lisp! "(make-marker 1)")]
      (eval-lisp! "(goto-char 1)")
      (eval-lisp! "(insert-before-markers \"X\")")
      ;; Marker should still be at 1 (before X)
      (is (= 1 (eval-lisp! (str "(marker-position " marker-id ")")))
          "Marker should stay before inserted text")
      (is (= "AXB" (eval-lisp! "(buffer-string)"))))))

;; =============================================================================
;; Text Deletion
;; =============================================================================

(deftest test-delete-region-basic
  (testing "delete-region removes text"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(delete-region 0 6)")
    (is (= "World" (eval-lisp! "(buffer-string)"))))

  (testing "delete-region with reversed args"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(delete-region 11 5)")  ; END before START
    (is (= "Hello" (eval-lisp! "(buffer-string)"))
        "Should work with reversed arguments")))

(deftest test-delete-and-extract-region
  (testing "returns deleted text"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [deleted (eval-lisp! "(delete-and-extract-region 0 6)")]
      (is (= "Hello " deleted))
      (is (= "World" (eval-lisp! "(buffer-string)"))))))

;; =============================================================================
;; Text Extraction
;; =============================================================================

(deftest test-buffer-substring-basic
  (testing "buffer-substring extracts text"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (is (= "World" (eval-lisp! "(buffer-substring 6 11)")))))

(deftest test-buffer-substring-no-properties
  (testing "strips text properties"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (let [text (eval-lisp! "(buffer-substring-no-properties 0 5)")]
      (is (= "Hello" text)))))

;; =============================================================================
;; Point and Position
;; =============================================================================

(deftest test-goto-char-basic
  (testing "goto-char moves point"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 6)")
    (is (= 6 (eval-lisp! "(point)"))))

  (testing "goto-char clamps to buffer bounds"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 100)")
    (is (<= (eval-lisp! "(point)") 5)
        "Should clamp to buffer end"))

  (testing "goto-char accepts marker"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (let [marker-id (eval-lisp! "(make-marker 6)")]
      (eval-lisp! (str "(goto-char " marker-id ")"))
      (is (= 6 (eval-lisp! "(point)"))))))

(deftest test-point-returns-position
  (testing "point returns current position"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 3)")
    (is (= 3 (eval-lisp! "(point)")))))

;; =============================================================================
;; Position Predicates
;; =============================================================================

(deftest test-bobp-eobp-predicates
  (testing "bobp at beginning"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 0)")
    (is (true? (eval-lisp! "(bobp)")) "Should be at beginning"))

  (testing "bobp not at beginning"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 3)")
    (is (not (eval-lisp! "(bobp)")) "Should not be at beginning"))

  (testing "eobp at end"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 5)")
    (is (true? (eval-lisp! "(eobp)")) "Should be at end"))

  (testing "eobp not at end"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 0)")
    (is (not (eval-lisp! "(eobp)")) "Should not be at end")))

(deftest test-bolp-eolp-predicates
  (testing "bolp at line beginning"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\")")
    (eval-lisp! "(goto-char 6)")  ; Beginning of 'World'
    (is (true? (eval-lisp! "(bolp)")) "Should be at line beginning"))

  (testing "eolp at line end"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\")")
    (eval-lisp! "(goto-char 5)")  ; End of 'Hello', before newline
    (is (true? (eval-lisp! "(eolp)")) "Should be at line end")))

;; =============================================================================
;; Character Access
;; =============================================================================

(deftest test-char-after-before
  (testing "char-after returns character"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 0)")
    (is (= "H" (eval-lisp! "(char-after)"))))

  (testing "char-after at end returns nil"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 5)")
    (is (nil? (eval-lisp! "(char-after)"))))

  (testing "char-before returns previous character"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 5)")
    (is (= "o" (eval-lisp! "(char-before)"))))

  (testing "char-before at beginning returns nil"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 0)")
    (is (nil? (eval-lisp! "(char-before)")))))

;; =============================================================================
;; Line Positions
;; =============================================================================

(deftest test-line-beginning-end-positions
  (testing "line-beginning-position"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\\nFoo\")")
    (eval-lisp! "(goto-char 8)")  ; Middle of 'World'
    (is (= 6 (eval-lisp! "(line-beginning-position)"))))

  (testing "line-end-position"
    (setup-test)
    (eval-lisp! "(insert \"Hello\\nWorld\\nFoo\")")
    (eval-lisp! "(goto-char 8)")  ; Middle of 'World'
    (is (= 11 (eval-lisp! "(line-end-position)")))))

;; =============================================================================
;; Region
;; =============================================================================

(deftest test-region-beginning-end
  (testing "region bounds"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(set-mark 0)")
    (eval-lisp! "(goto-char 5)")
    (is (= 0 (eval-lisp! "(region-beginning)")))
    (is (= 5 (eval-lisp! "(region-end)"))))

  (testing "region with point before mark"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(set-mark 10)")
    (eval-lisp! "(goto-char 5)")
    (is (= 5 (eval-lisp! "(region-beginning)")))
    (is (= 10 (eval-lisp! "(region-end)")))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-insert-empty-string
  (testing "insert empty string"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [len (count (eval-lisp! "(buffer-string)"))]
      (eval-lisp! "(insert \"\")")
      (is (= len (count (eval-lisp! "(buffer-string)")))))))

(deftest test-delete-region-empty
  (testing "delete empty region"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(delete-region 2 2)")
    (is (= "Hello" (eval-lisp! "(buffer-string)")))))

(deftest test-buffer-substring-whole-buffer
  (testing "extract whole buffer"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (is (= (eval-lisp! "(buffer-string)")
           (eval-lisp! "(buffer-substring 0 11)")))))
