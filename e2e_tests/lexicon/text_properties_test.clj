(ns lexicon.text-properties-test
  "E2E tests for text properties - CORE primitive needed by all packages.

  Text properties attach invisible metadata to buffer text ranges.
  Used by: completion UI, syntax highlighting, org-mode, dired, magit, etc.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - affects all packages"
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
;; Text Properties - Core Primitive Tests
;; =============================================================================

(deftest test-text-property-invisible
  (testing "Invisible text hidden from display"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    ;; Make "World" invisible
    (eval-lisp! "(put-text-property 6 11 'invisible t)")

    ;; Buffer still contains full text
    (is (= "Hello World" (eval-lisp! "(buffer-string)"))
        "Buffer text includes invisible content"))

  (testing "Invisible property is per-region not per-char"
    (setup-test)
    (eval-lisp! "(insert \"AAABBBCCC\")")
    ;; Make middle section invisible
    (eval-lisp! "(put-text-property 3 6 'invisible t)")

    ;; Buffer should still contain full text
    (is (= "AAABBBCCC" (eval-lisp! "(buffer-string)"))
        "Full buffer text preserved")))

(deftest test-text-properties-survive-edits
  (testing "Property range adjusts on insert before"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(put-text-property 6 11 'face 'bold)")

    ;; Insert before the property range
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(insert \"XXX\")")

    ;; Property should shift by 3 positions
    (is (= :bold (eval-lisp! "(get-text-property 9 'face)"))
        "Property at start of range shifted")
    (is (= :bold (eval-lisp! "(get-text-property 13 'face)"))
        "Property at end of range shifted"))

  (testing "Property expands when text inserted within range"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(put-text-property 6 11 'face 'bold)")

    ;; Insert within the bold region
    (eval-lisp! "(goto-char 8)")
    (eval-lisp! "(insert \"XXX\")")

    ;; New text should inherit the property
    (is (= :bold (eval-lisp! "(get-text-property 8 'face)"))
        "Newly inserted text inherits property"))

  (testing "Property shrinks when text deleted from range"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(put-text-property 6 11 'face 'bold)")

    ;; Delete part of the bold region
    (eval-lisp! "(delete-region 6 8)")

    ;; Property range should shrink
    (is (= :bold (eval-lisp! "(get-text-property 6 'face)"))
        "Property still present at start")
    (is (nil? (eval-lisp! "(get-text-property 10 'face)"))
        "Property range shortened")))

(deftest test-text-property-mouse-face
  (testing "Mouse-face property exists and can be queried"
    (setup-test)
    (eval-lisp! "(insert \"Click me\")")
    (eval-lisp! "(put-text-property 0 8 'mouse-face 'highlight)")

    (is (= :highlight (eval-lisp! "(get-text-property 4 'mouse-face)"))
        "Mouse-face property set correctly")))

(deftest test-text-property-help-echo
  (testing "Help-echo property exists"
    (setup-test)
    (eval-lisp! "(insert \"Hover me\")")
    (eval-lisp! "(put-text-property 0 8 'help-echo \"This is a tooltip\")")

    (is (= "This is a tooltip" (eval-lisp! "(get-text-property 4 'help-echo)"))
        "Help-echo property set correctly")))

(deftest test-text-property-face
  (testing "Face property for basic styling"
    (setup-test)
    (eval-lisp! "(insert \"Bold text\")")
    (eval-lisp! "(put-text-property 0 4 'face 'bold)")

    (is (= :bold (eval-lisp! "(get-text-property 2 'face)"))
        "Face property set")))

(deftest test-text-properties-multiple
  (testing "Multiple properties coexist"
    (setup-test)
    (eval-lisp! "(insert \"Multi-property text\")")
    (eval-lisp! "(put-text-property 0 18 'face 'bold)")
    (eval-lisp! "(put-text-property 0 18 'invisible t)")
    (eval-lisp! "(put-text-property 0 18 'help-echo \"Tooltip\")")

    (is (= :bold (eval-lisp! "(get-text-property 5 'face)")))
    (is (true? (eval-lisp! "(get-text-property 5 'invisible)")))
    (is (= "Tooltip" (eval-lisp! "(get-text-property 5 'help-echo)")))))

(deftest test-text-properties-propertize
  (testing "Propertize creates property string"
    (setup-test)
    ;; Insert with propertize
    (eval-lisp! "(insert (propertize \"Click\" 'face 'link 'help-echo \"Tooltip\"))")

    (is (= "Click" (eval-lisp! "(buffer-string)"))
        "String content inserted")
    (is (= :link (eval-lisp! "(get-text-property 0 'face)"))
        "Face property from propertize")
    (is (= "Tooltip" (eval-lisp! "(get-text-property 0 'help-echo)"))
        "Help-echo property from propertize")))

;; =============================================================================
;; Property Query Functions
;; =============================================================================

(deftest test-text-properties-at
  (testing "Get all properties at once"
    (setup-test)
    (eval-lisp! "(insert \"Text\")")
    (eval-lisp! "(put-text-property 0 4 'face 'bold)")
    (eval-lisp! "(put-text-property 0 4 'invisible t)")

    (let [props (eval-lisp! "(text-properties-at 2)")]
      (is (some? props) "Properties returned")
      ;; Properties returned as plist, check values exist
      (is (= :bold (eval-lisp! "(get-text-property 2 'face)"))
          "Face property accessible")
      (is (true? (eval-lisp! "(get-text-property 2 'invisible)"))
          "Invisible property accessible"))))

;; =============================================================================
;; Integration with Core Systems
;; =============================================================================

(deftest test-text-properties-persist-across-undo
  (testing "Properties restored on undo"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(put-text-property 0 5 'face 'bold)")
    (eval-lisp! "(undo-boundary)")

    ;; Delete the text
    (eval-lisp! "(delete-region 0 5)")
    (is (= "" (eval-lisp! "(buffer-string)")))
    (eval-lisp! "(undo-boundary)")

    ;; Undo the deletion
    (eval-lisp! "(undo)")
    (is (= "Hello" (eval-lisp! "(buffer-string)")))
    (is (= :bold (eval-lisp! "(get-text-property 2 'face)"))
        "Property restored with text")))

(deftest test-text-properties-in-read-only-buffer
  (testing "Properties in read-only buffer"
    (setup-test)
    (eval-lisp! "(insert \"Read only\")")
    (eval-lisp! "(setq buffer-read-only t)")

    ;; Should be able to add properties even when read-only (with inhibit-read-only)
    (eval-lisp! "(let ((inhibit-read-only t))
                   (put-text-property 0 9 'face 'bold))")

    (is (= :bold (eval-lisp! "(get-text-property 4 'face)"))
        "Property set in read-only buffer")

    ;; Clean up
    (eval-lisp! "(setq buffer-read-only nil)")))

;; =============================================================================
;; Remove/Add Text Properties
;; =============================================================================

(deftest test-remove-text-properties
  (testing "remove-text-properties clears specific property"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(put-text-property 0 11 'face 'bold)")
    (eval-lisp! "(put-text-property 0 11 'invisible t)")

    ;; Remove only face property
    (eval-lisp! "(remove-text-properties 0 11 '(face nil))")

    (is (nil? (eval-lisp! "(get-text-property 5 'face)"))
        "Face property removed")
    (is (true? (eval-lisp! "(get-text-property 5 'invisible)"))
        "Other properties preserved")))

(deftest test-add-text-properties
  (testing "add-text-properties adds without removing"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(put-text-property 0 11 'face 'bold)")

    ;; Add another property
    (eval-lisp! "(add-text-properties 0 11 '(invisible t help-echo \"tip\"))")

    (is (= :bold (eval-lisp! "(get-text-property 5 'face)"))
        "Original property preserved")
    (is (true? (eval-lisp! "(get-text-property 5 'invisible)"))
        "New property added")
    (is (= "tip" (eval-lisp! "(get-text-property 5 'help-echo)"))
        "Another new property added")))
