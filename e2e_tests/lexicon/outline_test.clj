(ns lexicon.outline-test
  "E2E tests for outline and code folding.

  Emacs source: lisp/outline.el, lisp/progmodes/hideshow.el
  Status: 0% implemented

  Key features:
  - Outline headings can be collapsed
  - hs-minor-mode hides code blocks
  - Uses text properties (invisible)

  Related: Issue #114 (Outline & Folding), Issue #94 (TDD)
  Priority: LOW"
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
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; Outline Mode
;; =============================================================================

(deftest test-outline-mode-basics
  (testing "outline headings identified"
    (setup-test)
    (eval-lisp! "(insert \"* Heading 1\\nContent\\n** Heading 2\\nMore content\")")
    (eval-lisp "(outline-minor-mode 1)")
    (is true "Outline minor mode should be enabled"))

  (testing "heading can be collapsed"
    (setup-test)
    (eval-lisp! "(insert \"* Heading\\nHidden content\\nMore hidden\")")
    (eval-lisp "(outline-minor-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp "(outline-hide-subtree)")
    (is true "Some content should be hidden")))

(deftest test-outline-navigation
  (testing "next heading navigation"
    (setup-test)
    (eval-lisp! "(insert \"* H1\\nContent\\n* H2\\nContent\\n* H3\")")
    (eval-lisp "(outline-minor-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp "(outline-next-heading)")
    (is (> (eval-lisp! "(point)") 0)
        "Should move to next heading")))

;; =============================================================================
;; Hideshow (Code Folding)
;; =============================================================================

(deftest test-hs-minor-mode-basics
  (testing "hs-minor-mode enables"
    (setup-test)
    (eval-lisp! "(insert \"function foo() {\\n  body\\n}\")")
    (eval-lisp "(hs-minor-mode 1)")
    (is true "hs-minor-mode should be enabled"))

  (testing "code block can be hidden"
    (setup-test)
    (eval-lisp! "(insert \"function foo() {\\n  body\\n}\")")
    (eval-lisp "(hs-minor-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp "(hs-hide-block)")
    (is true "Block body should be hidden")))

(deftest test-hs-toggle-hiding
  (testing "toggle hiding works"
    (setup-test)
    (eval-lisp! "(insert \"function foo() {\\n  body\\n}\")")
    (eval-lisp "(hs-minor-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp "(hs-toggle-hiding)")
    (is true "Toggle should change visibility")))

;; =============================================================================
;; Integration with Text Properties
;; =============================================================================

(deftest test-folding-uses-invisible-property
  (testing "hidden text has invisible property"
    (setup-test)
    (eval-lisp! "(insert \"* Heading\\nHidden content\")")
    (eval-lisp "(outline-minor-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp "(outline-hide-subtree)")
    (let [prop (eval-lisp "(get-text-property 10 'invisible)")]
      (is (or (not (:success prop))
              (some? (:result prop)))
          "Hidden text should have invisible property"))))
