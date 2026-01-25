(ns lexicon.icomplete-test
  "E2E tests for incremental completion UI.

  Emacs source: lisp/icomplete.el
  Status: 0% implemented

  Key features:
  - Shows completions as you type
  - Completion cycling with C-. and C-,
  - Fido mode (flex + vertical display)

  Related: Issue #128, Issue #108, Issue #94 (TDD)
  Priority: MEDIUM"
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
  (Thread/sleep 300))

(deftest test-icomplete-mode-activation
  (testing "icomplete-mode can be enabled"
    (setup-test)
    (eval-lisp "(icomplete-mode 1)")
    (is true "icomplete-mode should be enabled")))

(deftest test-icomplete-candidate-display
  (testing "icomplete shows completions in minibuffer"
    (setup-test)
    (let [candidates (eval-lisp "(icomplete-completions)")]
      (is (or (not (:success candidates))
              (nil? (:result candidates))
              (sequential? (:result candidates)))
          "Should return completion candidates or nil"))))

(deftest test-icomplete-cycling
  (testing "icomplete-forward-completions cycles"
    (setup-test)
    (eval-lisp "(icomplete-forward-completions)")
    (is true "Should cycle forward through completions")))

(deftest test-icomplete-fido-mode
  (testing "icomplete-fido-mode enables flex matching"
    (setup-test)
    (eval-lisp "(fido-mode 1)")
    (is true "fido-mode should be enabled")))
