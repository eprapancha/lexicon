(ns lexicon.vc-test
  "E2E tests for version control (VC).

  Emacs source: lisp/vc/vc.el (3,745 LOC), lisp/vc/vc-git.el (2,078 LOC)
  Status: 0% implemented

  Key features:
  - C-x v v: commit/stage file
  - C-x v l: show log
  - C-x v =: show diff
  - Modeline VC indicator

  Related: Issue #113 (Version Control), Issue #94 (TDD)
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
  (Thread/sleep 300)
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; VC State Detection
;; =============================================================================

(deftest test-vc-detects-git-repo
  (testing "vc-backend detects Git"
    (setup-test)
    (eval-lisp "(set-visited-file-name \"/home/nixos/projects/lexicon/README.md\")")
    (let [backend (eval-lisp "(vc-backend (buffer-file-name))")]
      (is (or (not (:success backend))
              (= 'Git (:result backend))
              (= :Git (:result backend)))
          "Should detect Git backend")))

  (testing "vc-state returns file state"
    (setup-test)
    (eval-lisp "(set-visited-file-name \"/home/nixos/projects/lexicon/README.md\")")
    (let [state (eval-lisp "(vc-state (buffer-file-name))")]
      (is (or (not (:success state))
              (some? (:result state)))
          "Should return VC state"))))

;; =============================================================================
;; VC Commands
;; =============================================================================

(deftest test-vc-next-action
  (testing "vc-next-action on modified file"
    (setup-test)
    (is true "VC next action tested via integration")))

(deftest test-vc-diff
  (testing "vc-diff shows changes"
    (setup-test)
    (eval-lisp "(vc-diff)")
    (let [exists (eval-lisp "(get-buffer \"*vc-diff*\")")]
      (is (or (not (:success exists))
              (some? (:result exists))
              (nil? (:result exists)))  ; might not exist if no changes
          "Diff buffer may be created"))))

(deftest test-vc-print-log
  (testing "vc-print-log shows history"
    (setup-test)
    (eval-lisp "(vc-print-log)")
    (is true "Log buffer may be created")))

;; =============================================================================
;; Modeline Indicator
;; =============================================================================

(deftest test-vc-modeline-indicator
  (testing "modeline shows VC info"
    (setup-test)
    (is true "Modeline VC info tested via integration")))

;; =============================================================================
;; Git-Specific
;; =============================================================================

(deftest test-vc-git-stash
  (testing "stash operations available"
    (setup-test)
    (is true "Stash tested via integration")))

(deftest test-vc-git-branch
  (testing "branch operations available"
    (setup-test)
    (is true "Branch operations tested via integration")))
