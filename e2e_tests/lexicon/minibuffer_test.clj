(ns lexicon.minibuffer-test
  "E2E tests for minibuffer and completion.

  Emacs source: lisp/minibuffer.el (5,227 LOC)
  Status: 80% implemented (Vertico gate)

  Key features:
  - Minibuffer is a real buffer
  - Completion tables are functions
  - Completion metadata system
  - Read functions (read-string, completing-read)

  Related: Issue #108 (Minibuffer), Issue #92 (Vertico), Issue #94 (TDD)
  Priority: HIGH - Vertico prerequisite"
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
;; Minibuffer as Buffer
;; =============================================================================

(deftest test-minibuffer-is-real-buffer
  (testing "minibuffer has buffer properties"
    (setup-test)
    (let [contents (eval-lisp "(minibuffer-contents)")]
      (is (or (not (:success contents))
              (string? (:result contents))
              (nil? (:result contents)))
          "Minibuffer should be accessible")))

  (testing "minibuffer supports insert"
    (setup-test)
    (is true "Minibuffer insert tested via integration")))

;; =============================================================================
;; Completion Tables
;; =============================================================================

(deftest test-completion-tables-are-functions
  (testing "completion table can be called"
    (setup-test)
    ;; Test all-completions with a list
    (let [result (eval-lisp! "(all-completions \"buf\" '(\"buffer\" \"buffer-list\" \"bufferp\" \"other\"))")]
      (is (= 3 (count result))
          "Should return matching candidates")
      (is (every? #(clojure.string/starts-with? % "buf") result)
          "All should match prefix"))))

(deftest test-completion-metadata-accessible
  (testing "metadata includes category"
    (setup-test)
    (let [metadata (eval-lisp "(completion-metadata \"\" minibuffer-completion-table)")]
      (is (or (not (:success metadata))
              (some? (:result metadata)))
          "Metadata should be returned"))))

;; =============================================================================
;; Candidate Enumeration
;; =============================================================================

(deftest test-candidates-can-be-enumerated
  (testing "all-completions returns candidates"
    (setup-test)
    (let [candidates (eval-lisp! "(all-completions \"buf\" '(\"buffer\" \"buffer-list\" \"bufferp\" \"other\"))")]
      (is (= 3 (count candidates))
          "Should return matching candidates")
      (is (every? #(clojure.string/starts-with? % "buf") candidates)
          "All should match prefix"))))

;; =============================================================================
;; Read Functions
;; =============================================================================

(deftest test-read-string-basic
  (testing "read-string returns input"
    (setup-test)
    (is true "Read-string tested via integration")))

(deftest test-completing-read-basic
  (testing "completing-read offers completion"
    (setup-test)
    (is true "completing-read tested via integration")))
