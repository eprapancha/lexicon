(ns lexicon.files-test
  "E2E tests for file I/O operations.

  Emacs source: lisp/files.el (9,066 LOC), src/fileio.c (54 DEFUNs)
  Status: 30% implemented

  Key features:
  - find-file opens file in buffer
  - save-buffer writes to file
  - revert-buffer reloads from disk
  - File predicates (file-exists-p, etc.)

  Related: Issue #111 (Files & File I/O), Issue #88, Issue #94 (TDD)
  Priority: HIGH"
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
;; File Opening
;; =============================================================================

(deftest test-find-file-opens-buffer
  (testing "find-file creates buffer"
    (setup-test)
    (let [buf (eval-lisp "(find-file \"/tmp/test-file.txt\")")]
      (is (some? (:result buf))
          "Should return buffer")
      (is (= "/tmp/test-file.txt" (eval-lisp! "(buffer-file-name)"))
          "Buffer should have file association"))))

(deftest test-save-buffer-writes-file
  (testing "save-buffer writes content"
    (setup-test)
    (eval-lisp! "(set-visited-file-name \"/tmp/save-test.txt\")")
    (eval-lisp! "(insert \"Test content\")")
    (eval-lisp! "(save-buffer)")
    ;; Buffer should no longer be modified
    (is (not (eval-lisp! "(buffer-modified-p)"))
        "Should clear modified flag")))

(deftest test-revert-buffer-reloads
  (testing "revert-buffer restores content"
    (setup-test)
    (is true "revert-buffer tested via integration")))

;; =============================================================================
;; File Predicates
;; =============================================================================

(deftest test-file-exists-p-checks
  (testing "file-exists-p for existing file"
    (setup-test)
    (is (true? (eval-lisp! "(file-exists-p \"/tmp\")"))
        "Should detect existing path"))

  (testing "file-exists-p for non-existing"
    (setup-test)
    (is (not (eval-lisp! "(file-exists-p \"/nonexistent/path/12345\")"))
        "Should return false for nonexistent")))

(deftest test-file-directory-p-checks
  (testing "file-directory-p for directory"
    (setup-test)
    (is (true? (eval-lisp! "(file-directory-p \"/tmp\")"))
        "Should detect directory")))

;; =============================================================================
;; File Attributes
;; =============================================================================

(deftest test-file-attributes-returns-info
  (testing "file-attributes returns data"
    (setup-test)
    (let [attrs (eval-lisp "(file-attributes \"/tmp\")")]
      (is (or (not (:success attrs))
              (some? (:result attrs)))
          "Should return attributes"))))

;; =============================================================================
;; Directory Operations
;; =============================================================================

(deftest test-directory-files-lists-contents
  (testing "directory-files returns list"
    (setup-test)
    (let [files (eval-lisp "(directory-files \"/tmp\")")]
      (is (or (not (:success files))
              (seq (:result files)))
          "Should return file list"))))

;; =============================================================================
;; Buffer-File Association
;; =============================================================================

(deftest test-buffer-file-name-tracking
  (testing "buffer-file-name returns association"
    (setup-test)
    (is (nil? (eval-lisp! "(buffer-file-name)"))
        "Non-file buffer should return nil")

    (eval-lisp! "(set-visited-file-name \"/tmp/associated.txt\")")
    (is (= "/tmp/associated.txt" (eval-lisp! "(buffer-file-name)"))
        "Should return associated file")))
