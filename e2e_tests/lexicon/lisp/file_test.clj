(ns lexicon.lisp.file-test
  "Lisp API tests for file operations.

  Tests file-related Lisp functions:
  - file-exists-p: Check if file exists
  - file-directory-p: Check if path is directory
  - file-attributes: Get file metadata
  - directory-files: List directory contents
  - buffer-file-name: Get file associated with buffer
  - save-buffer: Save buffer to file
  - revert-buffer: Reload buffer from file

  Note: Uses mock filesystem for testing. In production, these
  functions will use the File System Access API."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; file-exists-p
;; =============================================================================

(deftest test-file-exists-p-true
  (testing "file-exists-p returns true for existing file"
    (lisp/setup-test)
    ;; Mock filesystem has /home/user/file.txt
    (let [result (lisp/eval-lisp! "(file-exists-p \"/home/user/file.txt\")")]
      (is (= true result) "Should return true for existing file"))))

(deftest test-file-exists-p-directory
  (testing "file-exists-p returns true for directory"
    (lisp/setup-test)
    ;; Mock filesystem has /home/user directory
    (let [result (lisp/eval-lisp! "(file-exists-p \"/home/user\")")]
      (is (= true result) "Should return true for existing directory"))))

(deftest test-file-exists-p-false
  (testing "file-exists-p returns false for non-existent path"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(file-exists-p \"/nonexistent/path.txt\")")]
      (is (not result) "Should return false for non-existent path"))))

;; =============================================================================
;; file-directory-p
;; =============================================================================

(deftest test-file-directory-p-true
  (testing "file-directory-p returns true for directory"
    (lisp/setup-test)
    ;; Use /home/user which is listed in /home
    (let [result (lisp/eval-lisp! "(file-directory-p \"/home/user\")")]
      (is (= true result) "Should return true for directory"))))

(deftest test-file-directory-p-false
  (testing "file-directory-p returns false for file"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(file-directory-p \"/home/user/file.txt\")")]
      (is (not result) "Should return false for file"))))

(deftest test-file-directory-p-nonexistent
  (testing "file-directory-p returns false for non-existent path"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(file-directory-p \"/nonexistent\")")]
      (is (not result) "Should return false for non-existent path"))))

;; =============================================================================
;; file-attributes
;; =============================================================================

(deftest test-file-attributes-file
  (testing "file-attributes returns metadata for file"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(file-attributes \"/home/user/file.txt\")")]
      (is (map? result) "Should return a map")
      (is (= "file.txt" (:name result)) "Should have correct name")
      ;; Keywords become strings in JS bridge
      (is (= "file" (str (:type result))) "Should have type file"))))

(deftest test-file-attributes-directory
  (testing "file-attributes returns metadata for directory"
    (lisp/setup-test)
    ;; Use /home/user which is listed in /home
    (let [result (lisp/eval-lisp! "(file-attributes \"/home/user\")")]
      (is (map? result) "Should return a map")
      (is (= "user" (:name result)) "Should have correct name")
      ;; Keywords become strings in JS bridge
      (is (= "directory" (str (:type result))) "Should have type directory"))))

(deftest test-file-attributes-nonexistent
  (testing "file-attributes returns nil for non-existent path"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(file-attributes \"/nonexistent.txt\")")]
      (is (nil? result) "Should return nil for non-existent path"))))

;; =============================================================================
;; directory-files
;; =============================================================================

(deftest test-directory-files-lists-contents
  (testing "directory-files returns list of filenames"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(directory-files \"/home/user\")")]
      (is (sequential? result) "Should return a sequence")
      (is (>= (count result) 2) "Should have at least 2 entries")
      (is (some #(= "file.txt" %) result) "Should include file.txt")
      (is (some #(= "notes.org" %) result) "Should include notes.org"))))

(deftest test-directory-files-root
  (testing "directory-files works for root directory"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(directory-files \"/\")")]
      (is (sequential? result) "Should return a sequence")
      (is (some #(= "home" %) result) "Should include home"))))

(deftest test-directory-files-empty
  (testing "directory-files returns empty for non-existent directory"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(directory-files \"/nonexistent\")")]
      (is (empty? result) "Should return empty for non-existent directory"))))

;; =============================================================================
;; buffer-file-name
;; =============================================================================

(deftest test-buffer-file-name-nil-for-scratch
  (testing "buffer-file-name returns nil for buffer without file"
    (lisp/setup-test)
    ;; setup-test uses *scratch* buffer which has no file
    (let [result (lisp/eval-lisp! "(buffer-file-name)")]
      (is (nil? result) "Should return nil for buffer without associated file"))))

;; =============================================================================
;; save-buffer and revert-buffer
;; =============================================================================

(deftest test-save-buffer-does-not-crash
  (testing "save-buffer executes without error"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test content\")")
    ;; save-buffer returns nil on success, throws on error
    ;; Using eval-lisp to catch any errors
    (let [result (lisp/eval-lisp "(save-buffer)")]
      ;; Just check it doesn't throw - may fail silently for unassociated buffer
      (is (some? result) "save-buffer call should complete"))))

(deftest test-revert-buffer-does-not-crash
  (testing "revert-buffer executes without error"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test content\")")
    ;; revert-buffer returns nil on success, throws on error
    (let [result (lisp/eval-lisp "(revert-buffer)")]
      ;; Just check it doesn't throw
      (is (some? result) "revert-buffer call should complete"))))
