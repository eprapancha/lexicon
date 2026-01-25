(ns lexicon.semantic.files-test
  "Semantic tests for file I/O operations.

  Emacs source: lisp/files.el (9,066 LOC), src/fileio.c (54 DEFUNs)
  Status: 30% implemented

  Key features:
  - find-file opens file in buffer
  - save-buffer writes to file
  - revert-buffer reloads from disk
  - File predicates (file-exists-p, etc.)

  Related: Issue #111 (Files & File I/O), Issue #88, Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; File Opening
;;; =============================================================================

(deftest ^:critical find-file-opens-buffer
  "CRITICAL: find-file opens file in new buffer.

  Emacs Semantics (files.el):
  - Creates buffer with file contents
  - Sets buffer-file-name
  - Enables appropriate major mode

  Why this matters:
  - Primary file access method"
  (testing "find-file creates buffer"
    (let [buf (helpers/find-file "/tmp/test-file.txt")]
      (is (some? buf)
          "Should return buffer")
      (is (= "/tmp/test-file.txt" (helpers/buffer-file-name))
          "Buffer should have file association"))))

(deftest ^:critical save-buffer-writes-file
  "CRITICAL: save-buffer writes buffer to file.

  Emacs Semantics:
  - Writes buffer-string to buffer-file-name
  - Clears modified flag
  - Creates backup if configured

  Why this matters:
  - Primary save operation"
  (testing "save-buffer writes content"
    (with-test-buffer "*test*"
      (helpers/visit-file 1 "/tmp/save-test.txt")
      (helpers/insert "Test content")

      (helpers/save-buffer)

      ;; Buffer should no longer be modified
      (is (not (helpers/buffer-modified-p))
          "Should clear modified flag"))))

(deftest ^:critical revert-buffer-reloads
  "CRITICAL: revert-buffer reloads from disk.

  Emacs Semantics:
  - Re-reads file contents
  - Discards local changes
  - Confirms if buffer modified

  Why this matters:
  - Sync with external changes"
  (testing "revert-buffer restores content"
    (with-test-buffer "*test*"
      (helpers/visit-file 1 "/tmp/revert-test.txt")
      (helpers/insert "Original")
      (helpers/save-buffer)

      (helpers/insert " Modified")

      (helpers/revert-buffer)

      (is (= "Original" (helpers/buffer-string))
          "Should restore original content"))))

;;; =============================================================================
;;; File Predicates
;;; =============================================================================

(deftest ^:high file-exists-p-checks
  "HIGH: file-exists-p checks file existence.

  Emacs Semantics (fileio.c):
  - Returns t if file exists
  - Works with directories too

  Why this matters:
  - Conditional file operations"
  (testing "file-exists-p for existing file"
    (is (helpers/file-exists-p "/tmp")
        "Should detect existing path"))

  (testing "file-exists-p for non-existing"
    (is (not (helpers/file-exists-p "/nonexistent/path/12345"))
        "Should return false for nonexistent")))

(deftest ^:high file-directory-p-checks
  "HIGH: file-directory-p checks if path is directory.

  Emacs Semantics:
  - Returns t if path is directory
  - Returns nil for files

  Why this matters:
  - Dired needs to distinguish files/dirs"
  (testing "file-directory-p for directory"
    (is (helpers/file-directory-p "/tmp")
        "Should detect directory"))

  (testing "file-directory-p for file"
    (is (not (helpers/file-directory-p "/etc/passwd"))
        "Should return false for file")))

;;; =============================================================================
;;; File Attributes
;;; =============================================================================

(deftest ^:medium file-attributes-returns-info
  "MEDIUM: file-attributes returns file metadata.

  Emacs Semantics (fileio.c):
  - Returns list of attributes
  - Includes size, mtime, permissions

  Why this matters:
  - Dired display, file operations"
  (testing "file-attributes returns data"
    (let [attrs (helpers/file-attributes "/tmp")]
      (is (some? attrs)
          "Should return attributes"))))

;;; =============================================================================
;;; Directory Operations
;;; =============================================================================

(deftest ^:medium directory-files-lists-contents
  "MEDIUM: directory-files lists directory contents.

  Emacs Semantics:
  - Returns list of filenames
  - Optional FULL for full paths
  - Optional MATCH for filtering

  Why this matters:
  - Dired file listing"
  (testing "directory-files returns list"
    (let [files (helpers/directory-files "/tmp")]
      (is (seq files)
          "Should return file list"))))

;;; =============================================================================
;;; Buffer-File Association
;;; =============================================================================

(deftest ^:high buffer-file-name-tracking
  "HIGH: buffer-file-name tracks associated file.

  Emacs Semantics:
  - Returns nil for non-file buffers
  - Returns path for file buffers
  - set-visited-file-name changes it

  Why this matters:
  - Save/revert operations"
  (testing "buffer-file-name returns association"
    (with-test-buffer "*test*"
      (is (nil? (helpers/buffer-file-name))
          "Non-file buffer should return nil")

      (helpers/visit-file 1 "/tmp/associated.txt")
      (is (= "/tmp/associated.txt" (helpers/buffer-file-name))
          "Should return associated file"))))
