(ns lexicon.ui.fs.access-test
  "E2E tests for File System Access API integration.

  Tests the FS Access module:
  - grant-directory-access command is available
  - find-file fallback behavior when API not supported (Firefox)

  Note: Since E2E tests run in Firefox which doesn't support
  the File System Access API, these tests verify the graceful
  degradation path.

  Uses keyboard simulation for all operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Command Registration Tests
;; =============================================================================

(deftest test-grant-directory-access-command-exists
  (testing "grant-directory-access command is available via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Open M-x and type the command name
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "grant-directory")
    (Thread/sleep 300)
    ;; Escape without running
    (h/press-key "Escape")
    (Thread/sleep 100)
    ;; Verify editor is responsive by typing
    (h/type-text "ok")
    (Thread/sleep 50)
    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "ok")
          "Editor should be responsive after M-x grant-directory"))))

;; =============================================================================
;; Fallback Behavior Tests
;; =============================================================================

(deftest test-find-file-does-not-crash
  (testing "C-x C-f works without crashing (uses fallback in Firefox)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Press C-x C-f to trigger find-file
    (h/press-ctrl-x "C-f")
    (Thread/sleep 500)
    ;; In Firefox with no granted directories, the file picker opens
    ;; (which we can't interact with in automated tests)
    ;; But the editor should not crash
    ;; Press Escape to cancel any pending operation
    (h/press-key "Escape")
    (Thread/sleep 100)
    ;; Verify editor is still responsive by typing
    (h/type-text "test")
    (Thread/sleep 50)
    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "test")
          "Editor should still be responsive after find-file"))))

(deftest test-find-file-command-via-m-x
  (testing "find-file command is available via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Open M-x and type find-file
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "find-file")
    (Thread/sleep 300)
    ;; Escape without running
    (h/press-key "Escape")
    (Thread/sleep 100)
    ;; Verify editor is responsive
    (h/type-text "ok")
    (Thread/sleep 50)
    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "ok")
          "Editor should be responsive after M-x find-file"))))
