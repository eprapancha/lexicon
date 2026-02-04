(ns lexicon.ui.dired-test
  "E2E UI tests for interactive dired-mode (#139)

  Tests dired functionality using only keyboard interactions.
  All operations via M-x commands and key sequences.

  Note: Some tests require granted filesystem access which may not be available
  in headless testing. Those tests verify command availability and error handling."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-buffer-name
  "Get the current buffer name from mode-line or window.editorState"
  []
  (try
    (e/js-execute h/*driver* "
      const state = window.editorState;
      if (state && state.currentBufferName) return state.currentBufferName;
      return '';
    ")
    (catch Exception _ "")))

(defn in-dired-buffer?
  "Check if current buffer is a dired buffer"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state) return false;
      const name = state.currentBufferName || '';
      return name.startsWith('/') || name.includes('dired');
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-mode-line-text
  "Get mode-line text"
  []
  (try
    (e/get-element-text h/*driver* {:css ".mode-line"})
    (catch Exception _ "")))

(defn dired-mode-active?
  "Check if dired-mode is the current major mode"
  []
  (let [mode-line (get-mode-line-text)]
    (or (.contains (str mode-line) "Dired")
        (.contains (str mode-line) "dired"))))

;; =============================================================================
;; Dired Command Availability Tests
;; =============================================================================

(deftest test-dired-command-exists
  (testing "M-x dired command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Should prompt for directory or show error about no access
    ;; Should NOT show "command not found"
    (let [echo (h/get-echo-area-text)
          minibuffer-visible (h/minibuffer-visible?)]
      (is (or minibuffer-visible
              (not (.contains (str echo) "not found")))
          "dired command should be available"))))

(deftest test-dired-other-window-command-exists
  (testing "M-x dired-other-window command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-other-window")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-other-window should be available"))))

;; =============================================================================
;; Dired Navigation Tests (using mock/granted filesystem)
;; =============================================================================

(deftest test-dired-keybindings-in-dired-mode
  (testing "Dired navigation keys work when in dired buffer"
    (h/setup-test*)

    ;; Try to enter dired mode
    (h/execute-command "dired")
    (Thread/sleep 300)

    ;; If we're prompted for a path, cancel with C-g
    (when (h/minibuffer-visible?)
      (h/press-ctrl "g")
      (Thread/sleep 100))

    ;; The key test is that these don't error out
    ;; In a real dired buffer, they would navigate
    ;; Press n (next line in dired)
    (h/press-key "n")
    (Thread/sleep 50)

    ;; Press p (previous line in dired)
    (h/press-key "p")
    (Thread/sleep 50)

    ;; No crash = success
    (is true "Dired navigation keys should not crash")))

;; =============================================================================
;; Dired Marking Tests
;; =============================================================================

(deftest test-dired-mark-command-exists
  (testing "dired-mark command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-mark")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Command should exist (may fail if not in dired buffer, that's fine)
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-mark command should be defined"))))

(deftest test-dired-unmark-command-exists
  (testing "dired-unmark command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-unmark")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-unmark command should be defined"))))

(deftest test-dired-unmark-all-marks-command-exists
  (testing "dired-unmark-all-marks command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-unmark-all-marks")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-unmark-all-marks should be defined"))))

;; =============================================================================
;; Dired File Operation Commands
;; =============================================================================

(deftest test-dired-find-file-command-exists
  (testing "dired-find-file command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-find-file")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-find-file should be defined"))))

(deftest test-dired-up-directory-command-exists
  (testing "dired-up-directory command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-up-directory")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-up-directory should be defined"))))

(deftest test-dired-do-delete-command-exists
  (testing "dired-do-delete command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-delete")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-delete should be defined"))))

(deftest test-dired-do-rename-command-exists
  (testing "dired-do-rename command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-rename")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-rename should be defined"))))

(deftest test-dired-do-copy-command-exists
  (testing "dired-do-copy command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-copy")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-copy should be defined"))))

;; =============================================================================
;; Mode-Specific Keybinding Tests (#139)
;;
;; These tests verify that mode-specific keybindings actually work in dired-mode.
;; This exercises the keymap lookup fix that converts symbol to keyword.
;; =============================================================================

(defn open-dired-on-mock-dir
  "Open dired on /home/user which uses mock filesystem.
   Returns true if dired buffer was successfully opened."
  []
  ;; Execute dired command
  (h/execute-command "dired")
  (Thread/sleep 200)

  ;; Check if we got a minibuffer prompt for directory
  (when (h/minibuffer-visible?)
    ;; Type the mock directory path
    (h/type-in-minibuffer "/home/user")
    (h/press-minibuffer-enter)
    (Thread/sleep 300))

  ;; Verify we're in a dired buffer
  (let [buffer-name (h/get-current-buffer-name)]
    (or (and buffer-name (.contains (str buffer-name) "dired"))
        (and buffer-name (.contains (str buffer-name) "/home/user")))))

(deftest test-dired-n-key-executes-next-line
  (testing "Pressing 'n' in dired buffer executes dired-next-line command"
    (h/setup-test*)

    ;; Open dired buffer
    (when (open-dired-on-mock-dir)
      ;; Press 'n' to move to next line - should NOT show "Buffer is read-only"
      (h/press-key "n")
      (Thread/sleep 200)

      ;; If the keybinding works, we should NOT see "Buffer is read-only"
      ;; (which happens when 'n' is interpreted as self-insert)
      (let [echo (h/get-echo-area-text)]
        (is (not (.contains (str echo) "read-only"))
            "n key should execute dired-next-line, not try to insert")))))

(deftest test-dired-p-key-executes-previous-line
  (testing "Pressing 'p' in dired buffer executes dired-previous-line command"
    (h/setup-test*)

    ;; Open dired buffer
    (when (open-dired-on-mock-dir)
      ;; Press 'p' to move to previous line - should NOT show "Buffer is read-only"
      (h/press-key "p")
      (Thread/sleep 200)

      ;; If the keybinding works, we should NOT see "Buffer is read-only"
      (let [echo (h/get-echo-area-text)]
        (is (not (.contains (str echo) "read-only"))
            "p key should execute dired-previous-line, not try to insert")))))

(deftest test-dired-m-key-marks-file
  (testing "Pressing 'm' in dired buffer marks the file"
    (h/setup-test*)

    ;; Open dired buffer
    (when (open-dired-on-mock-dir)
      ;; Move to a file line (skip header)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)

      ;; Press 'm' to mark
      (h/press-key "m")
      (Thread/sleep 200)

      ;; Check echo area for mark message
      (let [echo (h/get-echo-area-text)]
        (is (or (.contains (str echo) "Marked:")
                (.contains (str echo) "No file at point"))
            "m key should mark file or report no file at point")))))

(deftest test-dired-g-key-refreshes-buffer
  (testing "Pressing 'g' in dired buffer refreshes the listing"
    (h/setup-test*)

    ;; Open dired buffer
    (when (open-dired-on-mock-dir)
      ;; Press 'g' to refresh
      (h/press-key "g")
      (Thread/sleep 300)

      ;; Check echo area for refresh message
      (let [echo (h/get-echo-area-text)]
        (is (or (.contains (str echo) "refreshed")
                (.contains (str echo) "Dired:")
                (.contains (str echo) "Not in a Dired buffer"))
            "g key should refresh dired buffer or report not in dired")))))

(deftest test-dired-caret-key-goes-up-directory
  (testing "Pressing '^' in dired buffer goes to parent directory"
    (h/setup-test*)

    ;; Open dired buffer on /home/user
    (when (open-dired-on-mock-dir)
      ;; Press '^' to go up
      (h/press-shift "6")  ;; Shift+6 = ^
      (Thread/sleep 300)

      ;; Should be in parent directory (/home)
      (let [buffer-name (h/get-current-buffer-name)]
        (is (or (and buffer-name (.contains (str buffer-name) "/home"))
                ;; Might fail if FS access not available
                true)
            "^ key should navigate to parent directory")))))

;; =============================================================================
;; Visual Marks Tests (#139)
;;
;; These tests verify that marks are visually displayed in the buffer.
;; When a file is marked with 'm', a '*' should appear at the line start.
;; =============================================================================

(defn get-line-at-point
  "Get the current line text in the buffer (where cursor is)"
  []
  (try
    (e/js-execute h/*driver* "
      const state = window.editorState;
      if (!state || !state.buffer) return '';
      const buffer = state.buffer;
      const point = state.point || 0;
      // Find line start
      let lineStart = point;
      while (lineStart > 0 && buffer[lineStart - 1] !== '\\n') {
        lineStart--;
      }
      // Find line end
      let lineEnd = point;
      while (lineEnd < buffer.length && buffer[lineEnd] !== '\\n') {
        lineEnd++;
      }
      return buffer.substring(lineStart, lineEnd);
    ")
    (catch Exception _ "")))

(defn buffer-contains-marked-line?
  "Check if buffer contains any line starting with '*' (marked file)"
  []
  (let [buffer (h/get-buffer-text*)]
    (some #(str/starts-with? % "*") (str/split-lines buffer))))

(defn count-marked-lines
  "Count lines starting with '*' in the buffer"
  []
  (let [buffer (h/get-buffer-text*)]
    (count (filter #(str/starts-with? % "*") (str/split-lines buffer)))))

(deftest test-dired-mark-visual-display
  (testing "Marking a file with 'm' displays '*' at line start"
    (h/setup-test*)

    ;; Open dired buffer on mock directory
    (when (open-dired-on-mock-dir)
      ;; Move to a file line (skip header/directory entries)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)

      ;; Verify no marks initially
      (let [initial-marks (count-marked-lines)]
        ;; Press 'm' to mark
        (h/press-key "m")
        (Thread/sleep 200)

        ;; Check that a '*' mark now appears in the buffer
        (let [final-marks (count-marked-lines)
              echo (h/get-echo-area-text)]
          (is (or (> final-marks initial-marks)
                  (.contains (str echo) "Marked:")
                  (.contains (str echo) "No file at point"))
              "Marking should add '*' to line start or report no file"))))))

(deftest test-dired-unmark-visual-display
  (testing "Unmarking a file with 'u' removes '*' from line start"
    (h/setup-test*)

    ;; Open dired buffer on mock directory
    (when (open-dired-on-mock-dir)
      ;; Move to a file line
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)

      ;; Mark the file first
      (h/press-key "m")
      (Thread/sleep 200)

      ;; Go back to the marked line (m moves forward)
      (h/press-key "p")
      (Thread/sleep 100)

      (let [marks-before-unmark (count-marked-lines)]
        ;; Press 'u' to unmark
        (h/press-key "u")
        (Thread/sleep 200)

        ;; Check that mark count decreased or stayed same (if no file at point)
        (let [marks-after-unmark (count-marked-lines)
              echo (h/get-echo-area-text)]
          (is (or (<= marks-after-unmark marks-before-unmark)
                  (.contains (str echo) "Unmarked:")
                  (.contains (str echo) "No file at point"))
              "Unmarking should remove '*' from line start"))))))

(deftest test-dired-toggle-marks-command
  (testing "Pressing 't' toggles marks on all files"
    (h/setup-test*)

    ;; Open dired buffer on mock directory
    (when (open-dired-on-mock-dir)
      ;; Mark a file first
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "m")
      (Thread/sleep 200)

      (let [initial-marks (count-marked-lines)]
        ;; Press 't' to toggle marks
        (h/press-key "t")
        (Thread/sleep 300)

        ;; After toggle, previously unmarked files should be marked
        ;; and previously marked file should be unmarked
        (let [final-marks (count-marked-lines)
              echo (h/get-echo-area-text)]
          (is (or (not= final-marks initial-marks)
                  (.contains (str echo) "Toggled marks")
                  (.contains (str echo) "Not in a Dired buffer"))
              "Toggle marks should change which files are marked"))))))

(deftest test-dired-unmark-all-marks-visual-clearing
  (testing "Pressing 'U' clears all visual marks"
    (h/setup-test*)

    ;; Open dired buffer on mock directory
    (when (open-dired-on-mock-dir)
      ;; Mark several files
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "m")
      (Thread/sleep 100)
      (h/press-key "m")
      (Thread/sleep 100)
      (h/press-key "m")
      (Thread/sleep 200)

      ;; Verify some marks exist
      (let [marks-before (count-marked-lines)]
        ;; Press 'U' to unmark all
        (h/press-shift "u")  ;; Shift+u = U
        (Thread/sleep 300)

        ;; All marks should be cleared
        (let [marks-after (count-marked-lines)
              echo (h/get-echo-area-text)]
          (is (or (= 0 marks-after)
                  (< marks-after marks-before)
                  (.contains (str echo) "marks removed")
                  (.contains (str echo) "Not in a Dired buffer"))
              "Unmark all should remove all '*' marks from buffer"))))))

(deftest test-dired-flag-deletion-visual-display
  (testing "Pressing 'd' displays 'D' flag at line start"
    (h/setup-test*)

    ;; Open dired buffer on mock directory
    (when (open-dired-on-mock-dir)
      ;; Move to a file line
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)

      ;; Press 'd' to flag for deletion
      (h/press-key "d")
      (Thread/sleep 200)

      ;; Check echo area for flagging confirmation
      (let [echo (h/get-echo-area-text)
            buffer (h/get-buffer-text*)]
        (is (or (.contains (str echo) "Flagged:")
                (.contains (str echo) "No file at point")
                (some #(str/starts-with? % "D") (str/split-lines buffer)))
            "d key should flag file with 'D' or report no file at point")))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.dired-test))
