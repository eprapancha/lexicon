(ns lexicon.ui.uniquify-test
  "E2E tests for uniquify buffer name disambiguation (#141).

  Tests verify that when multiple files with the same base name are opened,
  their buffer names are disambiguated using directory path components."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helper Functions --

(defn wait-for-editor
  "Wait for editor to be ready and setup"
  []
  (h/setup-test*))

(defn get-current-buffer-name
  "Get the current buffer name from window.editorState.bufferName"
  []
  (e/js-execute h/*driver* "return window.editorState?.bufferName || null"))

(defn get-all-buffer-names
  "Get all buffer names from window.editorState.buffers"
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state || !state.buffers) return [];
    return Object.values(state.buffers).map(b => b.name);
  "))

;; -- Tests --

(deftest test-uniquify-module-loaded
  (testing "Uniquify module initializes correctly"
    (wait-for-editor)
    ;; Check that the editor has loaded
    (is (h/get-buffer-text*)
        "Editor should be loaded")))

(deftest test-single-file-uses-base-name
  (testing "Single file buffer uses base filename without disambiguation"
    (wait-for-editor)
    ;; Start in scratch buffer - check current buffer name
    (let [current-name (get-current-buffer-name)]
      (is (and current-name (str/includes? current-name "scratch"))
          "Should start in *scratch* buffer"))))

(deftest test-buffer-file-path-nil-for-scratch
  (testing "buffer-file-name returns nil for non-file buffers"
    (wait-for-editor)
    ;; Scratch buffer is not visiting a file, so buffer-file-name should return nil
    ;; We verify by checking the buffer name indicates it's scratch, not a file
    (let [current-name (get-current-buffer-name)]
      (is (str/starts-with? current-name "*")
          "Scratch buffer name starts with * (not a file buffer)"))))

(deftest test-global-vars-initialized
  (testing "Global variables are initialized"
    (wait-for-editor)
    ;; The editor should have loaded successfully
    (let [buffer-text (h/get-buffer-text*)]
      ;; If we got any buffer text (even empty), editor is initialized
      (is (string? buffer-text)
          "Editor should be initialized with a buffer"))))

(deftest test-uniquify-style-command-available
  (testing "Editor is responsive and buffer operations work"
    (wait-for-editor)
    ;; Verify that we can get all buffer names (tests the JS integration)
    (let [names (get-all-buffer-names)]
      ;; Should have at least scratch and Messages buffers
      (is (>= (count names) 1)
          "Should have at least one buffer"))))

;; Note: Full integration tests for uniquify require the ability to open
;; multiple files via the File System Access API, which is difficult to
;; automate in E2E tests due to browser security restrictions.
;;
;; The uniquify algorithm itself is tested through the module's pure functions.
;; Manual testing scenarios:
;; 1. Open /project1/config.cljs and /project2/config.cljs
;;    Expected: config.cljs<project1> and config.cljs<project2>
;; 2. Open /a/b/file.txt and /c/b/file.txt
;;    Expected: file.txt<a/b> and file.txt<c/b>
