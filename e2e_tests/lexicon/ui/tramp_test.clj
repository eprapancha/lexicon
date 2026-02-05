(ns lexicon.ui.tramp-test
  "E2E tests for TRAMP remote file access (#126).

  Tests verify actual behavior of implemented TRAMP commands:
  - tramp-list-connections: creates *TRAMP Connections* buffer with method listing
  - tramp-cleanup-all-connections: reports cleanup count
  - tramp-list-remote-buffers: reports remote buffer status

  Stubs (need WebSocket bridge):
  - Actual SSH/SCP connections, remote file reading/writing"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helpers --

(defn get-named-buffer-text
  "Get text of a buffer by name from editorState.buffers."
  [buf-name]
  (e/js-execute h/*driver*
                (str "
    const state = window.editorState;
    if (!state || !state.buffers) return '';
    for (const buf of Object.values(state.buffers)) {
      if (buf.name === '" buf-name "') return buf.text || '';
    }
    return '';
  ")))

;; -- Tests --

(deftest test-tramp-list-connections-buffer-content
  (testing "tramp-list-connections creates buffer with method listing"
    (h/setup-test*)
    (h/execute-command "tramp-list-connections")
    (Thread/sleep 200)
    (let [text (get-named-buffer-text "*TRAMP Connections*")]
      (is (seq text)
          "Should create non-empty *TRAMP Connections* buffer")
      (is (str/includes? text "TRAMP Connections")
          (str "Buffer should have header, got: " (subs text 0 (min 200 (count text)))))
      (is (str/includes? text "Methods:")
          (str "Buffer should list available methods, got: " (subs text 0 (min 300 (count text)))))
      ;; Should list known methods
      (is (str/includes? text "ssh")
          (str "Should list ssh method, got: " (subs text 0 (min 300 (count text))))))))

(deftest test-tramp-list-connections-shows-no-connections
  (testing "tramp-list-connections shows no connections when none active"
    (h/setup-test*)
    (h/execute-command "tramp-list-connections")
    (Thread/sleep 200)
    (let [text (get-named-buffer-text "*TRAMP Connections*")]
      (is (str/includes? text "no active connections")
          (str "Should show no-connections message, got: " (subs text 0 (min 300 (count text))))))))

(deftest test-tramp-cleanup-all-echoes-result
  (testing "tramp-cleanup-all-connections reports cleanup result"
    (h/setup-test*)
    (h/execute-command "tramp-cleanup-all-connections")
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "no connections")
              (str/includes? (str/lower-case echo) "connection")
              (str/includes? (str/lower-case echo) "clean"))
          (str "Should report cleanup result, got: " echo)))))

(deftest test-tramp-list-remote-buffers-echoes-result
  (testing "tramp-list-remote-buffers reports remote buffer status"
    (h/setup-test*)
    (h/execute-command "tramp-list-remote-buffers")
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "remote")
              (str/includes? (str/lower-case echo) "no remote"))
          (str "Should report remote buffer status, got: " echo)))))
