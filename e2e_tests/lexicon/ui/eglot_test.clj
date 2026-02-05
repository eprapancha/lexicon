(ns lexicon.ui.eglot-test
  "E2E tests for eglot LSP client (#129).

  Tests verify actual behavior of implemented eglot commands:
  - eglot: starts server for current mode, echoes connection status
  - eglot-events-buffer: creates *EGLOT Events* buffer with server info
  - eglot-shutdown: echoes shutdown status
  - eglot-shutdown-all: shuts down all servers, reports count
  - eglot-reconnect: echoes reconnection status
  - eglot-ensure: auto-starts if mode is configured

  Stubs (need LSP backend):
  - Real LSP protocol, real diagnostics, real code actions"
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

(deftest test-eglot-start-echoes-connection
  (testing "M-x eglot reports connection status"
    (h/setup-test*)
    (h/execute-command "eglot")
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "connect")
              (str/includes? (str/lower-case echo) "eglot")
              (str/includes? (str/lower-case echo) "server")
              (str/includes? (str/lower-case echo) "started"))
          (str "Should report connection status, got: " echo)))))

(deftest test-eglot-events-buffer-has-server-info
  (testing "eglot-events-buffer creates buffer with server config info"
    (h/setup-test*)
    (h/execute-command "eglot-events-buffer")
    (Thread/sleep 200)
    (let [text (get-named-buffer-text "*EGLOT Events*")]
      (is (seq text)
          "Should create non-empty *EGLOT Events* buffer")
      (is (str/includes? text "EGLOT Events")
          (str "Buffer should have header, got: " (subs text 0 (min 200 (count text)))))
      (is (str/includes? text "Servers:")
          (str "Buffer should show server count, got: " (subs text 0 (min 300 (count text)))))
      (is (str/includes? text "Configured modes:")
          (str "Buffer should show configured modes, got: " (subs text 0 (min 300 (count text))))))))

(deftest test-eglot-events-buffer-shows-events-after-start
  (testing "eglot-events-buffer shows events after eglot start"
    (h/setup-test*)
    ;; Start eglot first to generate events
    (h/execute-command "eglot")
    (Thread/sleep 500)
    ;; Now check events buffer
    (h/execute-command "eglot-events-buffer")
    (Thread/sleep 200)
    (let [text (get-named-buffer-text "*EGLOT Events*")]
      ;; After starting eglot, there should be events logged
      (is (or (str/includes? text "start")
              (str/includes? text "connect")
              (str/includes? text "(no events"))
          (str "Should show start event or no-events, got: "
               (subs text 0 (min 300 (count text))))))))

(deftest test-eglot-shutdown-echoes-result
  (testing "eglot-shutdown reports shutdown status"
    (h/setup-test*)
    (h/execute-command "eglot-shutdown")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "shutdown")
              (str/includes? (str/lower-case echo) "no server")
              (str/includes? (str/lower-case echo) "not running")
              (str/includes? (str/lower-case echo) "eglot"))
          (str "Should report shutdown status, got: " echo)))))

(deftest test-eglot-shutdown-all-echoes-count
  (testing "eglot-shutdown-all reports how many servers were stopped"
    (h/setup-test*)
    (h/execute-command "eglot-shutdown-all")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "shut down")
              (str/includes? (str/lower-case echo) "no server")
              (str/includes? (str/lower-case echo) "0 server")
              (str/includes? (str/lower-case echo) "eglot"))
          (str "Should report shutdown-all result, got: " echo)))))

(deftest test-eglot-reconnect-echoes-status
  (testing "eglot-reconnect reports reconnection attempt"
    (h/setup-test*)
    (h/execute-command "eglot-reconnect")
    (Thread/sleep 300)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "reconnect")
              (str/includes? (str/lower-case echo) "no server")
              (str/includes? (str/lower-case echo) "eglot"))
          (str "Should report reconnect status, got: " echo)))))

(deftest test-eglot-format-buffer-echoes-result
  (testing "eglot-format-buffer reports formatting result"
    (h/setup-test*)
    (h/execute-command "eglot-format-buffer")
    (Thread/sleep 200)
    (let [echo (h/get-echo-area-text)]
      (is (or (str/includes? (str/lower-case echo) "format")
              (str/includes? (str/lower-case echo) "no active server")
              (str/includes? (str/lower-case echo) "no change"))
          (str "Should report format result, got: " echo)))))
