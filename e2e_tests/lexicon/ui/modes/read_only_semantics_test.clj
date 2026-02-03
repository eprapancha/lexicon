(ns lexicon.ui.modes.read-only-semantics-test
  "E2E tests for Emacs read-only buffer semantic - Epic #86

  Tests critical invariant: Read-only buffers prevent modification"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-read-only-buffer-prevents-modification
  (testing "Emacs invariant: Read-only buffers reject modifications"
    (h/setup-test*)

    ;; Switch to *Messages* buffer which is typically read-only
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "*Messages*")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Get initial content
    (let [initial-text (h/get-buffer-text*)]
      ;; Try to type in the read-only buffer
      (h/type-text "SHOULD-NOT-APPEAR")
      (Thread/sleep 100)

      ;; Text should not have changed
      (let [after-text (h/get-buffer-text*)
            echo-text (h/get-echo-area-text)]
        (is (not (.contains after-text "SHOULD-NOT-APPEAR"))
            (str "Read-only buffer should reject typing, got: " after-text))

        ;; Echo area might show error message
        (is (or (not (.contains after-text "SHOULD-NOT-APPEAR"))
                (.contains echo-text "read-only")
                (.contains echo-text "Read-only"))
            "Should reject edit or show read-only message")))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.ui.modes.read-only-semantics-test))
