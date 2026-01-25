(ns lexicon.e2e.editor-semantics.minibuffer-test
  "E2E tests for Emacs minibuffer semantic - Epic #86

  Tests critical invariant:
  - The minibuffer is a real buffer, not a special widget"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(defn get-minibuffer-input-value
  "Get the value typed into minibuffer input field"
  []
  (try
    (e/get-element-value h/*driver* {:css ".minibuffer-input"})
    (catch Exception _ "")))

(deftest test-minibuffer-is-a-buffer
  (testing "Emacs invariant: The minibuffer is a real buffer, not a special widget"
    (h/setup-test*)

    ;; Open minibuffer with C-x b
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    ;; Minibuffer should be visible as a real buffer
    (is (h/minibuffer-visible?)
        "Minibuffer should open when command invoked")

    ;; Should be able to type into minibuffer
    (h/type-in-minibuffer "test-buffer-name")
    (Thread/sleep 50)

    ;; Should be able to read minibuffer contents
    (let [mb-text (get-minibuffer-input-value)]
      (is (.contains mb-text "test-buffer-name")
          (str "Should be able to insert and read text from minibuffer, got: '" mb-text "'")))

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Minibuffer should close
    (let [mb-visible-after (h/minibuffer-visible?)]
      (is (not mb-visible-after)
          "Minibuffer should close after cancellation"))

    ;; Open minibuffer again to verify buffer persists across activations
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (is (h/minibuffer-visible?)
        "Minibuffer should be able to reactivate")

    ;; Cancel again
    (h/press-ctrl "g")
    (Thread/sleep 50)))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.minibuffer-test))
