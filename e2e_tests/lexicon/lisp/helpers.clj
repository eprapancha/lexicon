(ns lexicon.lisp.helpers
  "Shared helpers for Lisp API tests.

  These tests intentionally use evalLisp to test the Lisp evaluation
  system itself, not user-visible keyboard behavior."
  (:require [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(defn eval-lisp
  "Evaluate Lisp code and return the result."
  [code]
  (let [result (e/js-execute h/*driver* (str "return window.evalLisp(`" code "`)"))
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp!
  "Evaluate Lisp code and return just the result (throws on error)"
  [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success
      result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test
  "Standard test setup for Lisp API tests"
  []
  (h/setup-test*)
  ;; Start with clean buffer - using Lisp API is intentional here
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))
