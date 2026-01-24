(ns lexicon.semantic.helpers
  "Macros for test helpers.")

(defmacro with-test-buffer
  "Create test buffer, execute body, clean up.

  Usage: (with-test-buffer \"*test*\"
           (insert \"text\")
           (is (= \"text\" (buffer-text))))"
  [buffer-name & body]
  `(let [buf-id# (lexicon.semantic.helpers/create-buffer ~buffer-name)]
     (try
       ~@body
       (finally
         ;; TODO: Clean up buffer after test
         nil))))
