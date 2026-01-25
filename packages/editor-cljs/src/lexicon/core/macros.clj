(ns lexicon.core.macros
  "Core macros for Lexicon editor.

  ClojureScript macros must be defined in .clj files (not .cljs).
  These provide syntactic sugar and control flow primitives.

  See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Priority 2: save-excursion")

(defmacro save-excursion
  "Save point and mark, execute body, then restore them.

  Semantic Guarantee:
  - Point before save-excursion: N
  - Body moves point to M
  - Point after save-excursion: N (restored)

  Why this matters:
  - Commands can look ahead/behind without confusing user
  - Dired can check file properties without moving cursor
  - Every search/query operation needs this

  Implementation:
  - Captures point and mark before body
  - Executes body in try block
  - Restores point/mark in finally block (even on error)

  Usage:
    (save-excursion
      (goto-char 0)
      (looking-at \"Hello\"))  ; Returns result, point restored

  Example from dired.el line 1258:
    (save-excursion (goto-char (point-min)) ...)"
  [& body]
  `(let [saved-point# (lexicon.core.api.test/point (lexicon.core.api.test/current-buffer))
         saved-mark# (lexicon.core.api.test/mark (lexicon.core.api.test/current-buffer))
         result# (atom nil)]
     (try
       (reset! result# (do ~@body))
       (finally
         ;; Restore point and mark
         (when saved-point#
           (lexicon.core.api.test/set-point! (lexicon.core.api.test/current-buffer) saved-point#))
         (when saved-mark#
           (lexicon.core.api.test/set-mark! (lexicon.core.api.test/current-buffer) saved-mark#))))
     @result#))

(defmacro save-current-buffer
  "Save current buffer, execute body, then restore it.

  Semantic Guarantee:
  - Current buffer before: B1
  - Body switches to buffer B2
  - Current buffer after: B1 (restored)

  Why this matters:
  - Commands that check other buffers
  - Dired operations that look at file buffers
  - User expects to stay in same buffer

  Usage:
    (save-current-buffer
      (set-buffer \"*other*\")
      (buffer-text))  ; Returns text from *other*, current buffer restored"
  [& body]
  `(let [saved-buffer-id# (lexicon.core.api.test/current-buffer)
         result# (atom nil)]
     (try
       (reset! result# (do ~@body))
       (finally
         ;; Restore current buffer
         (when saved-buffer-id#
           (lexicon.core.api.test/show-buffer! saved-buffer-id#))))
     @result#))

(defmacro with-current-buffer
  "Execute body with buffer temporarily current.

  Semantic Guarantee:
  - Current buffer set to buffer-id
  - Body executes
  - Original buffer restored

  Usage:
    (with-current-buffer \"*Messages*\"
      (goto-char (point-max))
      (insert \"Log message\"))"
  [buffer-id & body]
  `(save-current-buffer
     (lexicon.core.api.test/show-buffer! ~buffer-id)
     ~@body))

(defmacro save-restriction
  "Save narrowing state, execute body, then restore it.

  Note: Narrowing not yet implemented - this is a placeholder.

  Semantic Guarantee:
  - Narrowing before: [A, B]
  - Body changes narrowing to [C, D]
  - Narrowing after: [A, B] (restored)

  Usage:
    (save-restriction
      (narrow-to-region 10 20)
      ...)"
  [& body]
  `(let [result# (atom nil)]
     ;; TODO: Implement when narrowing is added
     (reset! result# (do ~@body))
     @result#))

(defmacro save-excursion-and-restriction
  "Combine save-excursion and save-restriction.

  Saves point, mark, and narrowing state.

  Usage:
    (save-excursion-and-restriction
      (narrow-to-region 0 100)
      (goto-char 50)
      ...)"
  [& body]
  `(save-excursion
     (save-restriction
       ~@body)))
