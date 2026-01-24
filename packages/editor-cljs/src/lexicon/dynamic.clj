(ns lexicon.dynamic
  "Macros for dynamic variable binding in Lexicon editor.

  These macros provide convenient syntax for temporarily changing
  dynamic variables.

  See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Priority 5: inhibit-read-only")

(defmacro with-inhibit-read-only
  "Execute body with read-only protection inhibited.

  Usage:
    (with-inhibit-read-only
      (insert \"Update read-only buffer\"))"
  [& body]
  `(binding [lexicon.dynamic/*inhibit-read-only* true]
     ~@body))

(defmacro without-undo
  "Execute body without recording undo entries.

  Usage:
    (without-undo
      (insert \"Temporary change\"))"
  [& body]
  `(binding [lexicon.dynamic/*undo-in-progress* true]
     ~@body))
