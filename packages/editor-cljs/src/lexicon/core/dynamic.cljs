(ns lexicon.core.dynamic
  "Dynamic variables for Lexicon editor.

  These are thread-local (in ClojureScript, just dynamic) variables
  that affect editor behavior in a scoped way.

  See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Priority 5: inhibit-read-only")

(def ^:dynamic *inhibit-read-only*
  "When true, bypass read-only buffer protection.

  Used by modes to update their own read-only buffers.
  Example: Dired refreshes its listing with inhibit-read-only.

  Usage:
    (binding [*inhibit-read-only* true]
      (insert \"text in read-only buffer\"))"
  false)

(def ^:dynamic *undo-in-progress*
  "When true, don't record undo entries.

  Prevents undo operations from being recorded in undo history."
  false)

(def ^:dynamic *save-in-progress*
  "When true, buffer save operation is in progress.

  Used by before-save-hook and after-save-hook."
  false)

(def ^:dynamic *minibuffer-depth*
  "Nesting depth of minibuffer activations.

  0 = no minibuffer active
  1 = one minibuffer
  2+ = recursive minibuffer"
  0)

(defn inhibit-read-only?
  "Check if read-only is currently inhibited."
  []
  *inhibit-read-only*)

(defn undo-in-progress?
  "Check if undo is currently in progress."
  []
  *undo-in-progress*)

;; Macros (with-inhibit-read-only, without-undo) are in dynamic.clj
