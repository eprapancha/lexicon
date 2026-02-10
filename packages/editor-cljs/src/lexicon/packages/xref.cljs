(ns lexicon.packages.xref
  "Cross-reference and definition lookup (xref.el).

  Implements Emacs xref.el core functionality:
  - xref-find-definitions (M-.): Jump to definition
  - xref-find-references: Find all references
  - xref-go-back (M-,): Return to previous location
  - xref-find-apropos: Search for pattern in all open buffers
  - xref results buffer (*xref*) with navigation

  Navigation uses a marker stack to track jump history.

  Key bindings:
  - M-. : xref-find-definitions
  - M-, : xref-go-back
  - M-? : xref-find-references

  Based on Emacs lisp/progmodes/xref.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Marker Stack (Package-local state)
;; =============================================================================

(defonce xref-marker-stack (atom []))
(defonce xref-results (atom {:matches [] :index 0}))

;; =============================================================================
;; Marker Stack Operations
;; =============================================================================

(defn- push-marker!
  "Push current position onto marker stack."
  [buffer-id position]
  (swap! xref-marker-stack conj {:buffer-id buffer-id :position position}))

(defn- pop-marker!
  "Pop and return top marker from stack."
  []
  (let [marker (peek @xref-marker-stack)]
    (when marker
      (swap! xref-marker-stack pop))
    marker))

;; =============================================================================
;; Search Functions
;; =============================================================================

(defn- search-buffers-for-pattern
  "Search all open buffers for a regex pattern.
   Returns vector of {:buffer-name :buffer-id :line-num :line-text}."
  [pattern-str]
  (let [pattern-re (try (js/RegExp. pattern-str "")
                        (catch :default _ nil))]
    (when pattern-re
      (let [buffers (lisp/buffer-list)]
        (reduce
         (fn [results buffer-info]
           (let [buffer-id (:id buffer-info)
                 name (:name buffer-info)
                 text (lisp/buffer-text-of buffer-id)]
             (if (and text (not (str/starts-with? (or name "") "*")))
               (let [lines (str/split text #"\n" -1)
                     matches (keep-indexed
                              (fn [i line]
                                (when (.test pattern-re line)
                                  (set! (.-lastIndex pattern-re) 0)
                                  {:buffer-name name
                                   :buffer-id buffer-id
                                   :line-num (inc i)
                                   :line-text line}))
                              lines)]
                 (into results matches))
               results)))
         []
         buffers)))))

(defn- search-buffers-for-identifier
  "Search all open buffers for word-boundary matches of identifier."
  [identifier]
  (search-buffers-for-pattern (str "\\b" identifier "\\b")))

;; =============================================================================
;; Xref Buffer Creation
;; =============================================================================

(defn- create-xref-buffer!
  "Create an *xref* results buffer with the given matches."
  [identifier matches]
  (let [output (str "Xref results for: " identifier "\n\n"
                    (if (seq matches)
                      (str/join "\n"
                                (map (fn [m]
                                       (str (:buffer-name m) ":"
                                            (:line-num m) ": "
                                            (str/trim (:line-text m))))
                                     matches))
                      "No matches found.")
                    "\n")]
    ;; Kill existing *xref* buffer if any
    (when-let [existing-id (lisp/get-buffer "*xref*")]
      (lisp/kill-buffer existing-id))
    ;; Create new xref buffer
    (lisp/create-special-buffer "*xref*" output
                                 {:major-mode :xref-mode
                                  :read-only true})))

;; =============================================================================
;; Xref Navigation
;; =============================================================================

(defn xref-next-result!
  "Go to next result in xref results."
  []
  (let [{:keys [matches index]} @xref-results
        new-idx (min (dec (count matches)) (inc index))]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (reset! xref-results {:matches matches :index new-idx})
        (lisp/switch-to-buffer (:buffer-id match))
        (lisp/message (str (:buffer-name match) ":" (:line-num match))))
      (lisp/message "No xref results"))))

(defn xref-prev-result!
  "Go to previous result in xref results."
  []
  (let [{:keys [matches index]} @xref-results
        new-idx (max 0 (dec index))]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (reset! xref-results {:matches matches :index new-idx})
        (lisp/switch-to-buffer (:buffer-id match))
        (lisp/message (str (:buffer-name match) ":" (:line-num match))))
      (lisp/message "No xref results"))))

;; =============================================================================
;; Xref Commands
;; =============================================================================

(defn xref-find-definitions!
  "Find definitions of identifier at point (M-.).
   Searches all open buffers for the identifier."
  []
  (let [buffer-id (lisp/current-buffer)
        cursor-pos (lisp/point)
        identifier (lisp/symbol-at-point)]
    ;; Save current position to marker stack
    (when buffer-id
      (push-marker! buffer-id cursor-pos))
    (if identifier
      (let [matches (search-buffers-for-identifier identifier)]
        (if (seq matches)
          (do
            (reset! xref-results {:matches matches :index 0})
            (create-xref-buffer! identifier matches)
            (lisp/split-window-below)
            (lisp/switch-to-buffer "*xref*")
            (lisp/message (str (count matches) " reference"
                               (when (not= 1 (count matches)) "s")
                               " found for " identifier)))
          (lisp/message (str identifier ": no definitions found"))))
      (lisp/message "No identifier at point"))))

(defn xref-find-references!
  "Find references to identifier at point (M-?).
   Searches all open buffers for references."
  []
  (let [identifier (lisp/symbol-at-point)]
    (if identifier
      (let [matches (search-buffers-for-identifier identifier)]
        (if (seq matches)
          (do
            (reset! xref-results {:matches matches :index 0})
            (create-xref-buffer! identifier matches)
            (lisp/split-window-below)
            (lisp/switch-to-buffer "*xref*")
            (lisp/message (str (count matches) " reference"
                               (when (not= 1 (count matches)) "s")
                               " found for " identifier)))
          (lisp/message (str identifier ": no references found"))))
      (lisp/message "No identifier at point"))))

(defn xref-go-back!
  "Return to previous location in xref marker stack (M-,)."
  []
  (if-let [marker (pop-marker!)]
    (do
      (lisp/switch-to-buffer (:buffer-id marker))
      (lisp/goto-char (:position marker))
      (lisp/message "Jumped back"))
    (lisp/message "Xref marker stack is empty")))

(defn xref-find-apropos-interactive []
  "Search for pattern in all open buffers."
  (lisp/read-from-minibuffer
   "Xref apropos (regexp): "
   (fn [pattern]
     (when (and pattern (seq pattern))
       (let [matches (search-buffers-for-pattern pattern)]
         (if (seq matches)
           (do
             (reset! xref-results {:matches matches :index 0})
             (create-xref-buffer! pattern matches)
             (lisp/split-window-below)
             (lisp/switch-to-buffer "*xref*")
             (lisp/message (str (count matches) " match"
                                (when (not= 1 (count matches)) "es")
                                " for " pattern)))
           (lisp/message (str "No matches for: " pattern))))))))

(defn xref-quit!
  "Quit xref buffer."
  []
  (when-let [xref-id (lisp/get-buffer "*xref*")]
    (lisp/delete-window)
    (lisp/kill-buffer xref-id)))

;; =============================================================================
;; Project Commands (project.el integration)
;; =============================================================================

(defn- infer-project-root
  "Infer project root from common path prefix of open buffers."
  []
  (let [buffers (lisp/all-buffer-info)
        paths (keep :file buffers)]
    (if (seq paths)
      ;; Find longest common prefix of all file paths
      (let [segments (map #(str/split % #"/") paths)
            common (reduce
                    (fn [acc segs]
                      (take-while (fn [[a b]] (= a b))
                                  (map vector acc segs)))
                    (first segments)
                    (rest segments))
            prefix (str/join "/" (map first common))]
        (if (seq prefix) prefix "/"))
      "/")))

(defn project-find-regexp-interactive []
  "Search for regexp in project files (C-x p g).
   Searches all open buffers."
  (lisp/read-from-minibuffer
   "Find regexp in project: "
   (fn [pattern]
     (when (and pattern (seq pattern))
       (let [matches (search-buffers-for-pattern pattern)]
         (if (seq matches)
           (do
             (reset! xref-results {:matches matches :index 0})
             (create-xref-buffer! pattern matches)
             (lisp/split-window-below)
             (lisp/switch-to-buffer "*xref*")
             (lisp/message (str (count matches) " match"
                                (when (not= 1 (count matches)) "es")
                                " in project for " pattern)))
           (lisp/message (str "No matches in project for: " pattern))))))))

(defn project-switch-to-buffer-interactive []
  "Switch to a buffer belonging to the current project (C-x p b).
   Lists non-special buffers."
  (let [buffers (lisp/all-buffer-info)
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
        names (map :name file-buffers)]
    (if (seq names)
      (lisp/read-from-minibuffer
       "Switch to project buffer: "
       (fn [buffer-name]
         (when-let [target-id (lisp/get-buffer buffer-name)]
           (lisp/switch-to-buffer target-id))))
      (lisp/message "No project buffers"))))

(defn project-kill-buffers!
  "Kill all buffers belonging to the current project (C-x p k)."
  []
  (let [buffers (lisp/all-buffer-info)
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
        ids (map :id file-buffers)]
    (if (seq ids)
      (do
        (doseq [id ids]
          (lisp/kill-buffer id))
        (lisp/message (str "Killed " (count ids) " project buffer"
                          (when (not= 1 (count ids)) "s"))))
      (lisp/message "No project buffers to kill"))))

(defn project-list-buffers!
  "List buffers belonging to the current project."
  []
  (let [root (infer-project-root)
        buffers (lisp/all-buffer-info)
        file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
        content (str "Project buffers (root: " root ")\n\n"
                     (if (seq file-buffers)
                       (str/join "\n" (map (fn [buf]
                                             (str "  " (:name buf)
                                                  (when (:modified? buf) " [modified]")))
                                           file-buffers))
                       "(no project buffers)")
                     "\n")]
    (lisp/create-special-buffer "*project-buffers*" content
                                 {:major-mode :special-mode
                                  :read-only true})
    (lisp/split-window-below)
    (lisp/switch-to-buffer "*project-buffers*")))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize xref and project modules."
  []
  ;; Xref commands
  (lisp/define-command 'xref-find-definitions
    xref-find-definitions!
    "Find definitions of identifier at point (M-.)")

  (lisp/define-command 'xref-find-references
    xref-find-references!
    "Find all references to identifier at point (M-?)")

  (lisp/define-command 'xref-go-back
    xref-go-back!
    "Return to previous location (M-,)")

  (lisp/define-command 'xref-find-apropos
    xref-find-apropos-interactive
    "Search for pattern across all open buffers")

  ;; Xref results navigation
  (lisp/define-command 'xref-next-result
    xref-next-result!
    "Go to next xref result")

  (lisp/define-command 'xref-prev-result
    xref-prev-result!
    "Go to previous xref result")

  (lisp/define-command 'xref-quit
    xref-quit!
    "Quit xref buffer")

  ;; xref-mode keybindings
  (lisp/define-key-for-mode :xref-mode "n" :xref-next-result)
  (lisp/define-key-for-mode :xref-mode "p" :xref-prev-result)
  (lisp/define-key-for-mode :xref-mode "q" :xref-quit)
  (lisp/define-key-for-mode :xref-mode "RET" :xref-goto-match)

  ;; Project commands
  (lisp/define-command 'project-find-regexp
    project-find-regexp-interactive
    "Search for regexp in project buffers (C-x p g)")

  (lisp/define-command 'project-switch-to-buffer
    project-switch-to-buffer-interactive
    "Switch to a project buffer (C-x p b)")

  (lisp/define-command 'project-kill-buffers
    project-kill-buffers!
    "Kill all project buffers (C-x p k)")

  (lisp/define-command 'project-list-buffers
    project-list-buffers!
    "List project buffers"))
