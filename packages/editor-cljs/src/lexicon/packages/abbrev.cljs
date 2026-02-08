(ns lexicon.packages.abbrev
  "Abbrev mode - word abbreviation expansion.

  Provides abbreviation tables that expand short abbreviations into
  longer text. Supports:
  - Global abbrevs (work in all buffers)
  - Mode-specific abbrevs (work only in specific major modes)
  - Manual expansion with C-x a e (expand-abbrev)

  Based on Emacs lisp/abbrev.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; Abbrev Tables (package-local state)
;; =============================================================================

(defonce global-abbrev-table (atom {}))
(defonce mode-abbrev-tables (atom {}))
(defonce abbrev-mode-buffers (atom #{}))

;; =============================================================================
;; Abbrev Table Operations
;; =============================================================================

(defn define-abbrev [table-atom abbrev expansion]
  (swap! table-atom assoc (str/lower-case abbrev) {:expansion expansion :count 0}))

(defn define-global-abbrev [abbrev expansion]
  (define-abbrev global-abbrev-table abbrev expansion))

(defn define-mode-abbrev [mode abbrev expansion]
  (when-not (contains? @mode-abbrev-tables mode)
    (swap! mode-abbrev-tables assoc mode {}))
  (swap! mode-abbrev-tables assoc-in [mode (str/lower-case abbrev)]
         {:expansion expansion :count 0}))

(defn- get-abbrev-expansion [abbrev mode]
  (let [abbrev-lower (str/lower-case abbrev)]
    (or
     (when-let [mode-table (get @mode-abbrev-tables mode)]
       (when-let [entry (get mode-table abbrev-lower)]
         {:expansion (:expansion entry) :table :mode}))
     (when-let [entry (get @global-abbrev-table abbrev-lower)]
       {:expansion (:expansion entry) :table :global}))))

;; =============================================================================
;; Abbrev Expansion
;; =============================================================================

(defn expand-abbrev! []
  (let [mode (lisp/major-mode)]
    (if-let [{:keys [word start]} (lisp/word-before-point)]
      (if-let [{:keys [expansion]} (get-abbrev-expansion word mode)]
        (let [word-end (lisp/point)]
          (lisp/delete-region start word-end)
          (lisp/insert expansion)
          (lisp/message (str "Expanded: " word " -> " expansion)))
        (lisp/message "No expansion for this abbrev"))
      (lisp/message "No abbrev to expand"))))

(defn add-global-abbrev! [abbrev expansion]
  (define-global-abbrev abbrev expansion)
  (lisp/message (str "Defined global abbrev: " abbrev " -> " expansion)))

(defn add-mode-abbrev! [abbrev expansion]
  (let [mode (lisp/major-mode)]
    (define-mode-abbrev mode abbrev expansion)
    (lisp/message (str "Defined " (name mode) " abbrev: " abbrev " -> " expansion))))

(defn list-abbrevs! []
  (let [global-abbrevs @global-abbrev-table
        mode-abbrevs @mode-abbrev-tables
        lines (atom [])]
    (when (seq global-abbrevs)
      (swap! lines conj "Global abbrevs:")
      (doseq [[abbrev {:keys [expansion]}] (sort-by first global-abbrevs)]
        (swap! lines conj (str "  " abbrev " -> " expansion))))
    (doseq [[mode table] (sort-by first mode-abbrevs)]
      (when (seq table)
        (swap! lines conj (str (name mode) " abbrevs:"))
        (doseq [[abbrev {:keys [expansion]}] (sort-by first table)]
          (swap! lines conj (str "  " abbrev " -> " expansion)))))
    (if (seq @lines)
      (lisp/message (str/join "\n" @lines))
      (lisp/message "No abbrevs defined"))))

(defn toggle-abbrev-mode! []
  (let [buffer-id (lisp/current-buffer)
        currently-enabled? (contains? @abbrev-mode-buffers buffer-id)]
    (if currently-enabled?
      (do
        (swap! abbrev-mode-buffers disj buffer-id)
        (lisp/message "Abbrev mode disabled"))
      (do
        (swap! abbrev-mode-buffers conj buffer-id)
        (lisp/message "Abbrev mode enabled")))))

;; =============================================================================
;; Default Abbrevs
;; =============================================================================

(defn- install-default-abbrevs! []
  (define-global-abbrev "teh" "the")
  (define-global-abbrev "adn" "and")
  (define-global-abbrev "taht" "that")
  (define-global-abbrev "wiht" "with")
  (define-mode-abbrev :clojure-mode "defn" "(defn  [])")
  (define-mode-abbrev :clojure-mode "defp" "(defn-  [])")
  (define-mode-abbrev :clojure-mode "letb" "(let [])"))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize abbrev mode commands."
  []
  (lisp/define-command 'expand-abbrev expand-abbrev!
    "Expand abbrev before point (C-x a e)")

  (lisp/define-command 'abbrev-mode toggle-abbrev-mode!
    "Toggle abbrev mode in current buffer")

  (lisp/define-command 'list-abbrevs list-abbrevs!
    "List all defined abbreviations")

  (install-default-abbrevs!))
