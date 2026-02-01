(ns lexicon.core.completion.buffer
  "Buffer Completion Table - Completion for buffer names.

  Used by:
    - C-x b (switch-to-buffer)
    - C-x k (kill-buffer)
    - C-x C-b (buffer-menu)

  Features:
    - Completes buffer names from current buffer list
    - Annotations show major mode and modification status
    - Sorts by recency (most recent first)

  See Issue #137."
  (:require [lexicon.core.completion.table :as table]))

(defn buffer-annotation
  "Generate annotation for buffer completion.
   Shows major mode and modified status."
  [buffer-info]
  (let [{:keys [major-mode modified?]} buffer-info]
    (str " ("
         (when major-mode (name major-mode))
         (when modified? " *")
         ")")))

(defn make-buffer-table
  "Create a completion table for buffer names.

  Takes the db (or buffers map) and returns a completion table.
  Buffers are sorted by recency (if :last-access is available)
  or alphabetically.

  Example:
    (make-buffer-table (:buffers @db))"
  [buffers]
  (let [buffer-names (map :name (vals buffers))
        buffer-info-map (into {}
                              (map (fn [[_id buf]]
                                     [(:name buf) {:major-mode (:major-mode buf)
                                                   :modified? (:modified? buf)
                                                   :last-access (:last-access buf)}])
                                   buffers))
        annotation-fn (fn [name]
                        (when-let [info (get buffer-info-map name)]
                          (buffer-annotation info)))
        ;; Sort by last-access if available, otherwise alphabetically
        sort-fn (fn [candidates]
                  (sort-by (fn [name]
                             (let [info (get buffer-info-map name)]
                               (- (or (:last-access info) 0))))
                           candidates))]
    (table/make-completion-table
     buffer-names
     {:category :buffer
      :annotation-fn annotation-fn
      :sort-fn sort-fn})))

(defn buffer-completions
  "Get buffer name completions for the given input.
   Simple helper for commands that just need a list of matches."
  [buffers input]
  (let [table (make-buffer-table buffers)]
    (table/all-completions table input nil)))

(defn buffer-names
  "Get all buffer names from db.
   Simple helper for getting raw buffer name list."
  [buffers]
  (map :name (vals buffers)))
