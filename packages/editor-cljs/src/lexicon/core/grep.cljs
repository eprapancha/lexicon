(ns lexicon.core.grep
  "Grep search integration (grep.el).

  Implements Emacs grep.el core functionality:
  - grep: Run grep with specified pattern
  - lgrep: Grep in a directory (searches open buffers)
  - rgrep: Recursive grep (searches all open buffers)
  - grep-find: Grep via find

  Output goes to *grep* buffer in grep-mode with navigation:
  - n/M-n: Next match
  - p/M-p: Previous match
  - Enter: Go to match
  - g: Re-run grep
  - q: Quit grep buffer

  In browser context, searches across open buffers since
  filesystem grep is not available.

  Based on Emacs lisp/progmodes/grep.el

  NOTE: This package uses lisp.cljs primitives where possible.
  Buffer creation still requires re-frame for WASM setup."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.lisp :as lisp]
            [lexicon.core.db :as db]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce grep-state (atom {:last-pattern nil
                           :matches []
                           :match-index 0}))

;; =============================================================================
;; Grep Engine - Search across open buffers
;; =============================================================================

(defn- search-buffer
  "Search a single buffer for pattern matches.
   Returns vector of {:buffer-name :line-num :line-text :match}."
  [buffer-name text pattern-re]
  (let [lines (str/split text #"\n" -1)]
    (reduce-kv
     (fn [results idx line]
       (if-let [match (re-find pattern-re line)]
         (conj results {:buffer-name buffer-name
                        :line-num (inc idx)
                        :line-text line
                        :match (if (string? match) match (first match))})
         results))
     []
     (vec lines))))

(defn- run-grep
  "Run grep across all open buffers. Returns formatted output and match count."
  [pattern]
  (let [buffers (lisp/buffer-list)
        pattern-re (try (js/RegExp. pattern "g")
                        (catch :default _ nil))]
    (if-not pattern-re
      {:output (str "grep: invalid regexp: " pattern "\n")
       :matches []
       :count 0}
      (let [all-matches
            (reduce
             (fn [acc buffer-info]
               (let [buffer-id (:id buffer-info)
                     name (:name buffer-info)
                     text (lisp/buffer-text-of buffer-id)]
                 (if (and text
                          (not (str/starts-with? name "*"))
                          (seq text))
                   (let [matches (search-buffer name text pattern-re)]
                     (into acc matches))
                   acc)))
             []
             buffers)
            output (if (seq all-matches)
                     (str "Grep results for: " pattern "\n\n"
                          (str/join "\n"
                                    (map (fn [m]
                                           (str (:buffer-name m) ":"
                                                (:line-num m) ":"
                                                (:line-text m)))
                                         all-matches))
                          "\n\n"
                          "Grep finished with " (count all-matches) " match"
                          (when (not= 1 (count all-matches)) "es")
                          " found.\n")
                     (str "Grep results for: " pattern "\n\n"
                          "Grep finished with 0 matches found.\n"))]
        {:output output
         :matches all-matches
         :count (count all-matches)}))))

;; =============================================================================
;; Grep Navigation (using lisp.cljs primitives)
;; =============================================================================

(defn grep-next-match!
  "Go to next grep match."
  []
  (let [{:keys [matches match-index]} @grep-state
        new-idx (min (inc match-index) (dec (count matches)))]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (swap! grep-state assoc :match-index new-idx)
        (lisp/message (str (:buffer-name match) ":"
                          (:line-num match) ": "
                          (subs (:line-text match) 0
                                (min 60 (count (:line-text match)))))))
      (lisp/message "No grep matches"))))

(defn grep-prev-match!
  "Go to previous grep match."
  []
  (let [{:keys [matches match-index]} @grep-state
        new-idx (max (dec match-index) 0)]
    (if (seq matches)
      (let [match (nth matches new-idx)]
        (swap! grep-state assoc :match-index new-idx)
        (lisp/message (str (:buffer-name match) ":"
                          (:line-num match) ": "
                          (subs (:line-text match) 0
                                (min 60 (count (:line-text match)))))))
      (lisp/message "No grep matches"))))

(defn grep-goto-match!
  "Jump to current grep match in source buffer."
  []
  (let [{:keys [matches match-index]} @grep-state]
    (if (and (seq matches) (< match-index (count matches)))
      (let [match (nth matches match-index)
            target-name (:buffer-name match)
            buffers (lisp/buffer-list)
            target-buffer (first (filter #(= (:name %) target-name) buffers))]
        (if target-buffer
          (do
            (lisp/switch-to-buffer (:id target-buffer))
            (lisp/goto-line (:line-num match))
            (lisp/message (str "Line " (:line-num match))))
          (lisp/message (str "Buffer " target-name " not found"))))
      (lisp/message "No match to visit"))))

(defn grep-rerun!
  "Re-run the last grep."
  []
  (if-let [pattern (:last-pattern @grep-state)]
    (rf/dispatch [:grep/run pattern])
    (lisp/message "No previous grep to re-run")))

(defn grep-quit!
  "Quit grep buffer."
  []
  (rf/dispatch [:kill-buffer-by-name "*grep*"]))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :grep/run
 (fn [{:keys [db]} [_ pattern]]
   "Run grep and display results in *grep* buffer."
   (let [{:keys [output matches count]} (run-grep pattern)
         existing (first (filter #(= (:name (val %)) "*grep*") (:buffers db)))]
     ;; Update grep state
     (swap! grep-state assoc
            :last-pattern pattern
            :matches matches
            :match-index 0)
     (if existing
       ;; Update existing buffer
       (let [buffer-id (key existing)
             ^js wasm (get-in db [:buffers buffer-id :wasm-instance])]
         (when wasm
           (let [len (.-length wasm)]
             (.delete wasm 0 len)
             (.insert wasm 0 output)))
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] output)
                  (assoc-in [:buffers buffer-id :cache :line-count]
                            (count (str/split output #"\n" -1))))
          :fx [[:dispatch [:switch-buffer buffer-id]]
               [:dispatch [:echo/message
                           (str "Grep finished: " count " match"
                                (when (not= 1 count) "es") " found")]]]})
       ;; Create new *grep* buffer
       (let [buffers (:buffers db)
             buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. output))
             lines (str/split output #"\n" -1)
             line-count (count lines)]
         (if-not wasm-instance
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
           {:db (assoc-in db [:buffers buffer-id]
                          {:id buffer-id
                           :name "*grep*"
                           :wasm-instance wasm-instance
                           :file-handle nil
                           :major-mode :grep-mode
                           :is-read-only? true
                           :is-modified? false
                           :mark-position nil
                           :cursor-position {:line 0 :column 0}
                           :selection-range nil
                           :minor-modes #{}
                           :buffer-local-vars {}
                           :ast nil
                           :language :text
                           :diagnostics []
                           :undo-stack []
                           :undo-in-progress? false
                           :editor-version 0
                           :text-properties {}
                           :overlays {}
                           :next-overlay-id 1
                           :cache {:text output
                                   :line-count line-count}})
            :fx [[:dispatch [:switch-buffer buffer-id]]
                 [:dispatch [:echo/message
                             (str "Grep finished: " count " match"
                                  (when (not= 1 count) "es") " found")]]]}))))))

(rf/reg-event-fx
 :grep/next-match
 (fn [{:keys [db]} [_]]
   (grep-next-match!)
   {:db db}))

(rf/reg-event-fx
 :grep/prev-match
 (fn [{:keys [db]} [_]]
   (grep-prev-match!)
   {:db db}))

(rf/reg-event-fx
 :grep/goto-match
 (fn [{:keys [db]} [_]]
   (grep-goto-match!)
   {:db db}))

(rf/reg-event-fx
 :grep/rerun
 (fn [{:keys [db]} [_]]
   (grep-rerun!)
   {:db db}))

(rf/reg-event-fx
 :grep/quit
 (fn [{:keys [db]} [_]]
   (grep-quit!)
   {:db db}))

(rf/reg-event-fx
 :grep/prompt
 (fn [_ [_]]
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

(rf/reg-event-fx
 :grep/lgrep
 (fn [_ [_]]
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Local grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

(rf/reg-event-fx
 :grep/rgrep
 (fn [_ [_]]
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Recursive grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize grep module and register commands."
  []
  (rf/dispatch [:register-command :grep
                {:docstring "Run grep and display results in *grep* buffer"
                 :interactive nil
                 :handler [:grep/prompt]}])

  (rf/dispatch [:register-command :lgrep
                {:docstring "Local grep - search in open buffers"
                 :interactive nil
                 :handler [:grep/lgrep]}])

  (rf/dispatch [:register-command :rgrep
                {:docstring "Recursive grep - search all open buffers"
                 :interactive nil
                 :handler [:grep/rgrep]}])

  (rf/dispatch [:register-command :grep-find
                {:docstring "Run grep via find (searches open buffers)"
                 :interactive nil
                 :handler [:grep/prompt]}])

  ;; grep-mode keybindings
  (rf/dispatch [:keymap/set-mode-key :grep-mode "n" :grep/next-match])
  (rf/dispatch [:keymap/set-mode-key :grep-mode "p" :grep/prev-match])
  (rf/dispatch [:keymap/set-mode-key :grep-mode "Enter" :grep/goto-match])
  (rf/dispatch [:keymap/set-mode-key :grep-mode "g" :grep/rerun])
  (rf/dispatch [:keymap/set-mode-key :grep-mode "q" :grep/quit])

  ;; Register navigation commands for M-x access
  (rf/dispatch [:register-command :next-error
                {:docstring "Go to next grep/compilation match"
                 :interactive nil
                 :handler [:grep/next-match]}])

  (rf/dispatch [:register-command :previous-error
                {:docstring "Go to previous grep/compilation match"
                 :interactive nil
                 :handler [:grep/prev-match]}]))
