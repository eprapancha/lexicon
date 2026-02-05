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

  Key bindings:
  - M-x grep: Run grep
  - M-x lgrep: Local grep
  - M-x rgrep: Recursive grep

  Based on Emacs lisp/progmodes/grep.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Grep Engine - Search across open buffers
;; =============================================================================

(defn- search-buffer
  "Search a single buffer for pattern matches.
   Returns vector of {:buffer-name :line-num :line-text :match-start :match-end}."
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
  [db pattern]
  (let [buffers (:buffers db)
        pattern-re (try (js/RegExp. pattern "g")
                        (catch :default _ nil))]
    (if-not pattern-re
      {:output (str "grep: invalid regexp: " pattern "\n")
       :matches []
       :count 0}
      (let [all-matches
            (reduce-kv
             (fn [acc _buffer-id buffer]
               (let [name (:name buffer)
                     wasm (:wasm-instance buffer)
                     text (when wasm
                            (try (.getText ^js wasm) (catch :default _ nil)))]
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
;; Grep Buffer Management
;; =============================================================================

(rf/reg-event-fx
 :grep/run
 (fn [{:keys [db]} [_ pattern]]
   "Run grep and display results in *grep* buffer."
   (let [{:keys [output matches count]} (run-grep db pattern)
         existing (first (filter #(= (:name (val %)) "*grep*") (:buffers db)))]
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
                            (count (str/split output #"\n" -1)))
                  (assoc-in [:grep :last-pattern] pattern)
                  (assoc-in [:grep :matches] matches)
                  (assoc-in [:grep :match-index] 0))
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
           {:db (-> db
                    (assoc-in [:buffers buffer-id]
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
                    (assoc-in [:grep :last-pattern] pattern)
                    (assoc-in [:grep :matches] matches)
                    (assoc-in [:grep :match-index] 0))
            :fx [[:dispatch [:switch-buffer buffer-id]]
                 [:dispatch [:echo/message
                             (str "Grep finished: " count " match"
                                  (when (not= 1 count) "es") " found")]]]}))))))

;; =============================================================================
;; Grep Navigation
;; =============================================================================

(rf/reg-event-fx
 :grep/next-match
 (fn [{:keys [db]} [_]]
   "Go to next grep match."
   (let [matches (get-in db [:grep :matches] [])
         idx (get-in db [:grep :match-index] 0)
         new-idx (min (inc idx) (dec (count matches)))]
     (if (seq matches)
       (let [match (nth matches new-idx)]
         {:db (assoc-in db [:grep :match-index] new-idx)
          :fx [[:dispatch [:echo/message
                           (str (:buffer-name match) ":"
                                (:line-num match) ": "
                                (subs (:line-text match) 0
                                      (min 60 (count (:line-text match)))))]]]})
       {:fx [[:dispatch [:echo/message "No grep matches"]]]}))))

(rf/reg-event-fx
 :grep/prev-match
 (fn [{:keys [db]} [_]]
   "Go to previous grep match."
   (let [matches (get-in db [:grep :matches] [])
         idx (get-in db [:grep :match-index] 0)
         new-idx (max (dec idx) 0)]
     (if (seq matches)
       (let [match (nth matches new-idx)]
         {:db (assoc-in db [:grep :match-index] new-idx)
          :fx [[:dispatch [:echo/message
                           (str (:buffer-name match) ":"
                                (:line-num match) ": "
                                (subs (:line-text match) 0
                                      (min 60 (count (:line-text match)))))]]]})
       {:fx [[:dispatch [:echo/message "No grep matches"]]]}))))

(rf/reg-event-fx
 :grep/goto-match
 (fn [{:keys [db]} [_]]
   "Jump to current grep match in source buffer."
   (let [matches (get-in db [:grep :matches] [])
         idx (get-in db [:grep :match-index] 0)]
     (if (and (seq matches) (< idx (count matches)))
       (let [match (nth matches idx)
             target-name (:buffer-name match)
             target-buffer (first (filter #(= (:name (val %)) target-name)
                                          (:buffers db)))]
         (if target-buffer
           {:fx [[:dispatch [:switch-buffer (key target-buffer)]]
                 [:dispatch [:echo/message
                             (str "Line " (:line-num match))]]]}
           {:fx [[:dispatch [:echo/message
                             (str "Buffer " target-name " not found")]]]}))
       {:fx [[:dispatch [:echo/message "No match to visit"]]]}))))

(rf/reg-event-fx
 :grep/rerun
 (fn [{:keys [db]} [_]]
   "Re-run the last grep."
   (let [pattern (get-in db [:grep :last-pattern])]
     (if pattern
       {:fx [[:dispatch [:grep/run pattern]]]}
       {:fx [[:dispatch [:echo/message "No previous grep to re-run"]]]}))))

;; =============================================================================
;; Grep Commands (User-facing)
;; =============================================================================

(rf/reg-event-fx
 :grep/prompt
 (fn [_ [_]]
   "Prompt for grep pattern (M-x grep)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

(rf/reg-event-fx
 :grep/lgrep
 (fn [_ [_]]
   "Local grep - search open buffers (M-x lgrep)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Local grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

(rf/reg-event-fx
 :grep/rgrep
 (fn [_ [_]]
   "Recursive grep - search all open buffers (M-x rgrep)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Recursive grep (regexp): "
                      :on-confirm [:grep/run]}]]]}))

(rf/reg-event-fx
 :grep/quit
 (fn [_ [_]]
   "Quit grep buffer."
   {:fx [[:dispatch [:kill-buffer-by-name "*grep*"]]]}))

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
