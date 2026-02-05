(ns lexicon.core.xref
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

  Based on Emacs lisp/progmodes/xref.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Utility
;; =============================================================================

(defn- make-buffer
  "Create a standard read-only buffer map."
  [db buffer-name content major-mode]
  (let [buffers (:buffers db)
        buffer-id (db/next-buffer-id buffers)
        WasmGapBuffer (get-in db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
        lines (str/split content #"\n" -1)
        line-count (count lines)]
    (when wasm-instance
      {:buffer-id buffer-id
       :buffer {:id buffer-id
                :name buffer-name
                :wasm-instance wasm-instance
                :file-handle nil
                :major-mode major-mode
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
                :cache {:text content
                        :line-count line-count}}})))

;; =============================================================================
;; Xref Marker Stack
;; =============================================================================

(rf/reg-event-db
 :xref/push-marker
 (fn [db [_ marker]]
   (update-in db [:xref :marker-stack] (fnil conj []) marker)))

(rf/reg-event-fx
 :xref/pop-marker
 (fn [{:keys [db]} [_]]
   (let [stack (get-in db [:xref :marker-stack] [])
         marker (peek stack)]
     (if marker
       {:db (update-in db [:xref :marker-stack] pop)
        :fx [[:dispatch [:switch-buffer (:buffer-id marker)]]
             [:dispatch [:goto-char (:buffer-id marker) (:position marker)]]]}
       {:fx [[:dispatch [:echo/message "Xref marker stack is empty"]]]}))))

;; =============================================================================
;; Identifier & Search
;; =============================================================================

(defn- extract-identifier-at-point
  "Extract the identifier at the cursor position."
  [text cursor-pos]
  (when (and text (pos? (count text)))
    (let [safe-pos (min cursor-pos (count text))
          before (subs text (max 0 (- safe-pos 50)) safe-pos)
          after (subs text safe-pos (min (count text) (+ safe-pos 50)))
          word-before (re-find #"[\w-]+$" before)
          word-after (re-find #"^[\w-]+" after)
          id (str (or word-before "") (or word-after ""))]
      (when (seq id) id))))

(defn- search-buffers-for-pattern
  "Search all open buffers for a regex pattern.
   Returns vector of {:buffer-name :buffer-id :line-num :line-text}."
  [db pattern-str]
  (let [pattern-re (try (js/RegExp. pattern-str "")
                        (catch :default _ nil))]
    (when pattern-re
      (reduce-kv
       (fn [results buffer-id buffer]
         (let [name (:name buffer)
               wasm (:wasm-instance buffer)
               text (when wasm (try (.getText ^js wasm) (catch :default _ nil)))]
           (if (and text (not (str/starts-with? name "*")))
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
       (:buffers db)))))

(defn- search-buffers-for-identifier
  "Search all open buffers for word-boundary matches of identifier."
  [db identifier]
  (search-buffers-for-pattern db (str "\\b" identifier "\\b")))

(defn- create-xref-results-buffer
  "Create an *xref* results buffer with the given matches."
  [db identifier matches]
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
    (make-buffer db "*xref*" output :xref-mode)))

;; =============================================================================
;; Xref Navigation in Results
;; =============================================================================

(rf/reg-event-fx
 :xref/next-result
 (fn [{:keys [db]} [_]]
   "Go to next result in xref results."
   (let [results (get-in db [:xref :last-results] [])
         idx (get-in db [:xref :result-index] 0)
         new-idx (min (dec (count results)) (inc idx))]
     (if (seq results)
       (let [match (nth results new-idx)]
         {:db (assoc-in db [:xref :result-index] new-idx)
          :fx [[:dispatch [:switch-buffer (:buffer-id match)]]
               [:dispatch [:echo/message
                           (str (:buffer-name match) ":" (:line-num match))]]]})
       {:fx [[:dispatch [:echo/message "No xref results"]]]}))))

(rf/reg-event-fx
 :xref/prev-result
 (fn [{:keys [db]} [_]]
   "Go to previous result in xref results."
   (let [results (get-in db [:xref :last-results] [])
         idx (get-in db [:xref :result-index] 0)
         new-idx (max 0 (dec idx))]
     (if (seq results)
       (let [match (nth results new-idx)]
         {:db (assoc-in db [:xref :result-index] new-idx)
          :fx [[:dispatch [:switch-buffer (:buffer-id match)]]
               [:dispatch [:echo/message
                           (str (:buffer-name match) ":" (:line-num match))]]]})
       {:fx [[:dispatch [:echo/message "No xref results"]]]}))))

;; =============================================================================
;; Xref Commands
;; =============================================================================

(rf/reg-event-fx
 :xref/find-definitions
 (fn [{:keys [db]} [_]]
   "Find definitions of identifier at point (M-.).
    Searches all open buffers for the identifier."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         identifier (extract-identifier-at-point text cursor-pos)]
     ;; Save current position to marker stack
     (when buffer-id
       (rf/dispatch [:xref/push-marker {:buffer-id buffer-id
                                         :position cursor-pos}]))
     (if identifier
       (let [matches (search-buffers-for-identifier db identifier)]
         (if (seq matches)
           (let [result (create-xref-results-buffer db identifier matches)]
             (if result
               {:db (-> db
                        (assoc-in [:buffers (:buffer-id result)] (:buffer result))
                        (assoc-in [:xref :last-results] matches)
                        (assoc-in [:xref :result-index] 0))
                :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                     [:dispatch [:echo/message
                                 (str (count matches) " reference"
                                      (when (not= 1 (count matches)) "s")
                                      " found for " identifier)]]]}
               {:fx [[:dispatch [:echo/message "Error creating xref buffer"]]]}))
           {:fx [[:dispatch [:echo/message
                             (str identifier ": no definitions found")]]]}))
       {:fx [[:dispatch [:echo/message "No identifier at point"]]]}))))

(rf/reg-event-fx
 :xref/find-references
 (fn [{:keys [db]} [_]]
   "Find references to identifier at point (M-?).
    Searches all open buffers for references."
   (let [window (get-in db [:window-tree])
         buffer-id (when (= (:type window) :leaf) (:buffer-id window))
         cursor-pos (get-in db [:ui :cursor-position] 0)
         buffer (get-in db [:buffers buffer-id])
         wasm (:wasm-instance buffer)
         text (when wasm (try (.getText ^js wasm) (catch :default _ "")))
         identifier (extract-identifier-at-point text cursor-pos)]
     (if identifier
       (let [matches (search-buffers-for-identifier db identifier)]
         (if (seq matches)
           (let [result (create-xref-results-buffer db identifier matches)]
             (if result
               {:db (-> db
                        (assoc-in [:buffers (:buffer-id result)] (:buffer result))
                        (assoc-in [:xref :last-results] matches)
                        (assoc-in [:xref :result-index] 0))
                :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                     [:dispatch [:echo/message
                                 (str (count matches) " reference"
                                      (when (not= 1 (count matches)) "s")
                                      " found for " identifier)]]]}
               {:fx [[:dispatch [:echo/message "Error creating xref buffer"]]]}))
           {:fx [[:dispatch [:echo/message
                             (str identifier ": no references found")]]]}))
       {:fx [[:dispatch [:echo/message "No identifier at point"]]]}))))

(rf/reg-event-fx
 :xref/go-back
 (fn [_ [_]]
   "Return to previous location in xref marker stack (M-,)."
   {:fx [[:dispatch [:xref/pop-marker]]]}))

(rf/reg-event-fx
 :xref/find-apropos
 (fn [_ [_]]
   "Search for pattern in all open buffers."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Xref apropos (regexp): "
                      :on-confirm [:xref/find-apropos-exec]}]]]}))

(rf/reg-event-fx
 :xref/find-apropos-exec
 (fn [{:keys [db]} [_ pattern]]
   "Execute xref-find-apropos search across all open buffers."
   (if (and pattern (seq pattern))
     (let [matches (search-buffers-for-pattern db pattern)]
       (if (seq matches)
         (let [result (create-xref-results-buffer db pattern matches)]
           (if result
             {:db (-> db
                      (assoc-in [:buffers (:buffer-id result)] (:buffer result))
                      (assoc-in [:xref :last-results] matches)
                      (assoc-in [:xref :result-index] 0))
              :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                   [:dispatch [:echo/message
                               (str (count matches) " match"
                                    (when (not= 1 (count matches)) "es")
                                    " for " pattern)]]]}
             {:fx [[:dispatch [:echo/message "Error creating xref buffer"]]]}))
         {:fx [[:dispatch [:echo/message (str "No matches for: " pattern)]]]}))
     {:fx [[:dispatch [:echo/message "Empty pattern"]]]})))

;; =============================================================================
;; Project Commands (project.el integration)
;; =============================================================================

(defn- infer-project-root
  "Infer project root from common path prefix of open buffers."
  [db]
  (let [buffers (vals (:buffers db))
        paths (keep :file-name buffers)]
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

(rf/reg-event-fx
 :project/find-regexp
 (fn [_ [_]]
   "Search for regexp in project files (C-x p g).
    Searches all open buffers."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Find regexp in project: "
                      :on-confirm [:project/find-regexp-exec]}]]]}))

(rf/reg-event-fx
 :project/find-regexp-exec
 (fn [{:keys [db]} [_ pattern]]
   "Execute project-find-regexp across all open buffers."
   (if (and pattern (seq pattern))
     (let [matches (search-buffers-for-pattern db pattern)]
       (if (seq matches)
         (let [result (create-xref-results-buffer db pattern matches)]
           (if result
             {:db (-> db
                      (assoc-in [:buffers (:buffer-id result)] (:buffer result))
                      (assoc-in [:xref :last-results] matches)
                      (assoc-in [:xref :result-index] 0))
              :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                   [:dispatch [:echo/message
                               (str (count matches) " match"
                                    (when (not= 1 (count matches)) "es")
                                    " in project for " pattern)]]]}
             {:fx [[:dispatch [:echo/message "Error creating results buffer"]]]}))
         {:fx [[:dispatch [:echo/message (str "No matches in project for: " pattern)]]]}))
     {:fx [[:dispatch [:echo/message "Empty pattern"]]]})))

(rf/reg-event-fx
 :project/switch-to-buffer
 (fn [{:keys [db]} [_]]
   "Switch to a buffer belonging to the current project (C-x p b).
    Lists non-special buffers."
   (let [buffers (vals (:buffers db))
         file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
         names (map :name file-buffers)]
     (if (seq names)
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt "Switch to project buffer: "
                          :completions (vec names)
                          :on-confirm [:project/switch-to-buffer-exec]}]]]}
       {:fx [[:dispatch [:echo/message "No project buffers"]]]}))))

(rf/reg-event-fx
 :project/switch-to-buffer-exec
 (fn [{:keys [db]} [_ buffer-name]]
   "Execute project switch-to-buffer."
   (let [match (first (filter (fn [[_id buf]] (= (:name buf) buffer-name))
                              (:buffers db)))]
     (if match
       {:fx [[:dispatch [:switch-buffer (key match)]]]}
       {:fx [[:dispatch [:echo/message (str "Buffer not found: " buffer-name)]]]}))))

(rf/reg-event-fx
 :project/kill-buffers
 (fn [{:keys [db]} [_]]
   "Kill all buffers belonging to the current project (C-x p k)."
   (let [buffers (:buffers db)
         file-buffers (filter (fn [[_id buf]]
                                (not (str/starts-with? (:name buf) "*")))
                              buffers)
         ids (map first file-buffers)]
     (if (seq ids)
       {:fx (vec (concat
                  (map (fn [id] [:dispatch [:kill-buffer id]]) ids)
                  [[:dispatch [:echo/message
                               (str "Killed " (count ids) " project buffer"
                                    (when (not= 1 (count ids)) "s"))]]]))}
       {:fx [[:dispatch [:echo/message "No project buffers to kill"]]]}))))

(rf/reg-event-fx
 :project/list-buffers
 (fn [{:keys [db]} [_]]
   "List buffers belonging to the current project."
   (let [root (infer-project-root db)
         buffers (vals (:buffers db))
         file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
         content (str "Project buffers (root: " root ")\n\n"
                      (if (seq file-buffers)
                        (str/join "\n" (map (fn [buf]
                                              (str "  " (:name buf)
                                                   (when (:is-modified? buf) " [modified]")))
                                            file-buffers))
                        "(no project buffers)")
                      "\n")
         result (make-buffer db "*project-buffers*" content :special-mode)]
     (if result
       {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
        :fx [[:dispatch [:switch-buffer (:buffer-id result)]]]}
       {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize xref and project modules."
  []
  ;; Xref commands
  (rf/dispatch [:register-command :xref-find-definitions
                {:docstring "Find definitions of identifier at point (M-.)"
                 :interactive nil
                 :handler [:xref/find-definitions]}])

  (rf/dispatch [:register-command :xref-find-references
                {:docstring "Find all references to identifier at point (M-?)"
                 :interactive nil
                 :handler [:xref/find-references]}])

  (rf/dispatch [:register-command :xref-go-back
                {:docstring "Return to previous location (M-,)"
                 :interactive nil
                 :handler [:xref/go-back]}])

  (rf/dispatch [:register-command :xref-find-apropos
                {:docstring "Search for pattern across all open buffers"
                 :interactive nil
                 :handler [:xref/find-apropos]}])

  ;; Xref results navigation
  (rf/dispatch [:register-command :xref-next-result
                {:docstring "Go to next xref result"
                 :interactive nil
                 :handler [:xref/next-result]}])

  (rf/dispatch [:register-command :xref-prev-result
                {:docstring "Go to previous xref result"
                 :interactive nil
                 :handler [:xref/prev-result]}])

  ;; xref-mode keybindings
  (rf/dispatch [:keymap/set-mode-key :xref-mode "n" :xref-next-result])
  (rf/dispatch [:keymap/set-mode-key :xref-mode "p" :xref-prev-result])
  (rf/dispatch [:keymap/set-mode-key :xref-mode "q" :quit-window])

  ;; Project commands
  (rf/dispatch [:register-command :project-find-regexp
                {:docstring "Search for regexp in project buffers (C-x p g)"
                 :interactive nil
                 :handler [:project/find-regexp]}])

  (rf/dispatch [:register-command :project-switch-to-buffer
                {:docstring "Switch to a project buffer (C-x p b)"
                 :interactive nil
                 :handler [:project/switch-to-buffer]}])

  (rf/dispatch [:register-command :project-kill-buffers
                {:docstring "Kill all project buffers (C-x p k)"
                 :interactive nil
                 :handler [:project/kill-buffers]}])

  (rf/dispatch [:register-command :project-list-buffers
                {:docstring "List project buffers"
                 :interactive nil
                 :handler [:project/list-buffers]}]))
