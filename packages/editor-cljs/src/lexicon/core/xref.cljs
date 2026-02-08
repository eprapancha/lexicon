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
            [lexicon.lisp :as lisp]
            [lexicon.core.db :as db]))

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
;; Search Functions (Pure-ish, use lisp.cljs)
;; =============================================================================

(defn- search-buffers-for-pattern
  "Search all open buffers for a regex pattern.
   Returns vector of {:buffer-name :buffer-id :line-num :line-text}."
  [pattern-str]
  (let [pattern-re (try (js/RegExp. pattern-str "")
                        (catch :default _ nil))]
    (when pattern-re
      (let [buffer-ids (lisp/buffer-list-ids)]
        (reduce
         (fn [results buffer-id]
           (let [name (lisp/buffer-name-from-id buffer-id)
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
         buffer-ids)))))

(defn- search-buffers-for-identifier
  "Search all open buffers for word-boundary matches of identifier."
  [identifier]
  (search-buffers-for-pattern (str "\\b" identifier "\\b")))

;; =============================================================================
;; Xref Buffer Creation
;; =============================================================================

(defn- create-xref-buffer
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
                    "\n")
        buffers (:buffers db)
        buffer-id (db/next-buffer-id buffers)
        WasmGapBuffer (get-in db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (WasmGapBuffer. output))
        lines (str/split output #"\n" -1)
        line-count (count lines)]
    (when wasm-instance
      {:buffer-id buffer-id
       :buffer {:id buffer-id
                :name "*xref*"
                :wasm-instance wasm-instance
                :file-handle nil
                :major-mode :xref-mode
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
                        :line-count line-count}}})))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :xref/push-marker
 (fn [{:keys [_db]} [_ marker]]
   (push-marker! (:buffer-id marker) (:position marker))
   {}))

(rf/reg-event-fx
 :xref/pop-marker
 (fn [{:keys [_db]} [_]]
   (if-let [marker (pop-marker!)]
     {:fx [[:dispatch [:switch-buffer (:buffer-id marker)]]
           [:dispatch [:goto-char (:buffer-id marker) (:position marker)]]]}
     {:fx [[:dispatch [:echo/message "Xref marker stack is empty"]]]})))

;; =============================================================================
;; Xref Navigation in Results
;; =============================================================================

(rf/reg-event-fx
 :xref/next-result
 (fn [{:keys [_db]} [_]]
   "Go to next result in xref results."
   (let [{:keys [matches index]} @xref-results
         new-idx (min (dec (count matches)) (inc index))]
     (if (seq matches)
       (let [match (nth matches new-idx)]
         (reset! xref-results {:matches matches :index new-idx})
         {:fx [[:dispatch [:switch-buffer (:buffer-id match)]]
               [:dispatch [:echo/message
                           (str (:buffer-name match) ":" (:line-num match))]]]})
       {:fx [[:dispatch [:echo/message "No xref results"]]]}))))

(rf/reg-event-fx
 :xref/prev-result
 (fn [{:keys [_db]} [_]]
   "Go to previous result in xref results."
   (let [{:keys [matches index]} @xref-results
         new-idx (max 0 (dec index))]
     (if (seq matches)
       (let [match (nth matches new-idx)]
         (reset! xref-results {:matches matches :index new-idx})
         {:fx [[:dispatch [:switch-buffer (:buffer-id match)]]
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
   (let [buffer-id (lisp/current-buffer)
         cursor-pos (lisp/point)
         identifier (lisp/symbol-at-point)]
     ;; Save current position to marker stack
     (when buffer-id
       (push-marker! buffer-id cursor-pos))
     (if identifier
       (let [matches (search-buffers-for-identifier identifier)]
         (if (seq matches)
           (let [result (create-xref-buffer db identifier matches)]
             (if result
               (do
                 (reset! xref-results {:matches matches :index 0})
                 {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
                  :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                       [:dispatch [:echo/message
                                   (str (count matches) " reference"
                                        (when (not= 1 (count matches)) "s")
                                        " found for " identifier)]]]})
               {:fx [[:dispatch [:echo/message "Error creating xref buffer"]]]}))
           {:fx [[:dispatch [:echo/message
                             (str identifier ": no definitions found")]]]}))
       {:fx [[:dispatch [:echo/message "No identifier at point"]]]}))))

(rf/reg-event-fx
 :xref/find-references
 (fn [{:keys [db]} [_]]
   "Find references to identifier at point (M-?).
    Searches all open buffers for references."
   (let [identifier (lisp/symbol-at-point)]
     (if identifier
       (let [matches (search-buffers-for-identifier identifier)]
         (if (seq matches)
           (let [result (create-xref-buffer db identifier matches)]
             (if result
               (do
                 (reset! xref-results {:matches matches :index 0})
                 {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
                  :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                       [:dispatch [:echo/message
                                   (str (count matches) " reference"
                                        (when (not= 1 (count matches)) "s")
                                        " found for " identifier)]]]})
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
     (let [matches (search-buffers-for-pattern pattern)]
       (if (seq matches)
         (let [result (create-xref-buffer db pattern matches)]
           (if result
             (do
               (reset! xref-results {:matches matches :index 0})
               {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
                :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                     [:dispatch [:echo/message
                                 (str (count matches) " match"
                                      (when (not= 1 (count matches)) "es")
                                      " for " pattern)]]]})
             {:fx [[:dispatch [:echo/message "Error creating xref buffer"]]]}))
         {:fx [[:dispatch [:echo/message (str "No matches for: " pattern)]]]}))
     {:fx [[:dispatch [:echo/message "Empty pattern"]]]})))

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
     (let [matches (search-buffers-for-pattern pattern)]
       (if (seq matches)
         (let [result (create-xref-buffer db pattern matches)]
           (if result
             (do
               (reset! xref-results {:matches matches :index 0})
               {:db (assoc-in db [:buffers (:buffer-id result)] (:buffer result))
                :fx [[:dispatch [:switch-buffer (:buffer-id result)]]
                     [:dispatch [:echo/message
                                 (str (count matches) " match"
                                      (when (not= 1 (count matches)) "es")
                                      " in project for " pattern)]]]})
             {:fx [[:dispatch [:echo/message "Error creating results buffer"]]]}))
         {:fx [[:dispatch [:echo/message (str "No matches in project for: " pattern)]]]}))
     {:fx [[:dispatch [:echo/message "Empty pattern"]]]})))

(rf/reg-event-fx
 :project/switch-to-buffer
 (fn [{:keys [_db]} [_]]
   "Switch to a buffer belonging to the current project (C-x p b).
    Lists non-special buffers."
   (let [buffers (lisp/all-buffer-info)
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
 (fn [{:keys [_db]} [_ buffer-name]]
   "Execute project switch-to-buffer."
   (let [target-id (lisp/get-buffer buffer-name)]
     (if target-id
       {:fx [[:dispatch [:switch-buffer target-id]]]}
       {:fx [[:dispatch [:echo/message (str "Buffer not found: " buffer-name)]]]}))))

(rf/reg-event-fx
 :project/kill-buffers
 (fn [{:keys [_db]} [_]]
   "Kill all buffers belonging to the current project (C-x p k)."
   (let [buffers (lisp/all-buffer-info)
         file-buffers (filter #(not (str/starts-with? (:name %) "*")) buffers)
         ids (map :id file-buffers)]
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
                      "\n")
         buffer-id (db/next-buffer-id (:buffers db))
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
         lines (str/split content #"\n" -1)
         line-count (count lines)]
     (if wasm-instance
       {:db (assoc-in db [:buffers buffer-id]
                      {:id buffer-id
                       :name "*project-buffers*"
                       :wasm-instance wasm-instance
                       :file-handle nil
                       :major-mode :special-mode
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
                               :line-count line-count}})
        :fx [[:dispatch [:switch-buffer buffer-id]]]}
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
