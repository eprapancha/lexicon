(ns lexicon.core.xref
  "Cross-reference and definition lookup (xref.el).

  Implements Emacs xref.el core functionality:
  - xref-find-definitions (M-.): Jump to definition
  - xref-find-references: Find all references
  - xref-go-back (M-,): Return to previous location
  - xref-find-apropos: Search for pattern in definitions
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
;; Xref Commands
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

(defn- search-buffers-for-identifier
  "Search all open buffers for occurrences of identifier.
   Returns vector of {:buffer-name :line-num :line-text}."
  [db identifier]
  (let [pattern-re (try (js/RegExp. (str "\\b" identifier "\\b") "")
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
                                ;; Reset lastIndex for non-global regex
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
 (fn [{:keys [db]} [_]]
   "Search for pattern in definitions."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Xref apropos (regexp): "
                      :on-confirm [:xref/find-apropos-exec]}]]]}))

(rf/reg-event-fx
 :xref/find-apropos-exec
 (fn [{:keys [db]} [_ pattern]]
   {:fx [[:dispatch [:echo/message
                     (str "xref-find-apropos: " pattern
                          " (no backend available)")]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize xref module and register commands."
  []
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
                {:docstring "Search for pattern in definitions"
                 :interactive nil
                 :handler [:xref/find-apropos]}]))
