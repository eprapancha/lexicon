(ns lexicon.packages.imenu
  "Buffer structure navigation (imenu).

  Provides semantic navigation within buffers:
  - Extract buffer structure (functions, classes, sections)
  - Jump to definitions
  - Hierarchical navigation (org-mode headings, markdown sections)
  - Mode-specific extraction

  Imenu works by:
  1. Mode defines imenu-create-index-function
  2. Function parses buffer and returns index
  3. Index: [{:name 'foo' :position 123} ...] or nested
  4. User selects item from completion
  5. Point jumps to position"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.completion.metadata :as metadata]))

;; -- Imenu Index Structure --

;; Index can be:
;; - Flat: [{:name "function-name" :position 123} ...]
;; - Nested: [{:name "Section" :children [{:name "subsection" :position 456} ...]} ...]

(defn flatten-imenu-index
  "Flatten nested imenu index into flat list.

  Args:
  - index: Nested index structure
  - prefix: Current prefix for nested items (default '')

  Returns: Flat list of {:name :position} maps"
  ([index] (flatten-imenu-index index ""))
  ([index prefix]
   (mapcat (fn [item]
            (if (:children item)
              ;; Nested item - recurse with prefix
              (flatten-imenu-index (:children item)
                                  (str prefix (:name item) " > "))
              ;; Leaf item
              [(assoc item :name (str prefix (:name item)))]))
          index)))

;; -- Mode-specific Index Functions --

;; Clojure/ClojureScript imenu
(defn clojure-imenu-index
  "Create imenu index for Clojure/ClojureScript buffer.
  Extracts (defn ...), (defn- ...), (def ...), (ns ...)"
  [buffer-state]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try (.getText wasm) (catch js/Error _e ""))
               "")
        ;; Find all top-level definitions
        ;; Pattern: (defn name ...) or (def name ...)
        def-pattern #"\(def(?:n)?(?:-)?[\s\n]+([^\s\)]+)"
        matches (re-seq def-pattern text)]
    (vec (for [[match name] matches]
          (let [pos (str/index-of text match)]
            {:name name
             :position pos})))))

;; Markdown imenu
(defn markdown-imenu-index
  "Create imenu index for Markdown buffer.
  Extracts headings (# Heading, ## Subheading, etc.)"
  [buffer-state]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try (.getText wasm) (catch js/Error _e ""))
               "")
        lines (str/split-lines text)
        ;; Find all headings
        index (atom [])
        pos (atom 0)]
    (doseq [line lines]
      (when-let [[_ hashes title] (re-matches #"^(#{1,6})\s+(.+)" line)]
        (swap! index conj {:name (str hashes " " title)
                          :position @pos
                          :level (count hashes)}))
      (swap! pos + (count line) 1))
    @index))

;; Fundamental mode imenu (line numbers)
(defn fundamental-imenu-index
  "Create simple line-based index for fundamental-mode.
  Shows every 10th line."
  [buffer-state]
  (let [wasm (:wasm-instance buffer-state)
        text (if wasm
               (try (.getText wasm) (catch js/Error _e ""))
               "")
        lines (str/split-lines text)
        ;; Create index for every 10th line
        index (vec (for [i (range 0 (count lines) 10)]
                    {:name (str "Line " (inc i))
                     :position (apply + (map #(inc (count %))
                                           (take i lines)))}))]
    index))

;; -- Mode Registration --

(def imenu-functions
  "Registry of imenu-create-index-function per mode."
  {:clojure-mode clojure-imenu-index
   :clojurescript-mode clojure-imenu-index
   :emacs-lisp-mode clojure-imenu-index  ; Treat Elisp similar to Clojure for now
   :markdown-mode markdown-imenu-index
   :fundamental-mode fundamental-imenu-index})

(defn get-imenu-function
  "Get imenu function for MODE."
  [mode]
  (get imenu-functions mode fundamental-imenu-index))

;; -- Imenu Index Creation --

(defn create-imenu-index
  "Create imenu index for BUFFER-ID.

  Returns: Vector of {:name :position} maps or nil"
  [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        mode (or (:major-mode buffer) :fundamental-mode)
        imenu-fn (get-imenu-function mode)]
    (when imenu-fn
      (try
        (imenu-fn buffer)
        (catch js/Error e
          (js/console.warn "Imenu index creation failed:" e)
          nil)))))

;; -- Imenu Commands --

;; M-x imenu
(rf/reg-event-fx
  :imenu
  (fn [{:keys [db]} [_]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])
          index (create-imenu-index db buffer-id)]
      (if (seq index)
        (let [flat-index (flatten-imenu-index index)
              candidates (map :name flat-index)
              metadata (metadata/make-metadata
                        :category :imenu
                        :annotation-function nil)]
          {:fx [[:dispatch [:minibuffer/activate
                            {:prompt "Imenu: "
                             :completions candidates
                             :metadata metadata
                             :on-confirm [:imenu/goto]}]]]
           :db (assoc-in db [:imenu :current-index] flat-index)})
        {:db (assoc db :message "No imenu items in buffer")}))))

(rf/reg-event-fx
  :imenu/goto
  (fn [{:keys [db]} [_ item-name]]
    (let [index (get-in db [:imenu :current-index])
          item (first (filter #(= (:name %) item-name) index))
          buffer-id (get-in db [:editor :current-buffer-id])]
      (if item
        {:fx [[:dispatch [:buffer/goto-char buffer-id (:position item)]]]}
        {:db (assoc db :message (str "Imenu item not found: " item-name))}))))

;; -- Imenu Caching --

;; Cache imenu index per buffer (invalidate on buffer change)
(rf/reg-event-db
  :imenu/cache-index
  (fn [db [_ buffer-id index]]
    (assoc-in db [:imenu :cache buffer-id] {:index index
                                           :timestamp (js/Date.now)})))

(rf/reg-sub
  :imenu/cached-index
  (fn [db [_ buffer-id]]
    (get-in db [:imenu :cache buffer-id :index])))

;; Invalidate cache on buffer modification
(rf/reg-event-db
  :imenu/invalidate-cache
  (fn [db [_ buffer-id]]
    (update-in db [:imenu :cache] dissoc buffer-id)))

;; Hook into buffer modification to invalidate
(defn hook-buffer-modification! []
  ;; In real implementation, would hook into buffer modification events
  ;; For now, just a placeholder
  nil)

;; -- Imenu List (Buffer) --

;; Create *Imenu* buffer showing all items (like occur)
(rf/reg-event-fx
  :imenu-list
  (fn [{:keys [db]} [_]]
    (let [buffer-id (get-in db [:editor :current-buffer-id])
          source-buffer (get-in db [:buffers buffer-id])
          index (create-imenu-index db buffer-id)]
      (if (seq index)
        (let [flat-index (flatten-imenu-index index)
              ;; Create buffer content
              lines (map (fn [item]
                          (str (:name item) " [" (:position item) "]"))
                        flat-index)
              content (str/join "\n" lines)]
          {:fx [[:dispatch [:create-buffer "*Imenu*"
                           :content content
                           :major-mode :special-mode
                           :read-only true]]]})
        {:db (assoc db :message "No imenu items in buffer")}))))

;; -- Initialization --

(defn initialize-imenu! []
  ;; Hook into buffer modification
  (hook-buffer-modification!)

  ;; Initialize cache
  (rf/dispatch-sync [:imenu/init-cache]))

(rf/reg-event-db
  :imenu/init-cache
  (fn [db [_]]
    (assoc-in db [:imenu :cache] {})))

;; Auto-initialize on namespace load
(initialize-imenu!)
