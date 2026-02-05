(ns lexicon.core.info
  "Info documentation browser (info.el).

  Implements Emacs info.el core functionality:
  - Info node navigation (next, prev, up)
  - Menu navigation
  - Cross-reference following
  - History navigation (back, forward)
  - Search within Info
  - Index lookup

  Commands:
  - info / C-h i: Open Info browser
  - Info-next (n): Go to next node
  - Info-prev (p): Go to previous node
  - Info-up (u): Go up in hierarchy
  - Info-menu (m): Select menu item
  - Info-follow-reference (f): Follow cross-reference
  - Info-history-back (l): Go back in history
  - Info-history-forward (r): Go forward in history
  - Info-index (i): Look up in index
  - Info-search (s): Search for string
  - Info-top-node (t): Go to top node
  - Info-directory (d): Go to directory node
  - quit (q): Quit Info

  Key bindings (info-mode):
  - n: Next node
  - p: Previous node
  - u: Up node
  - m: Menu
  - f: Follow reference
  - l: History back
  - r: History forward
  - i: Index
  - s: Search
  - t: Top node
  - d: Directory
  - q: Quit

  Based on Emacs lisp/info.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Info Node Structure
;; =============================================================================

(def info-directory-content
  "The top-level Info directory node."
  (str "File: dir,\tNode: Top\n\n"
       "The Lexicon Info Directory\n"
       (str/join "" (repeat 40 "*")) "\n\n"
       "This is the top of the Info tree.\n\n"
       "* Menu:\n\n"
       "Lexicon\n"
       "* Lexicon::              The Lexicon text editor.\n"
       "* Emacs Lisp::           Emacs Lisp reference.\n\n"
       "Programming\n"
       "* ClojureScript::        ClojureScript programming language.\n\n"))

(def lexicon-info-content
  "Built-in Info documentation for Lexicon."
  (str "File: lexicon,\tNode: Top,\tNext: Getting Started,\tUp: (dir)\n\n"
       "Lexicon User Manual\n"
       (str/join "" (repeat 40 "*")) "\n\n"
       "Lexicon is a faithful recreation of GNU Emacs for the modern web.\n"
       "It implements Emacs's actual architecture using modern technologies\n"
       "(Rust/WASM, ClojureScript, re-frame).\n\n"
       "* Menu:\n\n"
       "* Getting Started::     Quick start guide\n"
       "* Key Bindings::        Default key bindings\n"
       "* Commands::            Available commands\n"
       "* Customization::       Customizing Lexicon\n\n"))

(def info-nodes
  "Map of node names to their content."
  {"(dir)Top" info-directory-content
   "(lexicon)Top" lexicon-info-content
   "(lexicon)Getting Started"
   (str "File: lexicon,\tNode: Getting Started,\tNext: Key Bindings,\tPrev: Top,\tUp: Top\n\n"
        "Getting Started with Lexicon\n"
        (str/join "" (repeat 40 "=")) "\n\n"
        "Lexicon runs in your web browser. Open http://localhost:8080 to start.\n\n"
        "Basic navigation:\n"
        "  C-f / C-b    Move forward/backward one character\n"
        "  C-n / C-p    Move to next/previous line\n"
        "  C-a / C-e    Move to beginning/end of line\n"
        "  M-f / M-b    Move forward/backward one word\n\n"
        "File operations:\n"
        "  C-x C-f      Find file (open)\n"
        "  C-x C-s      Save file\n"
        "  C-x C-w      Write file (save as)\n\n"
        "Buffer operations:\n"
        "  C-x b        Switch buffer\n"
        "  C-x k        Kill buffer\n"
        "  C-x C-b      List buffers\n\n"
        "See also: *Note Key Bindings:: for complete key binding reference.\n\n")
   "(lexicon)Key Bindings"
   (str "File: lexicon,\tNode: Key Bindings,\tNext: Commands,\tPrev: Getting Started,\tUp: Top\n\n"
        "Key Bindings\n"
        (str/join "" (repeat 40 "=")) "\n\n"
        "Editing:\n"
        "  C-d          Delete character forward\n"
        "  DEL          Delete character backward\n"
        "  C-k          Kill to end of line\n"
        "  C-y          Yank (paste)\n"
        "  M-y          Yank pop (cycle kill ring)\n"
        "  C-w          Kill region\n"
        "  M-w          Copy region\n"
        "  C-/          Undo\n\n"
        "Search:\n"
        "  C-s          Incremental search forward\n"
        "  C-r          Incremental search backward\n"
        "  M-%          Query replace\n\n"
        "Windows:\n"
        "  C-x 2        Split window horizontally\n"
        "  C-x 3        Split window vertically\n"
        "  C-x 0        Delete window\n"
        "  C-x 1        Delete other windows\n"
        "  C-x o        Other window\n\n")
   "(lexicon)Commands"
   (str "File: lexicon,\tNode: Commands,\tNext: Customization,\tPrev: Key Bindings,\tUp: Top\n\n"
        "Commands\n"
        (str/join "" (repeat 40 "=")) "\n\n"
        "Lexicon provides hundreds of commands accessible via M-x.\n\n"
        "Common commands:\n"
        "  M-x grep          Search across open buffers\n"
        "  M-x occur          List lines matching regexp\n"
        "  M-x shell          Open a shell buffer\n"
        "  M-x eshell         Open an Emacs shell\n"
        "  M-x info           Open Info documentation\n"
        "  M-x ibuffer        Buffer list with filtering\n"
        "  M-x dired          Directory editor\n\n"
        "See also: *Note Customization:: for configuration options.\n\n")
   "(lexicon)Customization"
   (str "File: lexicon,\tNode: Customization,\tPrev: Commands,\tUp: Top\n\n"
        "Customization\n"
        (str/join "" (repeat 40 "=")) "\n\n"
        "Lexicon can be customized through:\n\n"
        "  1. Themes - M-x load-theme\n"
        "  2. Minor modes - Toggle features like line numbers, whitespace display\n"
        "  3. Key bindings - Customize via init file\n"
        "  4. Variables - Buffer-local and global variables\n\n"
        "The init file is loaded on startup from your browser's local storage.\n\n")
   "(elisp)Top"
   (str "File: elisp,\tNode: Top,\tUp: (dir)\n\n"
        "Emacs Lisp Reference\n"
        (str/join "" (repeat 40 "*")) "\n\n"
        "This is a placeholder for the Emacs Lisp reference manual.\n"
        "In Lexicon, the extension language is ClojureScript rather than Elisp.\n\n")})

;; =============================================================================
;; Info Navigation
;; =============================================================================

(defn- parse-header
  "Parse Info node header to extract navigation info.
   Returns {:file :node :next :prev :up}."
  [content]
  (let [first-line (first (str/split content #"\n"))
        extract (fn [key]
                  (when-let [match (re-find (re-pattern (str key ":\\s*([^,\\n]+)")) first-line)]
                    (str/trim (nth match 1))))]
    {:file (extract "File")
     :node (extract "Node")
     :next (extract "Next")
     :prev (extract "Prev")
     :up (extract "Up")}))

(defn- resolve-node-key
  "Resolve a node reference to a key in info-nodes."
  [current-file node-ref]
  (if (str/starts-with? (or node-ref "") "(")
    ;; Absolute reference like (dir) or (lexicon)Top
    (let [match (re-find #"\(([^)]+)\)(.*)" node-ref)]
      (if match
        (str "(" (nth match 1) ")" (or (nth match 2) "Top"))
        node-ref))
    ;; Relative reference - use current file
    (str "(" (or current-file "dir") ")" (or node-ref "Top"))))

;; =============================================================================
;; Info Buffer Management
;; =============================================================================

(rf/reg-event-fx
 :info/open
 (fn [{:keys [db]} [_ node-key]]
   "Open Info browser or navigate to a node."
   (let [key (or node-key "(dir)Top")
         content (get info-nodes key info-directory-content)
         ;; Check if *info* buffer exists
         existing (first (filter #(= (:name (val %)) "*info*") (:buffers db)))]
     (if existing
       ;; Update existing buffer
       (let [buffer-id (key existing)
             ^js wasm (get-in db [:buffers buffer-id :wasm-instance])]
         (when wasm
           (let [len (.-length wasm)]
             (.delete wasm 0 len)
             (.insert wasm 0 content)))
         {:db (-> db
                  (assoc-in [:buffers buffer-id :cache :text] content)
                  (assoc-in [:buffers buffer-id :cache :line-count]
                            (count (str/split content #"\n" -1)))
                  (update-in [:info :history] (fnil conj []) key)
                  (assoc-in [:info :current-node] key))
          :fx [[:dispatch [:switch-buffer buffer-id]]]})
       ;; Create new *info* buffer
       (let [buffers (:buffers db)
             buffer-id (db/next-buffer-id buffers)
             WasmGapBuffer (get-in db [:system :wasm-constructor])
             wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
             lines (str/split content #"\n" -1)
             line-count (count lines)]
         (if-not wasm-instance
           {:fx [[:dispatch [:echo/message "Error: WASM not initialized"]]]}
           {:db (-> db
                    (assoc-in [:buffers buffer-id]
                              {:id buffer-id
                               :name "*info*"
                               :wasm-instance wasm-instance
                               :file-handle nil
                               :major-mode :info-mode
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
                    (assoc-in [:info :current-node] key)
                    (assoc-in [:info :history] [key])
                    (assoc-in [:info :history-pos] 0))
            :fx [[:dispatch [:switch-buffer buffer-id]]]}))))))

;; =============================================================================
;; Navigation Events
;; =============================================================================

(rf/reg-event-fx
 :info/next-node
 (fn [{:keys [db]} [_]]
   "Go to next Info node."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         header (parse-header content)
         next-ref (:next header)]
     (if next-ref
       (let [next-key (resolve-node-key (:file header) next-ref)]
         {:fx [[:dispatch [:info/open next-key]]]})
       {:fx [[:dispatch [:echo/message "No next node"]]]}))))

(rf/reg-event-fx
 :info/prev-node
 (fn [{:keys [db]} [_]]
   "Go to previous Info node."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         header (parse-header content)
         prev-ref (:prev header)]
     (if prev-ref
       (let [prev-key (resolve-node-key (:file header) prev-ref)]
         {:fx [[:dispatch [:info/open prev-key]]]})
       {:fx [[:dispatch [:echo/message "No previous node"]]]}))))

(rf/reg-event-fx
 :info/up-node
 (fn [{:keys [db]} [_]]
   "Go up in Info node hierarchy."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         header (parse-header content)
         up-ref (:up header)]
     (if up-ref
       (let [up-key (resolve-node-key (:file header) up-ref)]
         {:fx [[:dispatch [:info/open up-key]]]})
       {:fx [[:dispatch [:echo/message "No parent node"]]]}))))

(rf/reg-event-fx
 :info/top-node
 (fn [{:keys [db]} [_]]
   "Go to top node of current Info file."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         header (parse-header content)
         file (:file header)]
     {:fx [[:dispatch [:info/open (str "(" (or file "dir") ")Top")]]]})))

(rf/reg-event-fx
 :info/directory
 (fn [_ [_]]
   "Go to Info directory node."
   {:fx [[:dispatch [:info/open "(dir)Top"]]]}))

(rf/reg-event-fx
 :info/history-back
 (fn [{:keys [db]} [_]]
   "Go back in Info history."
   (let [history (get-in db [:info :history] [])
         pos (get-in db [:info :history-pos] 0)
         new-pos (inc pos)]
     (if (< new-pos (count history))
       (let [node-key (nth history (- (count history) 1 new-pos))]
         {:db (assoc-in db [:info :history-pos] new-pos)
          :fx [[:dispatch [:info/open node-key]]]})
       {:fx [[:dispatch [:echo/message "No more history"]]]}))))

(rf/reg-event-fx
 :info/history-forward
 (fn [{:keys [db]} [_]]
   "Go forward in Info history."
   (let [pos (get-in db [:info :history-pos] 0)
         new-pos (dec pos)]
     (if (>= new-pos 0)
       (let [history (get-in db [:info :history] [])
             node-key (nth history (- (count history) 1 new-pos))]
         {:db (assoc-in db [:info :history-pos] new-pos)
          :fx [[:dispatch [:info/open node-key]]]})
       {:fx [[:dispatch [:echo/message "No forward history"]]]}))))

(rf/reg-event-fx
 :info/search
 (fn [_ [_]]
   "Search within Info."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Info search: "
                      :on-confirm [:info/search-exec]}]]]}))

(rf/reg-event-fx
 :info/search-exec
 (fn [{:keys [db]} [_ query]]
   "Execute Info search."
   (let [results (filter (fn [[_ content]]
                           (str/includes? (str/lower-case content)
                                          (str/lower-case query)))
                         info-nodes)
         first-match (first results)]
     (if first-match
       {:fx [[:dispatch [:info/open (key first-match)]]
             [:dispatch [:echo/message (str "Found: " (key first-match))]]]}
       {:fx [[:dispatch [:echo/message (str "No match for: " query)]]]}))))

(rf/reg-event-fx
 :info/quit
 (fn [_ [_]]
   "Quit Info browser."
   {:fx [[:dispatch [:kill-buffer-by-name "*info*"]]]}))

;; =============================================================================
;; Menu Navigation
;; =============================================================================

(defn- extract-menu-items
  "Extract menu items from Info node content.
   Menu items look like: * Name:: Description"
  [content]
  (let [lines (str/split content #"\n")]
    (keep (fn [line]
            (when-let [match (re-find #"^\*\s+(.+?)::" line)]
              (str/trim (nth match 1))))
          lines)))

(defn- extract-cross-references
  "Extract cross-references from Info node content.
   References look like: *Note Name:: or *note Name::"
  [content]
  (let [matches (re-seq #"\*[Nn]ote\s+(.+?)::" content)]
    (mapv (fn [match] (str/trim (nth match 1))) matches)))

(rf/reg-event-fx
 :info/menu
 (fn [{:keys [db]} [_]]
   "Select a menu item from current Info node (m)."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         items (extract-menu-items content)]
     (if (seq items)
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt (str "Menu item: (" (str/join ", " items) "): ")
                          :on-confirm [:info/menu-select]}]]]}
       {:fx [[:dispatch [:echo/message "No menu in this node"]]]}))))

(rf/reg-event-fx
 :info/menu-select
 (fn [{:keys [db]} [_ item]]
   "Navigate to selected menu item."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         header (parse-header content)
         file (:file header)
         node-key (resolve-node-key file item)]
     (if (get info-nodes node-key)
       {:fx [[:dispatch [:info/open node-key]]]}
       ;; Try with different file prefixes
       (let [alt-keys (map #(str "(" % ")" item) ["lexicon" "dir" "elisp"])
             found (first (filter #(get info-nodes %) alt-keys))]
         (if found
           {:fx [[:dispatch [:info/open found]]]}
           {:fx [[:dispatch [:echo/message
                             (str "No menu item: " item)]]]}))))))

(rf/reg-event-fx
 :info/follow-reference
 (fn [{:keys [db]} [_]]
   "Follow a cross-reference from current Info node (f)."
   (let [current-key (get-in db [:info :current-node] "(dir)Top")
         content (get info-nodes current-key "")
         refs (extract-cross-references content)]
     (if (seq refs)
       {:fx [[:dispatch [:minibuffer/activate
                         {:prompt (str "Follow xref: (" (str/join ", " refs) "): ")
                          :on-confirm [:info/menu-select]}]]]}
       {:fx [[:dispatch [:echo/message "No cross-references in this node"]]]}))))

(rf/reg-event-fx
 :info/index
 (fn [_ [_]]
   "Look up a topic in the index (i)."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Index topic: "
                      :on-confirm [:info/search-exec]}]]]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize info module and register commands."
  []
  ;; Main entry point
  (rf/dispatch [:register-command :info
                {:docstring "Open the Info documentation browser (C-h i)"
                 :interactive nil
                 :handler [:info/open]}])

  ;; Navigation commands
  (rf/dispatch [:register-command :Info-next
                {:docstring "Go to next Info node"
                 :interactive nil
                 :handler [:info/next-node]}])

  (rf/dispatch [:register-command :Info-prev
                {:docstring "Go to previous Info node"
                 :interactive nil
                 :handler [:info/prev-node]}])

  (rf/dispatch [:register-command :Info-up
                {:docstring "Go up in Info node hierarchy"
                 :interactive nil
                 :handler [:info/up-node]}])

  (rf/dispatch [:register-command :Info-top-node
                {:docstring "Go to top node of current Info file"
                 :interactive nil
                 :handler [:info/top-node]}])

  (rf/dispatch [:register-command :Info-directory
                {:docstring "Go to Info directory node"
                 :interactive nil
                 :handler [:info/directory]}])

  (rf/dispatch [:register-command :Info-history-back
                {:docstring "Go back in Info history"
                 :interactive nil
                 :handler [:info/history-back]}])

  (rf/dispatch [:register-command :Info-history-forward
                {:docstring "Go forward in Info history"
                 :interactive nil
                 :handler [:info/history-forward]}])

  (rf/dispatch [:register-command :Info-search
                {:docstring "Search within Info"
                 :interactive nil
                 :handler [:info/search]}])

  (rf/dispatch [:register-command :Info-menu
                {:docstring "Select a menu item from current Info node"
                 :interactive nil
                 :handler [:info/menu]}])

  (rf/dispatch [:register-command :Info-follow-reference
                {:docstring "Follow a cross-reference in current Info node"
                 :interactive nil
                 :handler [:info/follow-reference]}])

  (rf/dispatch [:register-command :Info-index
                {:docstring "Look up a topic in the index"
                 :interactive nil
                 :handler [:info/index]}])

  ;; Mode keybindings
  (rf/dispatch [:keymap/set-mode-key :info-mode "n" :Info-next])
  (rf/dispatch [:keymap/set-mode-key :info-mode "p" :Info-prev])
  (rf/dispatch [:keymap/set-mode-key :info-mode "u" :Info-up])
  (rf/dispatch [:keymap/set-mode-key :info-mode "m" :Info-menu])
  (rf/dispatch [:keymap/set-mode-key :info-mode "f" :Info-follow-reference])
  (rf/dispatch [:keymap/set-mode-key :info-mode "i" :Info-index])
  (rf/dispatch [:keymap/set-mode-key :info-mode "l" :Info-history-back])
  (rf/dispatch [:keymap/set-mode-key :info-mode "r" :Info-history-forward])
  (rf/dispatch [:keymap/set-mode-key :info-mode "t" :Info-top-node])
  (rf/dispatch [:keymap/set-mode-key :info-mode "d" :Info-directory])
  (rf/dispatch [:keymap/set-mode-key :info-mode "s" :Info-search])
  (rf/dispatch [:keymap/set-mode-key :info-mode "q" :info/quit]))
