(ns lexicon.packages.info
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

  Based on Emacs lisp/info.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce info-state (atom {:current-node "(dir)Top"
                           :history []
                           :history-pos 0}))

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
;; Info Navigation (Pure Functions)
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

;; =============================================================================
;; Info Buffer Management
;; =============================================================================

(defn info-open!
  "Open Info browser or navigate to a node."
  [node-key]
  (let [key (or node-key "(dir)Top")
        content (get info-nodes key info-directory-content)
        existing-id (lisp/get-buffer "*info*")]

    ;; Update state
    (swap! info-state (fn [state]
                        (-> state
                            (update :history (fnil conj []) key)
                            (assoc :current-node key))))

    (if existing-id
      ;; Update existing buffer - kill and recreate
      (do
        (lisp/kill-buffer existing-id)
        (lisp/create-special-buffer "*info*" content
                                    {:major-mode :info-mode
                                     :read-only true})
        (lisp/switch-to-buffer "*info*"))
      ;; Create new *info* buffer
      (do
        (lisp/create-special-buffer "*info*" content
                                    {:major-mode :info-mode
                                     :read-only true})
        (lisp/switch-to-buffer "*info*")))))

;; =============================================================================
;; Navigation Commands
;; =============================================================================

(defn info-next-node!
  "Go to next Info node."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        header (parse-header content)
        next-ref (:next header)]
    (if next-ref
      (let [next-key (resolve-node-key (:file header) next-ref)]
        (info-open! next-key))
      (lisp/message "No next node"))))

(defn info-prev-node!
  "Go to previous Info node."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        header (parse-header content)
        prev-ref (:prev header)]
    (if prev-ref
      (let [prev-key (resolve-node-key (:file header) prev-ref)]
        (info-open! prev-key))
      (lisp/message "No previous node"))))

(defn info-up-node!
  "Go up in Info node hierarchy."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        header (parse-header content)
        up-ref (:up header)]
    (if up-ref
      (let [up-key (resolve-node-key (:file header) up-ref)]
        (info-open! up-key))
      (lisp/message "No parent node"))))

(defn info-top-node!
  "Go to top node of current Info file."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        header (parse-header content)
        file (:file header)]
    (info-open! (str "(" (or file "dir") ")Top"))))

(defn info-directory!
  "Go to Info directory node."
  []
  (info-open! "(dir)Top"))

(defn info-history-back!
  "Go back in Info history."
  []
  (let [{:keys [history history-pos]} @info-state
        new-pos (inc history-pos)]
    (if (< new-pos (count history))
      (let [node-key (nth history (- (count history) 1 new-pos))]
        (swap! info-state assoc :history-pos new-pos)
        (info-open! node-key))
      (lisp/message "No more history"))))

(defn info-history-forward!
  "Go forward in Info history."
  []
  (let [{:keys [history history-pos]} @info-state
        new-pos (dec history-pos)]
    (if (>= new-pos 0)
      (let [node-key (nth history (- (count history) 1 new-pos))]
        (swap! info-state assoc :history-pos new-pos)
        (info-open! node-key))
      (lisp/message "No forward history"))))

(defn info-search-exec!
  "Execute Info search."
  [query]
  (let [results (filter (fn [[_ content]]
                          (str/includes? (str/lower-case content)
                                         (str/lower-case query)))
                        info-nodes)
        first-match (first results)]
    (if first-match
      (do
        (info-open! (key first-match))
        (lisp/message (str "Found: " (key first-match))))
      (lisp/message (str "No match for: " query)))))

(defn info-search!
  "Search within Info."
  []
  (lisp/read-from-minibuffer "Info search: " info-search-exec!))

(defn info-quit!
  "Quit Info browser."
  []
  (when-let [info-id (lisp/get-buffer "*info*")]
    (lisp/kill-buffer info-id)))

;; =============================================================================
;; Menu Navigation
;; =============================================================================

(defn info-menu-select!
  "Navigate to selected menu item."
  [item]
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        header (parse-header content)
        file (:file header)
        node-key (resolve-node-key file item)]
    (if (get info-nodes node-key)
      (info-open! node-key)
      ;; Try with different file prefixes
      (let [alt-keys (map #(str "(" % ")" item) ["lexicon" "dir" "elisp"])
            found (first (filter #(get info-nodes %) alt-keys))]
        (if found
          (info-open! found)
          (lisp/message (str "No menu item: " item)))))))

(defn info-menu!
  "Select a menu item from current Info node (m)."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        items (extract-menu-items content)]
    (if (seq items)
      (lisp/read-from-minibuffer
       (str "Menu item: (" (str/join ", " items) "): ")
       info-menu-select!)
      (lisp/message "No menu in this node"))))

(defn info-follow-reference!
  "Follow a cross-reference from current Info node (f)."
  []
  (let [current-key (:current-node @info-state)
        content (get info-nodes current-key "")
        refs (extract-cross-references content)]
    (if (seq refs)
      (lisp/read-from-minibuffer
       (str "Follow xref: (" (str/join ", " refs) "): ")
       info-menu-select!)
      (lisp/message "No cross-references in this node"))))

(defn info-index!
  "Look up a topic in the index (i)."
  []
  (lisp/read-from-minibuffer "Index topic: " info-search-exec!))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-info!
  "Initialize info module and register commands."
  []
  ;; Main entry point
  (lisp/define-command 'info
    (fn [] (info-open! nil))
    "Open the Info documentation browser (C-h i)")

  ;; Navigation commands
  (lisp/define-command 'Info-next
    info-next-node!
    "Go to next Info node")

  (lisp/define-command 'Info-prev
    info-prev-node!
    "Go to previous Info node")

  (lisp/define-command 'Info-up
    info-up-node!
    "Go up in Info node hierarchy")

  (lisp/define-command 'Info-top-node
    info-top-node!
    "Go to top node of current Info file")

  (lisp/define-command 'Info-directory
    info-directory!
    "Go to Info directory node")

  (lisp/define-command 'Info-history-back
    info-history-back!
    "Go back in Info history")

  (lisp/define-command 'Info-history-forward
    info-history-forward!
    "Go forward in Info history")

  (lisp/define-command 'Info-search
    info-search!
    "Search within Info")

  (lisp/define-command 'Info-menu
    info-menu!
    "Select a menu item from current Info node")

  (lisp/define-command 'Info-follow-reference
    info-follow-reference!
    "Follow a cross-reference in current Info node")

  (lisp/define-command 'Info-index
    info-index!
    "Look up a topic in the index")

  (lisp/define-command 'Info-quit
    info-quit!
    "Quit Info browser")

  ;; Mode keybindings
  (lisp/define-key-for-mode :info-mode "n" :Info-next)
  (lisp/define-key-for-mode :info-mode "p" :Info-prev)
  (lisp/define-key-for-mode :info-mode "u" :Info-up)
  (lisp/define-key-for-mode :info-mode "m" :Info-menu)
  (lisp/define-key-for-mode :info-mode "f" :Info-follow-reference)
  (lisp/define-key-for-mode :info-mode "i" :Info-index)
  (lisp/define-key-for-mode :info-mode "l" :Info-history-back)
  (lisp/define-key-for-mode :info-mode "r" :Info-history-forward)
  (lisp/define-key-for-mode :info-mode "t" :Info-top-node)
  (lisp/define-key-for-mode :info-mode "d" :Info-directory)
  (lisp/define-key-for-mode :info-mode "s" :Info-search)
  (lisp/define-key-for-mode :info-mode "q" :Info-quit))
