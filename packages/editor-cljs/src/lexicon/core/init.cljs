(ns lexicon.core.init
  "User initialization file loading for Lexicon.

  Loads and evaluates user's init file (~/.lexicon/init.cljs) on startup.

  The init file can:
  - Define custom commands using defun
  - Set keybindings using global-set-key
  - Configure variables
  - Add hooks

  Example init file:
  ```clojure
  (ns user
    (:require [lexicon.core.core :refer [message]]))

  (message \"Loading user init file...\")

  ;; Custom command
  (defun my-hello []
    (interactive)
    (message \"Hello from custom command!\"))

  (message \"Init file loaded successfully\")
  ```

  Phase 6.5 Week 7-8"
  (:require [re-frame.core :as rf]
            [lexicon.core.eval :as eval]
            [lexicon.core.api.message :as msg]))

;; =============================================================================
;; Init File Configuration
;; =============================================================================

(def init-file-path
  "Path to user's init file.
   In a real implementation, this would expand ~ to user's home directory.
   For browser environment, we'll use localStorage or IndexedDB."
  ".lexicon/init.cljs")

(def init-file-content-key
  "LocalStorage key for storing init file content."
  "lexicon-init-file")

;; =============================================================================
;; Init File Storage (Browser-based)
;; =============================================================================

(defn get-init-file-from-storage
  "Get init file content from localStorage.

   Returns:
     Init file content as string, or nil if not found"
  []
  (try
    (when-let [content (.getItem js/localStorage init-file-content-key)]
      (when-not (empty? content)
        content))
    (catch :default e
      (js/console.warn "Failed to read init file from storage:" e)
      nil)))

(defn save-init-file-to-storage
  "Save init file content to localStorage.

   Args:
     content - Init file content as string"
  [content]
  (try
    (.setItem js/localStorage init-file-content-key content)
    true
    (catch :default e
      (js/console.error "Failed to save init file to storage:" e)
      false)))

;; =============================================================================
;; Init File Loading
;; =============================================================================

(defn load-init-file
  "Load and evaluate user's init file if it exists.

   Returns:
     Map with :success boolean and optional :error or :message"
  []
  (if-let [init-content (get-init-file-from-storage)]
    (do
      (msg/message "Loading init file...")
      (let [result (eval/eval-string init-content)]
        (if (:success result)
          (do
            (msg/message "Init file loaded successfully")
            {:success true :message "Init file loaded successfully"})
          (do
            (msg/message (str "Error loading init file: " (:error result)))
            {:success false :error (:error result)}))))
    {:success true :message "No init file found"}))

;; =============================================================================
;; Re-frame Event Handlers
;; =============================================================================

(rf/reg-event-fx
 :init/load-file
 (fn [_ [_]]
   "Load user's init file on startup.

   This is called during application initialization."
   (let [result (load-init-file)]
     (when-not (:success result)
       (js/console.error "Init file error:" (:error result)))
     {})))

(rf/reg-event-fx
 :init/save-file
 (fn [_ [_ content]]
   "Save init file content to localStorage.

   Args:
     content - Init file content as string"
   (if (save-init-file-to-storage content)
     {:fx [[:dispatch [:message "Init file saved"]]]}
     {:fx [[:dispatch [:message "Failed to save init file"]]]})))

(rf/reg-event-fx
 :init/reload-file
 (fn [_ [_]]
   "Reload init file (useful for testing changes).

   Re-evaluates the init file without restarting the editor."
   (let [result (load-init-file)]
     (if (:success result)
       {:fx [[:dispatch [:message "Init file reloaded"]]]}
       {:fx [[:dispatch [:message (str "Error reloading: " (:error result))]]]}))))

;; =============================================================================
;; Commands
;; =============================================================================

(defn init-init-commands!
  "Register init file commands. Must be called after :initialize-commands."
  []
  (rf/dispatch-sync
   [:register-command
    :load-init-file
    {:docstring "Load user's init file from storage"
     :interactive-spec nil
     :handler [:init/load-file]}])

  (rf/dispatch-sync
   [:register-command
    :reload-init-file
    {:docstring "Reload user's init file (for testing)"
     :interactive-spec nil
     :handler [:init/reload-file]}]))

;; =============================================================================
;; Example Init File Template
;; =============================================================================

(def example-init-file
  ";; Lexicon Init File
;; This file is evaluated when Lexicon starts.

(ns user
  \"User customizations for Lexicon\"
  (:require [lexicon.core.core :refer [message]]))

(message \"Loading user init file...\")

;; Example: Custom greeting command
;; (defun my-greeting []
;;   \"Display a custom greeting\"
;;   (interactive)
;;   (message \"Hello from Lexicon!\"))

;; Example: Custom keybinding
;; (global-set-key \"C-c h\" :my-greeting)

;; Example: Set variables
;; (setq fill-column 80)
;; (setq show-trailing-whitespace true)

(message \"Init file loaded successfully\")
")
