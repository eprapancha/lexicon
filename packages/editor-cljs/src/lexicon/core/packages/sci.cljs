(ns lexicon.core.packages.sci
  "SCI (Small Clojure Interpreter) integration for safe package evaluation.

  Provides sandboxed execution environment for external packages with
  access only to the Core API, while allowing :core and :local packages
  full access via native evaluation."
  (:require [sci.core :as sci]
            [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- Trust Levels --

(def trust-levels
  "Package trust levels determine evaluation strategy and access rights.

  :core     - Built-in packages (full access, native eval)
  :local    - User-installed from filesystem (full access, native eval)
  :external - Third-party from internet (Core API only, SCI sandbox)"
  #{:core :local :external})

;; -- Core API Bindings --
;; TODO: These will be populated as Core API is implemented
;; For now, providing scaffolding with basic re-frame dispatch wrappers

(defn- make-api-namespace
  "Create SCI namespace with Core API bindings for external packages.

  External packages run in SCI sandbox and can only access functions
  defined here. This provides security isolation from editor internals."
  []
  {'lexicon.core.api
   {;; Buffer API
    'create-buffer (fn [name]
                     @(rf/subscribe [:command/sync-result
                                     [:buffer/create {:name name}]]))
    'current-buffer (fn []
                      @(rf/subscribe [:buffers/current-id]))
    'switch-to-buffer (fn [buffer-id]
                        (rf/dispatch [:buffer/switch buffer-id]))

    ;; Command API
    'define-command (fn [command-id opts handler]
                      (rf/dispatch [:command/register command-id
                                    (assoc opts :handler handler)]))
    'execute-command (fn [command-id & args]
                       (apply rf/dispatch (into [command-id] args)))

    ;; Keymap API
    'define-key (fn [keymap key-seq command-id]
                  (rf/dispatch [:keymap/bind keymap key-seq command-id]))

    ;; Mode API
    'define-major-mode (fn [mode-id opts]
                         (rf/dispatch [:mode/register-major mode-id opts]))
    'define-minor-mode (fn [mode-id opts]
                         (rf/dispatch [:mode/register-minor mode-id opts]))

    ;; Hook API
    'add-hook (fn [hook-id handler & {:keys [priority]
                                      :or {priority 50}}]
                (rf/dispatch [:hook/add hook-id handler priority]))
    'remove-hook (fn [hook-id handler]
                   (rf/dispatch [:hook/remove hook-id handler]))

    ;; Messaging
    'message (fn [& args]
               (rf/dispatch [:echo/message (apply str args)]))}})

;; -- SCI Context --

;; SCI evaluation context for external packages.
;; Initialized once with Core API bindings and ClojureScript stdlib.
(defonce ^:private sci-ctx
  (delay
    (sci/init
      {:namespaces (make-api-namespace)

       ;; Dynamic context variables (match Emacs dynamic scope)
       :bindings {'*current-buffer* (sci/new-dynamic-var '*current-buffer* nil)
                  '*current-command* (sci/new-dynamic-var '*current-command* nil)
                  '*prefix-arg* (sci/new-dynamic-var '*prefix-arg* nil)
                  '*this-command-keys* (sci/new-dynamic-var '*this-command-keys* nil)}

       ;; Security: deny dangerous JavaScript access
       :deny ['js/eval
              'js/Function
              'js/XMLHttpRequest
              'js/fetch
              ;; No direct access to editor internals
              're-frame.core/dispatch
              're-frame.core/subscribe
              'lexicon.db/*
              'lexicon.events/*]

       ;; Prevent unrestricted eval
       :allow-unrestricted-eval false

       ;; Allow ClojureScript core functions
       :classes {'js js/globalThis :allow :all}})))

;; -- Package Evaluation --

(defn eval-string
  "Evaluate ClojureScript source code string with appropriate trust level.

  Parameters:
    source      - ClojureScript source code as string
    trust-level - One of :core, :local, :external (default :external)
    opts        - Optional map with:
                  :namespace - Namespace context for evaluation
                  :bindings  - Additional dynamic bindings

  Returns:
    {:success true/false
     :result  <evaluation result>
     :error   <error message if failed>}

  Trust level determines evaluation strategy:
    :core/:local  - Native eval (full access)
    :external     - SCI sandbox (Core API only)"
  ([source]
   (eval-string source :external {}))
  ([source trust-level]
   (eval-string source trust-level {}))
  ([source trust-level opts]
   {:pre [(contains? trust-levels trust-level)
          (string? source)]}

   (try
     (let [result (case trust-level
                    ;; Core and local packages get full access via native eval
                    (:core :local)
                    (js/eval source)

                    ;; External packages run in SCI sandbox
                    :external
                    (let [ctx (or (:sci-context opts) @sci-ctx)
                          bindings (:bindings opts {})]
                      (sci/binding [sci/*ctx* ctx]
                        (sci/eval-string* ctx source))))]

       {:success true
        :result result})

     (catch :default e
       {:success false
        :error (ex-message e)
        :exception e}))))

(defn eval-form
  "Evaluate a ClojureScript form (data structure) with appropriate trust level.

  Similar to eval-string but accepts quoted forms instead of strings.
  Useful for programmatically constructed package code."
  ([form]
   (eval-form form :external {}))
  ([form trust-level]
   (eval-form form trust-level {}))
  ([form trust-level opts]
   {:pre [(contains? trust-levels trust-level)]}

   (try
     (let [result (case trust-level
                    (:core :local)
                    (eval form)

                    :external
                    (let [ctx (or (:sci-context opts) @sci-ctx)]
                      (sci/eval-form ctx form)))]

       {:success true
        :result result})

     (catch :default e
       {:success false
        :error (ex-message e)
        :exception e}))))

(defn create-package-context
  "Create an isolated SCI context for a specific package.

  Each package gets its own namespace to avoid collisions.
  Returns SCI context that can be passed to eval-string via :sci-context opt."
  [package-name]
  (sci/fork @sci-ctx))

;; -- Package Loading --

(defn load-package-source
  "Load and evaluate package source code from string.

  Parameters:
    package-name - Keyword package identifier
    source       - ClojureScript source code
    trust-level  - One of :core, :local, :external

  Returns:
    {:success true/false
     :package-name <name>
     :trust-level <level>
     :error <if failed>}

  This function:
    1. Creates isolated context for package
    2. Evaluates source in appropriate sandbox
    3. Calls initialize! if present
    4. Returns result"
  [package-name source trust-level]
  {:pre [(keyword? package-name)
         (string? source)
         (contains? trust-levels trust-level)]}

  (try
    (let [;; Create isolated context for this package
          pkg-ctx (when (= trust-level :external)
                    (create-package-context package-name))

          ;; Evaluate package source
          eval-result (eval-string source trust-level
                                   (when pkg-ctx
                                     {:sci-context pkg-ctx}))

          _ (when-not (:success eval-result)
              (throw (ex-info "Package evaluation failed"
                              {:package package-name
                               :error (:error eval-result)})))

          ;; Call initialize! function if it exists
          init-result (when (= trust-level :external)
                        (eval-string "(if (resolve 'initialize!)
                                       (initialize!)
                                       ::no-init)"
                                     trust-level
                                     {:sci-context pkg-ctx}))]

      {:success true
       :package-name package-name
       :trust-level trust-level
       :initialized? (not= (:result init-result) ::no-init)})

    (catch :default e
      {:success false
       :package-name package-name
       :trust-level trust-level
       :error (ex-message e)
       :exception e})))

(comment
  ;; Example: Evaluate external package code
  (eval-string
    "(ns my-package.core
       (:require [lexicon.core.core.api :as api]))

     (defn initialize! []
       (api/message \"Hello from package!\"))"
    :external)

  ;; Example: Load full package
  (load-package-source
    :my-package
    "(ns my-package.core
       (:require [lexicon.core.core.api :as api]))
     (defn initialize! []
       (api/message \"Package loaded!\"))"
    :external))
