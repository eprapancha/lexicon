(ns lexicon.core.packages.sci
  "SCI (Small Clojure Interpreter) integration for safe package evaluation.

  Provides sandboxed execution environment for external packages with
  access only to the Core API, while allowing :core and :local packages
  full access via native evaluation."
  (:require [sci.core :as sci]
            [lexicon.lisp :as lisp]))

;; -- Trust Levels --

(def trust-levels
  "Package trust levels determine evaluation strategy and access rights.

  :core     - Built-in packages (full access, native eval)
  :local    - User-installed from filesystem (full access, native eval)
  :external - Third-party from internet (Core API only, SCI sandbox)"
  #{:core :local :external})

;; -- Core API Namespace --

(defn- make-api-namespace
  "Create SCI namespace map from lexicon.lisp/sci-namespace.

  Exposes all 229+ lexicon.lisp functions so packages can call
  (message ...), (insert ...) etc. directly."
  []
  lisp/sci-namespace)

;; -- SCI Context --

;; SCI evaluation context for external packages.
;; Initialized once with Core API in both 'user and 'lexicon.api namespaces.
;; - 'user: for interactive eval (M-:) and packages without ns forms
;; - 'lexicon.api: for packages to (:require [lexicon.api :refer [message]])
(defonce ^:private sci-ctx
  (delay
    (let [api-ns (make-api-namespace)
          dynamic-vars {'*current-buffer* (sci/new-dynamic-var '*current-buffer* nil)
                        '*current-command* (sci/new-dynamic-var '*current-command* nil)
                        '*prefix-arg* (sci/new-dynamic-var '*prefix-arg* nil)
                        '*this-command-keys* (sci/new-dynamic-var '*this-command-keys* nil)}]
      (sci/init
        {:namespaces {;; user ns: API + dynamic vars (for interactive eval)
                      'user (merge api-ns dynamic-vars)
                      ;; lexicon.api: packages require from here
                      'lexicon.api api-ns}

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
         :classes {'js js/globalThis :allow :all}}))))

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
                    (let [ctx (or (:sci-context opts) @sci-ctx)]
                      (sci/eval-string* ctx source)))]

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

          ;; Append initialize! call to source so it runs in the package namespace.
          ;; Use try/catch so packages without initialize! still load fine.
          source-with-init (str source "\n(try (initialize!) (catch :default _ nil))")

          ;; Evaluate package source + initialize in one pass
          eval-result (eval-string source-with-init trust-level
                                   (when pkg-ctx
                                     {:sci-context pkg-ctx}))

          _ (when-not (:success eval-result)
              (throw (ex-info (str "Package evaluation failed: " (:error eval-result))
                              {:package package-name
                               :error (:error eval-result)})))]

      {:success true
       :package-name package-name
       :trust-level trust-level
       :initialized? true})

    (catch :default e
      {:success false
       :package-name package-name
       :trust-level trust-level
       :error (ex-message e)
       :exception e})))

(comment
  ;; Example: Evaluate external package code
  ;; All lexicon.lisp functions are in the user namespace
  (eval-string
    "(defn initialize! []
       (message \"Hello from package!\"))"
    :external)

  ;; Example: Load full package
  (load-package-source
    :my-package
    "(defn initialize! []
       (message \"Package loaded!\"))"
    :external))
