(ns lexicon.core.packages.loader
  "Package loading infrastructure for local filesystem packages.

  Handles:
  - Reading package metadata (package.edn)
  - Schema validation
  - Source file loading
  - Package lifecycle management (load/unload/reload)
  - Integration with SCI evaluation"
  (:require [lexicon.core.packages.sci :as sci]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs.reader :as reader]))

;; -- Schema Validation --

(def required-fields
  "Required fields in package.edn"
  #{:name :version :description :entry :lexicon-version})

(defn validate-package-metadata
  "Validate package.edn schema.

  Returns:
    {:valid? true/false
     :errors [error messages]}

  Required fields:
    :name - String package name
    :version - String semantic version
    :description - String description
    :entry - Symbol entry namespace
    :lexicon-version - String version requirement"
  [metadata]
  (let [errors (cond-> []
                 (not (map? metadata))
                 (conj "package.edn must be a map")

                 (not-every? #(contains? metadata %) required-fields)
                 (conj (str "Missing required fields: "
                           (str/join ", " (remove #(contains? metadata %)
                                                 required-fields))))

                 (and (:name metadata) (not (string? (:name metadata))))
                 (conj ":name must be a string")

                 (and (:version metadata) (not (string? (:version metadata))))
                 (conj ":version must be a string")

                 (and (:entry metadata) (not (symbol? (:entry metadata))))
                 (conj ":entry must be a symbol")

                 (and (:lexicon-version metadata)
                      (not (string? (:lexicon-version metadata))))
                 (conj ":lexicon-version must be a string")

                 (and (:dependencies metadata)
                      (not (or (vector? (:dependencies metadata))
                              (nil? (:dependencies metadata)))))
                 (conj ":dependencies must be a vector or nil"))]

    {:valid? (empty? errors)
     :errors errors}))

;; -- File System Operations --

(defn read-package-edn
  "Read and parse package.edn from package directory.

  Parameters:
    package-dir - Path to package directory (e.g., 'packages/my-package')

  Returns:
    {:success true/false
     :metadata <parsed edn map>
     :error <error message if failed>}"
  [package-dir]
  (try
    (let [edn-path (str package-dir "/package.edn")
          ;; In browser, we'll need to fetch via HTTP or have it pre-loaded
          ;; For now, this is a placeholder - actual impl will use js/fetch
          content (js/fetch edn-path)
          metadata (reader/read-string content)]

      (let [validation (validate-package-metadata metadata)]
        (if (:valid? validation)
          {:success true
           :metadata metadata}
          {:success false
           :error (str "Invalid package.edn: " (str/join ", " (:errors validation)))})))

    (catch :default e
      {:success false
       :error (str "Failed to read package.edn: " (ex-message e))})))

(defn load-source-file
  "Load ClojureScript source file from package directory.

  Parameters:
    package-dir - Path to package directory
    namespace-sym - Namespace symbol to load

  Returns:
    {:success true/false
     :source <source code string>
     :error <error message if failed>}"
  [package-dir namespace-sym]
  (try
    (let [;; Convert namespace to file path: my.package.core -> my/package/core.cljs
          ns-path (-> (str namespace-sym)
                     (str/replace "." "/")
                     (str/replace "-" "_"))
          file-path (str package-dir "/src/" ns-path ".cljs")
          ;; In browser, fetch source via HTTP
          source (js/fetch file-path)]

      {:success true
       :source source})

    (catch :default e
      {:success false
       :error (str "Failed to load source file: " (ex-message e))})))

;; -- Package Loading --

(defn load-package-from-dir
  "Load a package from local directory.

  This is the main entry point for package loading:
  1. Read package.edn
  2. Validate metadata
  3. Load entry namespace source
  4. Evaluate with appropriate trust level
  5. Call initialize!
  6. Track in app-db

  Parameters:
    package-dir - Path to package directory
    opts - Optional map:
           :trust-level - :core, :local, or :external (default :local)

  Returns:
    {:success true/false
     :package-name <name>
     :error <if failed>}"
  ([package-dir]
   (load-package-from-dir package-dir {:trust-level :local}))
  ([package-dir {:keys [trust-level] :or {trust-level :local}}]
   (try
     ;; Step 1: Read package.edn
     (let [metadata-result (read-package-edn package-dir)
           _ (when-not (:success metadata-result)
               (throw (ex-info "Failed to read package metadata"
                              {:error (:error metadata-result)})))

           metadata (:metadata metadata-result)
           package-name (:name metadata)
           entry-ns (:entry metadata)

           ;; Step 2: Load entry namespace source
           source-result (load-source-file package-dir entry-ns)
           _ (when-not (:success source-result)
               (throw (ex-info "Failed to load package source"
                              {:error (:error source-result)})))

           source (:source source-result)

           ;; Step 3: Evaluate package source with SCI
           eval-result (sci/load-package-source
                         (keyword package-name)
                         source
                         trust-level)

           _ (when-not (:success eval-result)
               (throw (ex-info "Package evaluation failed"
                              {:error (:error eval-result)})))

           ;; Step 4: Register in app-db
           package-info (assoc metadata
                          :package-dir package-dir
                          :trust-level trust-level
                          :loaded? true
                          :load-time (js/Date.now))]

       ;; Dispatch event to track package
       (rf/dispatch [:packages/loaded package-name package-info])

       {:success true
        :package-name package-name
        :metadata metadata})

     (catch :default e
       {:success false
        :error (ex-message e)
        :exception e}))))

(defn unload-package
  "Unload a package and call its cleanup! function.

  Parameters:
    package-name - Keyword package identifier

  Returns:
    {:success true/false
     :error <if failed>}"
  [package-name]
  (try
    (let [;; Get package info from app-db
          package-info @(rf/subscribe [:packages/get package-name])

          _ (when-not package-info
              (throw (ex-info "Package not found" {:package package-name})))

          _ (when-not (:loaded? package-info)
              (throw (ex-info "Package not loaded" {:package package-name})))

          entry-ns (:entry (:metadata package-info))
          trust-level (:trust-level package-info)

          ;; Call cleanup! if it exists
          cleanup-source (str "(when (resolve 'cleanup!) (cleanup!))")
          cleanup-result (sci/eval-string cleanup-source trust-level)]

      ;; Mark as unloaded in app-db
      (rf/dispatch [:packages/unloaded package-name])

      {:success true
       :package-name package-name
       :cleaned-up? (:success cleanup-result)})

    (catch :default e
      {:success false
       :package-name package-name
       :error (ex-message e)})))

(defn reload-package
  "Reload a package (unload + load).

  Useful for development - reload package after editing source."
  [package-dir]
  (try
    ;; First read metadata to get package name
    (let [metadata-result (read-package-edn package-dir)
          _ (when-not (:success metadata-result)
              (throw (ex-info "Cannot reload: failed to read metadata"
                             {:error (:error metadata-result)})))

          package-name (:name (:metadata metadata-result))

          ;; Unload if currently loaded
          _ (when @(rf/subscribe [:packages/loaded? package-name])
              (unload-package (keyword package-name)))

          ;; Load fresh
          load-result (load-package-from-dir package-dir)]

      load-result)

    (catch :default e
      {:success false
       :error (ex-message e)})))

;; -- Re-frame Events --

(rf/reg-event-db
  :packages/loaded
  (fn [db [_ package-name package-info]]
    "Record that a package has been loaded"
    (-> db
        (assoc-in [:packages package-name] package-info)
        (assoc-in [:packages package-name :loaded?] true))))

(rf/reg-event-db
  :packages/unloaded
  (fn [db [_ package-name]]
    "Record that a package has been unloaded"
    (assoc-in db [:packages package-name :loaded?] false)))

(rf/reg-event-fx
  :packages/load
  (fn [{:keys [db]} [_ package-dir opts]]
    "Load a package from directory (with side effects)"
    (let [result (load-package-from-dir package-dir opts)]
      (if (:success result)
        {:db db
         :dispatch [:echo/message (str "Loaded package: " (:package-name result))]}
        {:db db
         :dispatch [:show-error (str "Failed to load package: " (:error result))]}))))

(rf/reg-event-fx
  :packages/unload
  (fn [{:keys [db]} [_ package-name]]
    "Unload a package (with side effects)"
    (let [result (unload-package package-name)]
      (if (:success result)
        {:db db
         :dispatch [:echo/message (str "Unloaded package: " package-name)]}
        {:db db
         :dispatch [:show-error (str "Failed to unload package: " (:error result))]}))))

(rf/reg-event-fx
  :packages/reload
  (fn [{:keys [db]} [_ package-dir]]
    "Reload a package (with side effects)"
    (let [result (reload-package package-dir)]
      (if (:success result)
        {:db db
         :dispatch [:echo/message (str "Reloaded package: " (:package-name result))]}
        {:db db
         :dispatch [:show-error (str "Failed to reload package: " (:error result))]}))))

;; -- Subscriptions --

(rf/reg-sub
  :packages/all
  (fn [db _]
    "Get all package info from db"
    (:packages db {})))

(rf/reg-sub
  :packages/loaded?
  (fn [db [_ package-name]]
    "Check if a package is loaded"
    (get-in db [:packages package-name :loaded?] false)))

(rf/reg-sub
  :packages/get
  (fn [db [_ package-name]]
    "Get package info by name"
    (get-in db [:packages package-name])))

(rf/reg-sub
  :packages/list-loaded
  (fn [db _]
    "Get list of loaded packages"
    (->> (:packages db {})
         (filter (fn [[_ info]] (:loaded? info)))
         (map first)
         vec)))

(comment
  ;; Example usage:

  ;; Load package from local directory
  (load-package-from-dir "packages/lexicon-test-package"
                         {:trust-level :local})

  ;; Load as external (sandboxed)
  (load-package-from-dir "packages/vertico"
                         {:trust-level :external})

  ;; Unload package
  (unload-package :lexicon-test-package)

  ;; Reload package during development
  (reload-package "packages/lexicon-test-package")

  ;; Via re-frame events
  (rf/dispatch [:packages/load "packages/lexicon-test-package" {:trust-level :local}])
  (rf/dispatch [:packages/unload :lexicon-test-package])
  (rf/dispatch [:packages/reload "packages/lexicon-test-package"])

  ;; Query loaded packages
  @(rf/subscribe [:packages/list-loaded])
  @(rf/subscribe [:packages/get :lexicon-test-package]))
