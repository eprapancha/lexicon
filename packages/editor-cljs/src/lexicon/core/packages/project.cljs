(ns lexicon.core.packages.project
  "Project detection and management (project.el).

  Provides project-aware commands and utilities:
  - Automatic project detection (.git, package.json, etc.)
  - Project file listing
  - Project-scoped find-file
  - Project switching
  - Project-local settings

  Projects are detected by markers:
  - .git directory (Git repository)
  - package.json (Node.js project)
  - Cargo.toml (Rust project)
  - project.clj (Clojure/Leiningen project)
  - shadow-cljs.edn (ClojureScript project)
  - Custom markers via configuration"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [clojure.set :as set]))

;; -- Project Detection --

(def project-markers
  "File/directory markers that indicate a project root."
  #{".git"
    "package.json"
    "Cargo.toml"
    "project.clj"
    "shadow-cljs.edn"
    "deps.edn"
    ".projectile"
    "Makefile"
    "README.md"})

(defn find-project-root
  "Find project root by searching upward for markers.

  Args:
  - path: Starting directory path

  Returns: Project root path or nil"
  [path]
  ;; In browser environment, we don't have file system access
  ;; For now, return a placeholder - would integrate with backend server
  ;; or use virtual file system
  nil)

(defn detect-project-type
  "Detect project type from markers in project root.

  Args:
  - root: Project root path

  Returns: Keyword indicating project type (:git, :node, :rust, etc.)"
  [root]
  ;; Placeholder - would check which markers exist
  :unknown)

(defn current-project
  "Get current project for active buffer.

  Returns: {:root path :type keyword :name string} or nil"
  [db]
  ;; For now, return hardcoded project info
  ;; In real implementation, would detect from current buffer path
  (let [buffer-id (get-in db [:editor :current-buffer-id])
        buffer (get-in db [:buffers buffer-id])]
    (when buffer
      ;; Placeholder - assume we're in the Lexicon project
      {:root "/home/nixos/projects/lexicon"
       :type :clojurescript
       :name "lexicon"})))

;; -- Project Registry --

(defn register-project!
  "Register a project in the known projects list."
  [root type name]
  (rf/dispatch [:project/register {:root root :type type :name name}]))

(rf/reg-event-db
  :project/register
  (fn [db [_ project]]
    (let [projects (get-in db [:project :known-projects] #{})]
      (assoc-in db [:project :known-projects] (conj projects project)))))

(rf/reg-sub
  :project/known-projects
  (fn [db _]
    (get-in db [:project :known-projects] #{})))

;; -- Project Files --

(defn project-files
  "Get list of all files in PROJECT.

  Args:
  - project: Project map {:root :type :name}

  Returns: Vector of file paths"
  [project]
  ;; Placeholder - would query file system or backend
  ;; For now, return empty list
  [])

(defn project-find-file-candidates
  "Get file completion candidates for current project."
  [db]
  (if-let [project (current-project db)]
    (project-files project)
    []))

;; -- Project Commands --

;; project-find-file (C-x p f in Emacs)
(rf/reg-event-fx
  :project-find-file
  (fn [{:keys [db]} [_]]
    (if-let [project (current-project db)]
      (let [files (project-files project)
            metadata {:category :file
                      :annotation-function :file}]
        (if (seq files)
          {:fx [[:dispatch [:minibuffer/activate
                            {:prompt (str "Find file in " (:name project) ": ")
                             :completions files
                             :metadata metadata
                             :on-confirm [:project/open-file]}]]]}
          {:db (assoc db :message "No files in project")}))
      {:db (assoc db :message "Not in a project")})))

(rf/reg-event-fx
  :project/open-file
  (fn [{:keys [db]} [_ file-path]]
    {:fx [[:dispatch [:find-file file-path]]]}))

;; project-switch-project (C-x p p in Emacs)
(rf/reg-event-fx
  :project-switch-project
  (fn [{:keys [db]} [_]]
    (let [projects (get-in db [:project :known-projects] #{})
          project-names (map :name projects)
          metadata {:category :project
                    :annotation-function nil}]
      (if (seq projects)
        {:fx [[:dispatch [:minibuffer/activate
                          {:prompt "Switch to project: "
                           :completions project-names
                           :metadata metadata
                           :on-confirm [:project/switch-to]}]]]}
        {:db (assoc db :message "No known projects")}))))

(rf/reg-event-db
  :project/switch-to
  (fn [db [_ project-name]]
    ;; Find project by name
    (let [projects (get-in db [:project :known-projects] #{})
          project (first (filter #(= (:name %) project-name) projects))]
      (if project
        (-> db
            (assoc-in [:project :current-project] project)
            (assoc :message (str "Switched to project: " project-name)))
        (assoc db :message (str "Project not found: " project-name))))))

;; project-buffer-list
(rf/reg-sub
  :project/buffers
  (fn [db _]
    (if-let [project (current-project db)]
      (let [root (:root project)
            buffers (vals (:buffers db))]
        ;; Filter buffers that belong to this project
        ;; Placeholder - would check buffer file paths
        buffers)
      [])))

;; -- Project Search --

(rf/reg-event-fx
  :project-search
  (fn [{:keys [db]} [_ pattern]]
    (if-let [project (current-project db)]
      ;; Placeholder - would integrate with ripgrep or similar
      {:db (assoc db :message (str "Search not yet implemented: " pattern))}
      {:db (assoc db :message "Not in a project")})))

;; -- Project Configuration --

(defn add-project-marker!
  "Add a custom project marker (file/directory name)."
  [marker]
  (rf/dispatch [:project/add-marker marker]))

(rf/reg-event-db
  :project/add-marker
  (fn [db [_ marker]]
    (update-in db [:project :custom-markers] (fnil conj #{}) marker)))

(rf/reg-sub
  :project/markers
  (fn [db _]
    (let [custom (get-in db [:project :custom-markers] #{})]
      (set/union project-markers custom))))

;; -- Initialization --

(rf/reg-event-db
  :project/detect-current
  (fn [db [_]]
    (if-let [project (current-project db)]
      (assoc-in db [:project :current-project] project)
      db)))

(defn init!
  "Initialize project module and register commands."
  []
  ;; Register commands
  (rf/dispatch [:register-command :project-find-file
                {:docstring "Find file in current project (C-x p f)"
                 :interactive nil
                 :handler [:project-find-file]}])

  (rf/dispatch [:register-command :project-switch-project
                {:docstring "Switch to another project (C-x p p)"
                 :interactive nil
                 :handler [:project-switch-project]}])

  (rf/dispatch [:register-command :project-search
                {:docstring "Search in current project"
                 :interactive nil
                 :handler [:project-search]}])

  ;; Auto-detect current project
  (rf/dispatch [:project/detect-current]))
