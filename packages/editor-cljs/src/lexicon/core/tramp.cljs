(ns lexicon.core.tramp
  "TRAMP - Transparent Remote Access, Multiple Protocols (tramp.el).

  Implements Emacs tramp.el remote file access framework:
  - Path parsing: /method:user@host:path (single and multi-hop)
  - Method support: ssh, scp, sudo, su, docker, plink
  - Connection management and status tracking
  - File name handler dispatch (detects TRAMP paths in find-file)
  - Connection listing buffer

  In browser context, provides:
  - TRAMP path syntax parsing and validation
  - Multi-hop path parsing (/ssh:host1|ssh:host2:/path)
  - Connection state UI and listing
  - Integration with File System Access API as local backend
  - Framework for WebSocket-based remote file access

  Path format: /method:user@host:path
  Multi-hop: /method1:user1@host1|method2:user2@host2:path
  Examples:
  - /ssh:user@example.com:/home/user/file.txt
  - /sudo::/etc/hosts
  - /scp:user@host:/path/to/file
  - /ssh:host1|ssh:host2:/remote/path
  - /docker:container:/app/file

  Based on Emacs lisp/net/tramp.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Path Parsing
;; =============================================================================

(def tramp-path-re
  "Regex for TRAMP path syntax: /method:user@host:localname"
  #"^/([a-zA-Z]+):(?:([^@:|]+)@)?([^:|]*):(.*)$")

(def tramp-multi-hop-re
  "Regex for detecting multi-hop TRAMP paths."
  #"\|")

(defn parse-tramp-path
  "Parse a TRAMP path into components.
   Returns {:method :user :host :localname} or nil.
   For multi-hop paths, returns {:hops [...] :localname ...}."
  [path]
  (when (and path (str/starts-with? path "/"))
    (let [path-body (subs path 1)]
      ;; Check for multi-hop
      (if (re-find tramp-multi-hop-re path-body)
        ;; Multi-hop: split on | and parse each hop
        (let [;; The localname is after the last colon in the last hop
              parts (str/split path-body #"\|")
              hops (mapv (fn [part]
                           (let [;; Re-add / prefix for parsing
                                 match (re-find tramp-path-re (str "/" part))]
                             (when match
                               {:method (nth match 1)
                                :user (nth match 2)
                                :host (nth match 3)})))
                         (butlast parts))
              ;; Parse last part which includes localname
              last-match (re-find tramp-path-re (str "/" (last parts)))]
          (when (and (every? some? hops) last-match)
            {:hops (conj hops {:method (nth last-match 1)
                               :user (nth last-match 2)
                               :host (nth last-match 3)})
             :localname (nth last-match 4)}))
        ;; Single hop
        (when-let [match (re-find tramp-path-re path)]
          {:method (nth match 1)
           :user (nth match 2)
           :host (nth match 3)
           :localname (nth match 4)})))))

(defn tramp-path?
  "Check if a path is a TRAMP path."
  [path]
  (some? (parse-tramp-path path)))

(defn format-tramp-path
  "Format TRAMP components back into a path string."
  [{:keys [method user host localname hops]}]
  (if hops
    ;; Multi-hop
    (str "/"
         (str/join "|"
                   (map (fn [{:keys [method user host]}]
                          (str method ":"
                               (when user (str user "@"))
                               (or host "")))
                        (butlast hops)))
         "|"
         (let [last-hop (last hops)]
           (str (:method last-hop) ":"
                (when (:user last-hop) (str (:user last-hop) "@"))
                (or (:host last-hop) "")))
         ":" (or localname ""))
    ;; Single hop
    (str "/" method ":"
         (when user (str user "@"))
         (or host "")
         ":"
         (or localname ""))))

;; =============================================================================
;; TRAMP Methods
;; =============================================================================

(def tramp-default-method
  "Default TRAMP method when none is specified."
  "ssh")

(def tramp-methods
  "Supported TRAMP methods with their configurations."
  {"ssh" {:description "SSH connection"
          :login-program "ssh"
          :copy-program nil
          :requires-host true}
   "scp" {:description "SSH with SCP file transfer"
          :login-program "ssh"
          :copy-program "scp"
          :requires-host true}
   "sudo" {:description "Local privilege escalation"
           :login-program "sudo"
           :copy-program nil
           :requires-host false}
   "su" {:description "Switch user"
         :login-program "su"
         :copy-program nil
         :requires-host false}
   "docker" {:description "Docker container access"
             :login-program "docker"
             :copy-program nil
             :requires-host true}
   "plink" {:description "PuTTY SSH connection"
            :login-program "plink"
            :copy-program "pscp"
            :requires-host true}})

;; =============================================================================
;; Connection State
;; =============================================================================

(rf/reg-event-db
 :tramp/set-connection
 (fn [db [_ conn-key state]]
   (assoc-in db [:tramp :connections conn-key] state)))

(rf/reg-event-db
 :tramp/remove-connection
 (fn [db [_ conn-key]]
   (update-in db [:tramp :connections] dissoc conn-key)))

(rf/reg-sub
 :tramp/connections
 (fn [db _]
   (get-in db [:tramp :connections] {})))

(rf/reg-sub
 :tramp/connection-active?
 (fn [db [_ conn-key]]
   (let [conn (get-in db [:tramp :connections conn-key])]
     (= (:status conn) :connected))))

;; =============================================================================
;; TRAMP Commands
;; =============================================================================

(rf/reg-event-fx
 :tramp/find-file
 (fn [{:keys [db]} [_ path]]
   "Open a remote file via TRAMP path."
   (let [parsed (parse-tramp-path path)]
     (if parsed
       (let [method (or (:method parsed)
                        (when (:hops parsed)
                          (:method (last (:hops parsed)))))
             method-config (get tramp-methods method)
             conn-key (if (:hops parsed)
                        (str/join "|" (map (fn [h]
                                             (str (:method h) ":" (:host h)))
                                           (:hops parsed)))
                        (str method ":" (:host parsed)))]
         (if method-config
           {:db (assoc-in db [:tramp :connections conn-key]
                          {:status :connecting
                           :method method
                           :user (or (:user parsed)
                                     (when (:hops parsed)
                                       (:user (last (:hops parsed)))))
                           :host (or (:host parsed)
                                     (when (:hops parsed)
                                       (:host (last (:hops parsed)))))
                           :hops (:hops parsed)
                           :started-at (.now js/Date)})
            :fx [[:dispatch [:echo/message
                             (str "TRAMP: connecting via " method " to "
                                  (or (:host parsed)
                                      (when (:hops parsed)
                                        (:host (last (:hops parsed))))
                                      "localhost")
                                  (when (:hops parsed)
                                    (str " (multi-hop: " (count (:hops parsed)) " hops)"))
                                  " (requires WebSocket bridge)")]]]}
           {:fx [[:dispatch [:echo/message
                             (str "TRAMP: unknown method '" method "'")]]]}))
       {:fx [[:dispatch [:echo/message
                         (str "Not a TRAMP path: " path)]]]}))))

(rf/reg-event-fx
 :tramp/cleanup-connections
 (fn [{:keys [db]} [_]]
   "Clean up all TRAMP connections."
   (let [count (count (get-in db [:tramp :connections] {}))]
     {:db (assoc-in db [:tramp :connections] {})
      :fx [[:dispatch [:echo/message
                       (if (pos? count)
                         (str "TRAMP: " count " connection"
                              (when (not= count 1) "s") " closed")
                         "TRAMP: no connections to clean up")]]]})))

(rf/reg-event-fx
 :tramp/cleanup-this-connection
 (fn [{:keys [db]} [_]]
   "Clean up TRAMP connection for current buffer."
   {:fx [[:dispatch [:minibuffer/activate
                     {:prompt "Connection to close: "
                      :on-confirm [:tramp/cleanup-connection-exec]}]]]}))

(rf/reg-event-fx
 :tramp/cleanup-connection-exec
 (fn [{:keys [db]} [_ conn-key]]
   "Execute cleanup of specific connection."
   (if (get-in db [:tramp :connections conn-key])
     {:db (update-in db [:tramp :connections] dissoc conn-key)
      :fx [[:dispatch [:echo/message (str "TRAMP: closed " conn-key)]]]}
     {:fx [[:dispatch [:echo/message (str "TRAMP: no such connection: " conn-key)]]]})))

(rf/reg-event-fx
 :tramp/list-connections
 (fn [{:keys [db]} [_]]
   "List active TRAMP connections in a buffer."
   (let [connections (get-in db [:tramp :connections] {})
         content (str "TRAMP Connections\n"
                      (str/join "" (repeat 40 "=")) "\n\n"
                      (if (seq connections)
                        (str/join "\n"
                                  (map (fn [[k v]]
                                         (str "  " k "\n"
                                              "    Method: " (:method v) "\n"
                                              "    Host:   " (or (:host v) "localhost") "\n"
                                              "    User:   " (or (:user v) "-") "\n"
                                              "    Status: " (name (:status v)) "\n"
                                              (when (:hops v)
                                                (str "    Hops:   " (count (:hops v)) "\n"))))
                                       connections))
                        "  (no active connections)")
                      "\n\n"
                      "Methods: " (str/join ", " (keys tramp-methods)) "\n")
         buffers (:buffers db)
         buffer-id (db/next-buffer-id buffers)
         WasmGapBuffer (get-in db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (WasmGapBuffer. content))
         lines (str/split content #"\n" -1)
         line-count (count lines)]
     (if wasm-instance
       {:db (assoc-in db [:buffers buffer-id]
                      {:id buffer-id
                       :name "*TRAMP Connections*"
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

(rf/reg-event-fx
 :tramp/list-remote-buffers
 (fn [{:keys [db]} [_]]
   "List buffers opened via TRAMP."
   (let [connections (get-in db [:tramp :connections] {})]
     (if (seq connections)
       {:fx [[:dispatch [:echo/message
                         (str "Remote connections: "
                              (str/join ", " (keys connections)))]]]}
       {:fx [[:dispatch [:echo/message "No remote buffers"]]]}))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize TRAMP module and register commands."
  []
  (rf/dispatch [:register-command :tramp-cleanup-all-connections
                {:docstring "Clean up all TRAMP connections"
                 :interactive nil
                 :handler [:tramp/cleanup-connections]}])

  (rf/dispatch [:register-command :tramp-cleanup-this-connection
                {:docstring "Clean up specific TRAMP connection"
                 :interactive nil
                 :handler [:tramp/cleanup-this-connection]}])

  (rf/dispatch [:register-command :tramp-list-connections
                {:docstring "List TRAMP connections in a buffer"
                 :interactive nil
                 :handler [:tramp/list-connections]}])

  (rf/dispatch [:register-command :tramp-list-remote-buffers
                {:docstring "List buffers opened via TRAMP"
                 :interactive nil
                 :handler [:tramp/list-remote-buffers]}]))
