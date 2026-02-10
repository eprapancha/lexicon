(ns lexicon.packages.tramp
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

  Based on Emacs lisp/net/tramp.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State (package-local)
;; =============================================================================

(defonce tramp-state (atom {:connections {}}))

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
;; Connection State Management
;; =============================================================================

(defn set-connection!
  "Set a TRAMP connection state."
  [conn-key state]
  (swap! tramp-state assoc-in [:connections conn-key] state))

(defn remove-connection!
  "Remove a TRAMP connection."
  [conn-key]
  (swap! tramp-state update :connections dissoc conn-key))

(defn get-connections
  "Get all TRAMP connections."
  []
  (get @tramp-state :connections {}))

(defn connection-active?
  "Check if a TRAMP connection is active."
  [conn-key]
  (let [conn (get-in @tramp-state [:connections conn-key])]
    (= (:status conn) :connected)))

;; =============================================================================
;; TRAMP Operations
;; =============================================================================

(defn find-file-tramp!
  "Open a remote file via TRAMP path."
  [path]
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
          (do
            (set-connection! conn-key
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
            (lisp/message
             (str "TRAMP: connecting via " method " to "
                  (or (:host parsed)
                      (when (:hops parsed)
                        (:host (last (:hops parsed))))
                      "localhost")
                  (when (:hops parsed)
                    (str " (multi-hop: " (count (:hops parsed)) " hops)"))
                  " (requires WebSocket bridge)")))
          (lisp/message (str "TRAMP: unknown method '" method "'"))))
      (lisp/message (str "Not a TRAMP path: " path)))))

(defn cleanup-connections!
  "Clean up all TRAMP connections."
  []
  (let [conns (get-connections)
        cnt (count conns)]
    (swap! tramp-state assoc :connections {})
    (lisp/message
     (if (pos? cnt)
       (str "TRAMP: " cnt " connection"
            (when (not= cnt 1) "s") " closed")
       "TRAMP: no connections to clean up"))))

(defn cleanup-connection!
  "Execute cleanup of specific connection."
  [conn-key]
  (if (get-in @tramp-state [:connections conn-key])
    (do
      (remove-connection! conn-key)
      (lisp/message (str "TRAMP: closed " conn-key)))
    (lisp/message (str "TRAMP: no such connection: " conn-key))))

(defn list-connections!
  "List active TRAMP connections in a buffer."
  []
  (let [connections (get-connections)
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
        existing-id (lisp/get-buffer "*TRAMP Connections*")]
    ;; Kill existing buffer if any
    (when existing-id
      (lisp/kill-buffer existing-id))
    ;; Create *TRAMP Connections* buffer
    (lisp/create-special-buffer "*TRAMP Connections*" content
                                {:major-mode :special-mode
                                 :read-only true})
    (lisp/switch-to-buffer "*TRAMP Connections*")))

(defn list-remote-buffers!
  "List buffers opened via TRAMP."
  []
  (let [connections (get-connections)]
    (if (seq connections)
      (lisp/message (str "Remote connections: "
                         (str/join ", " (keys connections))))
      (lisp/message "No remote buffers"))))

;; =============================================================================
;; Interactive Commands
;; =============================================================================

(defn tramp-cleanup-all-connections-interactive []
  (cleanup-connections!))

(defn tramp-cleanup-this-connection-interactive []
  (lisp/read-from-minibuffer
   "Connection to close: "
   cleanup-connection!))

(defn tramp-list-connections-interactive []
  (list-connections!))

(defn tramp-list-remote-buffers-interactive []
  (list-remote-buffers!))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize TRAMP module and register commands."
  []
  (lisp/define-command 'tramp-cleanup-all-connections
    tramp-cleanup-all-connections-interactive
    "Clean up all TRAMP connections")

  (lisp/define-command 'tramp-cleanup-this-connection
    tramp-cleanup-this-connection-interactive
    "Clean up specific TRAMP connection")

  (lisp/define-command 'tramp-list-connections
    tramp-list-connections-interactive
    "List TRAMP connections in a buffer")

  (lisp/define-command 'tramp-list-remote-buffers
    tramp-list-remote-buffers-interactive
    "List buffers opened via TRAMP"))
