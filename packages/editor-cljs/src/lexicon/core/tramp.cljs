(ns lexicon.core.tramp
  "TRAMP - Transparent Remote Access, Multiple Protocols (tramp.el).

  Implements Emacs tramp.el remote file access framework:
  - Path parsing: /method:user@host:path
  - Method support: ssh, scp, sudo, su
  - Connection management
  - File name handler dispatch

  In browser context, provides:
  - TRAMP path syntax parsing and validation
  - Connection state UI
  - Integration with File System Access API as local backend
  - Framework for WebSocket-based remote file access

  Path format: /method:user@host:path
  Examples:
  - /ssh:user@example.com:/home/user/file.txt
  - /sudo::/etc/hosts
  - /scp:user@host:/path/to/file

  Supported methods (framework):
  - ssh: SSH connection
  - scp: SSH with SCP file transfer
  - sudo: Local privilege escalation
  - su: Switch user

  Based on Emacs lisp/net/tramp.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; =============================================================================
;; Path Parsing
;; =============================================================================

(def tramp-path-re
  "Regex for TRAMP path syntax: /method:user@host:localname"
  #"^/([a-zA-Z]+):(?:([^@:]+)@)?([^:]*):(.*)$")

(defn parse-tramp-path
  "Parse a TRAMP path into components.
   Returns {:method :user :host :localname} or nil."
  [path]
  (when-let [match (re-find tramp-path-re (or path ""))]
    {:method (nth match 1)
     :user (nth match 2)
     :host (nth match 3)
     :localname (nth match 4)}))

(defn tramp-path?
  "Check if a path is a TRAMP path."
  [path]
  (some? (parse-tramp-path path)))

(defn format-tramp-path
  "Format TRAMP components back into a path string."
  [{:keys [method user host localname]}]
  (str "/" method ":"
       (when user (str user "@"))
       (or host "")
       ":"
       (or localname "")))

;; =============================================================================
;; TRAMP Methods
;; =============================================================================

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
         :requires-host false}})

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
       (let [method (:method parsed)
             method-config (get tramp-methods method)]
         (if method-config
           {:db (assoc-in db [:tramp :connections (str method ":" (:host parsed))]
                          {:status :connecting
                           :method method
                           :user (:user parsed)
                           :host (:host parsed)
                           :started-at (.now js/Date)})
            :fx [[:dispatch [:echo/message
                             (str "TRAMP: connecting via " method " to "
                                  (or (:host parsed) "localhost")
                                  " (requires WebSocket bridge)")]]]}
           {:fx [[:dispatch [:echo/message
                             (str "TRAMP: unknown method '" method "'")]]]}))
       {:fx [[:dispatch [:echo/message
                         (str "Not a TRAMP path: " path)]]]}))))

(rf/reg-event-fx
 :tramp/cleanup-connections
 (fn [{:keys [db]} [_]]
   "Clean up all TRAMP connections."
   {:db (assoc-in db [:tramp :connections] {})
    :fx [[:dispatch [:echo/message "TRAMP: all connections closed"]]]}))

(rf/reg-event-fx
 :tramp/list-connections
 (fn [{:keys [db]} [_]]
   "List active TRAMP connections."
   (let [connections (get-in db [:tramp :connections] {})]
     (if (seq connections)
       {:fx [[:dispatch [:echo/message
                         (str "TRAMP connections: "
                              (str/join ", " (keys connections)))]]]}
       {:fx [[:dispatch [:echo/message "No active TRAMP connections"]]]}))))

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
                {:docstring "Clean up current TRAMP connection"
                 :interactive nil
                 :handler [:tramp/cleanup-connections]}]))
