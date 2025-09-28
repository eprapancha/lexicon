(ns lexicon.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [clojure.java.io :as io])
  (:gen-class))

(defroutes app-routes
  (GET "/" [] "Lexicon Backend Server")
  (GET "/health" [] {:status 200 :body {:status "ok"}})
  (POST "/file" request
    {:status 200 :body {:message "File endpoint - not implemented"}})
  (GET "/file" request
    {:status 200 :body {:message "File endpoint - not implemented"}})
  (route/not-found {:status 404 :body {:error "Not found"}}))

(def app
  (-> app-routes
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete :options]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
      wrap-json-body
      wrap-json-response))

(defn start-server [port]
  (jetty/run-jetty app {:port port :join? false}))

(defn -main [& args]
  (let [port (Integer/parseInt (or (first args) "3000"))]
    (println (str "Starting Lexicon backend server on port " port))
    (start-server port)))