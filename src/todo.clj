(ns todo
  (:require [io.pedestal.http :as http]
           [io.pedestal.http.route :as route]
           [io.pedestal.test :as test]))

(defn response [status body & {:as headers}]
  {:status status :body body :headers headers})

(def ok        (partial response 200))
(def created   (partial response 201))
(def accepted  (partial response 202))

(def echo
  {:name ::echo
  	:enter (fn [context]
  	         (let [request (:request context)
  	               response (ok context)]
  	            (assoc context :response response)))})

(def routes
  (route/expand-routes
    #{["/todo"                    :post   echo :route-name :list-create]
      ["/todo"                    :get    echo :route-name :list-query-form]
      ["/todo/:list-id"           :get    echo :route-name :list-view]
      ["/todo/:list-id"           :post   echo :route-name :list-item-create]
      ["/todo/:list-id/:item-id"  :get    echo :route-name :list-item-view]
      ["/todo/:list-id/:item-id"  :put    echo :route-name :list-item-update]
      ["/todo/:list-id/:item-id"  :delete echo :route-name :list-item-delete]}))

(def service-map
  {::http/routes routes
   ::http/type   :jetty
   ::http/port   8890})

(defn start []
  (http/start (http/create-server service-map)))

(defonce server (atom nil))

(defn start-dev []
  (reset! server
          (http/start (http/create-server
                      (assoc service-map
                             ::http/join? false)))))

(defn stop-dev []
  (if (some? @server) 
    (http/stop @server)
    (println "Need to start the server before you can stop it")))

(defn restart []
  (stop-dev)
  (start-dev))

(defn test-request [verb url]
  (test/response-for (::http/service-fn @server) verb url))