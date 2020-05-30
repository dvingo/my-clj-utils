(ns dv.pedestal-util
  (:require
    [clojure.pprint :refer [pprint]]
    [ring.util.request :as ring-req]
    [clojure.string :as string]
    [com.fulcrologic.fulcro.server.api-middleware :refer [handle-api-request]]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [hiccup.page :refer [html5]]
    [io.pedestal.http :as http]
    [io.pedestal.interceptor :as interceptor]
    [muuntaja.core :as muu]
    [reitit.http :as rhttp]
    ;; needed for specs
    reitit.http.coercion
    [reitit.http.interceptors.exception :as exception]
    [reitit.http.interceptors.multipart :as multipart]
    [reitit.http.interceptors.muuntaja :as muuntaja]
    [reitit.http.interceptors.parameters :as parameters]
    [reitit.pedestal :as rpedestal]
    [space.matterandvoid.server.crux-node :refer [crux-node]]
    [dv.tick-util :as tu]
    [space.matterandvoid.server.config :refer [config]]
    [dv.crux-ring-session-store :refer [crux-session-store]]
    [space.matterandvoid.server.pathom :refer [parser]]
    [taoensso.timbre :as log]
    [clojure.java.io :as io]
    [clojure.edn :as edn])
  (:import [java.io PushbackReader IOException]))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (PushbackReader. r)))
    (catch IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))

(defn get-js-filename []
  (:output-name (first (load-edn (io/resource "public/js/main/manifest.edn")))))
(comment (get-js-filename)

  (select-keys
    (first (load-edn (io/resource "public/js/main/manifest.edn")))
    [:module-id :name :output-name :depends-on]))

(def js-filename (get-js-filename))

;; ================================================================================
;; Dynamically generated HTML. We do this so we can safely embed the CSRF token
;; in a js var for use by the client.
;; ================================================================================
(defn index [csrf-token]
  (log/debug "Serving index.html")
  (html5
    [:html {:lang "en"}
     [:head {:lang "en"}
      [:title "Application"]
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"}]
      [:link {:href "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
              :rel  "stylesheet"}]
      [:link {:rel "shortcut icon" :href "data:image/x-icon;," :type "image/x-icon"}]
      [:script (str "var fulcro_network_csrf_token = '" csrf-token "';")]]
     [:body
      [:div#app]
      [:script {:src (str "/js/main/" js-filename)}]]]))

(>defn html-response
  [html]
  [string? => map?]
  {:status 200 :body html :headers {"Content-Type" "text/html"}})

(defn csrf-token [req] (:io.pedestal.http.csrf/anti-forgery-token req))

(defn html-handler [req]
  (do
    (html-response (index (csrf-token req)))))

;; Tell muuntaja to encode the response even if Accept header is not present.
;(def transit-interceptor
;  {:leave (fn [ctx] (assoc-in ctx [:response :muuntaja/encode] transit))})

;; If the response object contains a "Content-Type" header then muuntaja
;; will not encode the body.
;; If it is not present it will encoding using the default encoding if Accept header
;; is not present in the request or is */*
(defn api-handler [{:keys [body-params] :as req}]
  (when body-params
    (let [resp (handle-api-request
                 body-params
                 (fn [tx] (parser {:ring/request req} tx)))]
      (update resp :headers dissoc "Content-Type"))))

(comment
  "Things available in (:request ctx)
  (
  :reitit.core/match :reitit.core/router :protocol :async-supported? :cookies
  :remote-addr :servlet-response :body-params :servlet :headers :server-port
  :servlet-request :muuntaja/request :content-length :session/key :content-type
  :path-info :uri :server-name :query-string :path-params :muuntaja/response
  :body :scheme :request-method :context-path :session
  )"
  )

(def log-req-keys
  [:path-info
   ;:path-params
   ;:query-string
   ;:body-params
   :body
   ;:query-params :form-params :params
   :uri :server-name :context-path :request-method
   :session
   ])

(defn log-request-begin [req]
  (log/info "----------------------------------BEGIN-REQUEST--------------------------------------")
  (pprint
    (select-keys req log-req-keys))
  (flush))
;(dorun (map #(log/info (str % ": " (% req))) log-req-keys))

(defn log-request-end []
  (log/info "------------------------------------END-REQUEST--------------------------------------"))

(def logger {:name  ::logger
             :enter (fn [ctx]
                      (let [req     (:request ctx)
                            session (-> req :session)]
                        (log-request-begin req)
                        ;(log/info "ENTER LOG Session: ")
                        ;(pprint session)
                        ctx))
             :leave (fn [ctx]
                      (log/info "logger/leave, session in request: ")
                      (pprint (:session (:request ctx)))

                      ;(log/info "response: ")
                      ;(pprint (:response ctx))
                      (log/info "logger/leave, session in response: " (:session (:response ctx)))
                      (log-request-end)
                      ctx)})

(def transit-type "application/transit+json")

(defn make-muuntaja [dev?]
  (log/info "make-muun dev? " dev?)
  (let [muu-config
        (-> muu/default-options
          (assoc :default-format transit-type)
          (update-in
            [:formats transit-type]
            ;; add :verbose true for debugging transit
            merge
            {:decoder-opts {:verbose dev? :handlers tu/tick-transit-reader}
             :encoder-opts {:verbose dev? :handlers tu/tick-transit-writer-handler-map}}))]
    (muu/create muu-config)))

(defn rrouter [interceptors dev?]

  (rhttp/router
    [["/" html-handler]
     ["/api" api-handler]]
    {:data {:muuntaja     (make-muuntaja dev?)
            :interceptors (into [(muuntaja/format-interceptor)
                                 ;(default-response-interceptor)
                                 ;logger
                                 ;; query-params & form-params
                                 (parameters/parameters-interceptor)
                                 ;; TODO try to improve this, parsing the stack trace
                                 ;; look for tools that do it already
                                 (exception/exception-interceptor)
                                 (multipart/multipart-interceptor)
                                 ;transit-interceptor
                                 ] interceptors)}}))

(defn default-handler
  [req]
  (log/info "[default-handler]")
  (html-handler req))

(defn make-csp-policy
  [dev? font-domains style-domains]
  (cond-> {:default-src "'self'"
           :object-src  "none"
           :img-src     "'self' data:"
           :script-src  (str "'self' 'unsafe-inline'" (when dev? " 'unsafe-eval'"))
           :font-src    (string/join " " (list* "data:" font-domains))
           :style-src   (string/join " " (list* "'unsafe-inline'" style-domains))}
    ;; Allow shadow cljs web sockets
    dev? (assoc :connect-src "'self' *")))

(defn service [dev? disable-csrf? font-domains style-domains crux-node]
  {:env                  :prod
   ::http/routes         []
   ::http/resource-path  "/public"
   ::http/enable-session {:store (crux-session-store crux-node)}
   ::http/enable-csrf    (if disable-csrf? nil {})
   ::http/type           :jetty
   ::http/secure-headers {:content-security-policy-settings (make-csp-policy dev? font-domains styled-domains)}
   ::http/port           8084})

;; todo
(defn router [interceptors dev?]
  (rpedestal/routing-interceptor (rrouter interceptors dev?) default-handler))

;; todo pass in interceptors and dev?
;(defn make-service-map
;  ([] (make-service-map (service config)))
;  ([base-map]
;   (-> base-map
;     (merge {:env                   :dev
;             ::http/join?           false
;             ::http/allowed-origins {:creds true :allowed-origins (constantly true)}})
;
;     http/default-interceptors
;     ;; By default enabling csrf in pedestal also enables body parsing.
;     ;; We remove it as muuntaja does content negotiation.
;     (update ::http/interceptors
;       (fn [ints]
;         (vec (remove #(= (:name %) :io.pedestal.http.body-params/body-params) ints))))
;
;     ;; use reitit for routing
;     (rpedestal/replace-last-interceptor (router))
;     (update ::http/interceptors conj (interceptor/interceptor logger))
;     ;(update ::http/interceptors
;     ;  (fn [ceptors]
;     ;    (println "********************************************************************************")
;     ;    (println "ceptors: ")
;     ;    (println ceptors "    " (type ceptors))
;     ;    (let [r (vec (cons (interceptor/interceptor logger) ceptors))]
;     ;      (println "new ints: ")
;     ;      (pprint r)
;     ;      r)))
;     (cond-> (= (:env base-map) :dev) http/dev-interceptors)
;     ;; (doto (-> ::http/interceptors ((fn [is] (println "LAST ONE") (doall (map #(println %) is))))))
;     )))
;
;(comment (make-service-map (service config)))
