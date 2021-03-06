(ns dv.fulcro-reitit
  (:require
    [cljs.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.guardrails.core :refer [>defn => | ? >def]]
    [dv.fulcro-util :as fu]
    [goog.object :as g]
    [reitit.core :as r]
    [reitit.frontend :as rf]
    [reitit.frontend.easy :as rfe]
    [taoensso.timbre :as log]))

;; todo docs

;; in a component use (r/route-segment :tasks) to ge the fulcro segment.

;(def routes
;  [["/" {:name :root :segment ["tasks"]}]
;   ["/tasks" {:name :tasks :segment ["tasks"]}]
;   ["/calendar" {:segment ["calendar"]}
;    ["" {:name :calendar :segment [(fn [_] (tu/iso-date-str))]}]
;    ["/:date" {:name :calendar-date :segment [:date]}]]
;   ["/signup" {:name :signup :segment ["signup"]}]])
;
;(defonce router (fr/make-router SPA routes))
;; not sure how to get around having to define these so code in your
;; app doesn't have to pass in the router.
;; one idea is to put the router on the fulcro app and then
;; the calling code can pass 'this' or 'app'
;(defn change-route!
;  ([kw]
;   (fr/change-route! router kw))
;  ([kw params]
;   (fr/change-route! router kw params)))
;(defn route-segment [route-name] (fr/route-segment router route-name))
;(defn current-app-route [] (fr/current-app-route router))
;(defn current-fulcro-route [] (fr/current-fulcro-route router))
;
;(defn current-route-name [] (fr/current-route-name router))
;(defn current-route [this] (fr/current-route this))
;(defn init! [] (fr/init! router))
;(defn route-href [x] (fr/route-href x))

;; example of possible redirect logic setup:
;   ["/goals" {:name :goals :redirect-to {:route :goals-date :params (fn [] {:date (t/today)})}}]

(defrecord Router
  [app
   reitit-router
   routes-by-name
   current-fulcro-route
   redirect-loop-count
   max-redirect-loop-count])

(defn- router? [x] (instance? Router x))

(def ^{:private true} max-redirect-loop-count 10)

;;;; why not just put the state in the fulcro state atom?
;; it's a valid question - one idea is to swap directly against the
;; state atom without triggering UI updates.

;    https://book.fulcrologic.com/#_swapping_on_the_state_atom
;;;;
(defn router-state [r]
  {:app                     (:app r)
   :reitit-router           (:reitit-router r)
   :routes-by-name          (:routes-by-name r)
   :current-fulcro-route    @(:current-fulcro-route r)
   :redirect-loop-count     @(:redirect-loop-count r)
   :max-redirect-loop-count (:max-redirect-loop-count r)})

(>def ::name keyword?)
(>def ::segment (s/or :vec (s/coll-of (s/or :s string? :k keyword? :fn fn?)) :fn fn?))
(>def ::route-map (s/keys :req-un [::name ::segment] :opt-un [::redirect-to]))
(>def ::route (s/cat :s string? :r ::route-map))

(defn map-vals [f m]
  (into {} (map (juxt key (comp f val))) m))

(defn reitit-router? [x] (satisfies? r/Router x))

(>defn make-routes-by-name
  "Returns a map like: {:root {:name :root :path '/'}}"
  [router]
  [reitit-router? => (s/map-of keyword? ::route-map)]
  (let [grouped (group-by (comp :name second) (r/routes router))]
    (map-vals
      ;; takes the path string and adds it as the key :path
      (fn [[[path-str prop-map]]]
        (assoc prop-map :path path-str))
      grouped)))

(>defn make-router
  [app routes]
  ;; todo use reitit specs here for routes
  [app/fulcro-app? vector? => router?]
  (let [router         (rf/router routes)
        routes-by-name (make-routes-by-name router)]
    (->Router app router routes-by-name (volatile! []) (volatile! 0) max-redirect-loop-count)))

;; :default is special - should be ::default
;; add support for sub-apps to register with the router - they can add their data to the vector list
;; something like:
;; register-routes!
;;
;; todo document routes data format
;; example of possible redirect logic setup:
;   ["/goals" {:name :goals :redirect-to {:route :goals-date :params (fn [] {:date (t/today)})}}]

;(def router (rf/router routes))

(defn route-segment [router name]
  (if-let [segment (some-> router router-state :routes-by-name name :segment)]
    segment
    (throw (js/Error. (str "No matching fulcro segment for route: " (pr-str name))))))

(defn route-href
  ([id] (rfe/href id))
  ([id props] (rfe/href id props)))

;; todo could move this and others to impl ns

(defn construct-fulcro-segments [match-data]
  (let [segment (-> match-data :data :segment)]
    (mapv
      (fn [s] (cond-> s (fn? s) (apply [match-data])))
      segment)))

(defn current-fulcro-route [router]
  (-> router router-state :current-fulcro-route))

;; todo support query-params also, by merging

(defn fulcro-segment-from-match [{:keys [path-params] :as m}]
  (let [fulcro-segments (construct-fulcro-segments m)
        ;; fill in any dynamic path segments with their values
        target-segment  (mapv (fn [part]
                                (cond->> part (keyword? part) (get path-params)))
                              fulcro-segments)]
    target-segment))

(defn set-redirect-loop-count! [router v]
  (vreset! (:redirect-loop-count router) v))


(defn handle-redirect [router route params]
  (let [params (params)
        {:keys [max-redirect-loop-count redirect-loop-count]} (router-state router)]
    (do (log/info "redirecting to: " route " with params " params)
        (if (> redirect-loop-count max-redirect-loop-count)
          (do
            (log/error (str "The route " route " hit the max redirects limit: " max-redirect-loop-count))
            (set-redirect-loop-count! router 0))
          (do
            (vswap! (:redirect-loop-count router) inc)
            (js/setTimeout #(rfe/replace-state route params)))))))

;; Looks like the first match comes in as nil when init! is called.
;; read from the current url in that case.
;; not sure if nil is passed only if the current url doesn't match any
;; of your routes. Try it out.
(defn on-match
  ""
  [router m]
  (log/info "on-match called with: " m)
  (let [{:keys [app reitit-router]} (router-state router)
        m          (or m {:path (g/get js/location "pathname")})
        {:keys [path]} m
        has-match? (rf/match-by-path reitit-router path)]
    (log/info "router, match: " m)
    (if-not has-match?
      ;; unknown page, redirect to root
      (do
        (log/info "No fulcro route matched the current URL, changing to the default route.")
        (js/setTimeout #(rfe/push-state :default)))

      ;; route has redirect
      (if-let [{:keys [route params]} (get-in m [:data :redirect-to])]
        (handle-redirect router route params)
        (let [fulcro-segment       (fulcro-segment-from-match m)
              current-fulcro-route (:current-fulcro-route router)]
          (log/info "Invoking Fulcro change route with " fulcro-segment)
          (vreset! current-fulcro-route fulcro-segment)
          (dr/change-route! app fulcro-segment))))))

(defn current-route [this]
  (some-> (dr/current-route this this) first keyword))

(defn current-app-route [router]
  (dr/current-route (:app router)))

(defn current-route-from-url [router]
  (rf/match-by-path (:reitit-router router) (g/get js/location "pathname")))

(defn current-route-name
  "Returns the keyword name of the current route as determined by the URL path."
  [router]
  (some-> (current-route-from-url router) :data :name))

(defn route=url?*
  [router route-key params {{curr-name :name} :data curr-params :path-params}]
  (let [routes-by-name (:routes-by-name router)]
    (boolean
      (when-let [{:keys [name]} (routes-by-name route-key)]
        (and
          (= name curr-name)
          (= params curr-params))))))

(defn route=url?
  "predicate does the :key like :goals {:date \"2020-05-20\"}
  match current reitit match of the url"
  [router route-key params]
  (route=url?* router route-key params (current-route-from-url router)))

(comment (route=url? :goals {:date "2020-05-12"}))

(>defn change-route!
  ([router route-key]
   [router? keyword? => any?]
   (let [routes-by-name (:routes-by-name router)
         {:keys [name] :as route} (get routes-by-name route-key)]
     (when-not (route=url? router route-key {})
       (log/info "Changing route to: " route)
       (rfe/push-state name))))

  ([router route-key params]
   [router? keyword? map? => any?]
   (let [routes-by-name (:routes-by-name router)
         {:keys [name] :as route} (get routes-by-name route-key)]
     (when-not (route=url? router route-key params)
       (log/info "Changing route to : " route)
       (log/info "push state : " name " params: " params)
       (rfe/push-state name params)))))

;(defn change-route-to-default! [this]
;  (change-route! :default))


(defn init!
  ""
  [router]
  (log/info "Starting router.")
  (let [{:keys [app reitit-router]} (router-state router)]
    (dr/initialize! app)
    (rfe/start!
      reitit-router
      (partial on-match router)
      {:use-fragment false})))
