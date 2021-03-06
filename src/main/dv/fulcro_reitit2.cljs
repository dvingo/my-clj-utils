(ns dv.fulcro-reitit2
  (:require
    [cljs.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as c]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
    [com.fulcrologic.guardrails.core :refer [>defn => | ? >def]]
    [dv.fulcro-util :as fu]
    [dv.fulcro-util :as fu]
    [goog.object :as g]
    [reitit.core :as r]
    [reitit.frontend :as rf]
    [reitit.frontend.easy :as rfe]
    [taoensso.timbre :as log]))

(defn reitit-router? [x] (satisfies? r/Router x))

(>def ::name keyword?)
(>def ::segment (s/coll-of (s/or :s string? :k keyword? :fn fn?) :type vector?))
(>def ::route-map (s/keys :req-un [::name ::segment] :opt-un [::redirect-to]))
(>def ::route (s/cat :s string? :r ::route-map))

(>def ::reitit-router reitit-router?)
(>def ::routes-by-name map?)
(>def ::current-fulcro-route vector?)
(>def ::redirect-loop-count integer?)

(def ^:dynamic *max-redirect-loop-count* 10)
(>def ::max-redirect-loop-count (s/and integer? #(<= % *max-redirect-loop-count*)))

(>def ::router-state
  (s/keys :req-un
    [::reitit-router
     ::routes-by-name
     ::current-fulcro-route
     ::current-fulcro-route
     ::redirect-loop-count
     ::max-redirect-loop-count]))

(def router-state-keys
  [:reitit-router
   :routes-by-name
   :current-fulcro-route
   :current-fulcro-route
   :redirect-loop-count
   :max-redirect-loop-count])

;; leaving so it's easier to swap out how state is stored.
(>def ::state-arg app/fulcro-app?)

(>defn router-path
  ([] [=> (s/tuple qualified-keyword? keyword?)]
   [::router :router])
  ([prop]
   [keyword? => (s/tuple qualified-keyword? keyword? keyword?)]
   [::router :router prop]))

(defn map-vals [f m]
  (into {} (map (juxt key (comp f val))) m))

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

(defn assoc-router-state [s router-state]
  (let [{:keys [reitit-router routes-by-name current-fulcro-route
                max-redirect-loop-count
                redirect-loop-count]} router-state]
    (-> s
      (assoc-in [::router :router :reitit-router] reitit-router)
      (assoc-in [::router :router :routes-by-name] routes-by-name)
      (assoc-in [::router :router :current-fulcro-route] current-fulcro-route)
      (assoc-in [::router :router :redirect-loop-count] redirect-loop-count)
      (assoc-in [::router :router :max-redirect-loop-count] max-redirect-loop-count))))

(defn initial-router-state
  "Takes a map - your app state atom's value - assoc-in router state needed to work."
  [routes]
  (let [reitit-router (rf/router routes)]
    {:reitit-router           reitit-router
     :routes-by-name          (make-routes-by-name reitit-router)
     :current-fulcro-route    []
     :redirect-loop-count     0
     :max-redirect-loop-count *max-redirect-loop-count*}))

(comment
  (let [routes
        [["/" {:name :root :segment ["tasks"]}]
         ["/tasks" {:name :tasks :segment ["tasks"]}]
         ["/calendar" {:segment ["calendar"]}
          ;; showing default values
          ["" {:name :calendar :segment [(fn [_] "202020")]}]
          ["/:date" {:name :calendar-date :segment [:date]}]]
         ["/signup" {:name :signup :segment ["signup"]}]]]
    (init-router-state {} routes)))

(defn router-state* [app]
  (-> app ::app/runtime-atom deref (get-in (router-path))))

(>defn router-registered?
  [app]
  [app/fulcro-app? => boolean?]
  (boolean (router-state* app)))

(>defn router-state
  ([app]
   [app/fulcro-app? => ::router-state]
   (if (router-registered? app)
     (-> app
       router-state*
       (select-keys router-state-keys))
     (throw (js/Error. "No router registered on fulcro app."))))
  ([app p]
   [app/fulcro-app? keyword? => any?]
   (if (router-registered? app)
     (get (router-state* app) p)
     (throw (js/Error. "No router registered on fulcro app.")))))

(defn route-segment
  [app name]
  (if-let [segment (some-> app (router-state :routes-by-name) name :segment)]
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

(>defn current-fulcro-route
  [app]
  [app/fulcro-app? => vector?]
  (-> app router-state :current-fulcro-route))

;; todo support query-params also, by merging

(defn fulcro-segment-from-match
  [{:keys [path-params query-params] :as m}]
  (let [fulcro-segments (construct-fulcro-segments m)
        ;; fill in any dynamic path segments with their values from the reitit match
        target-segment  (mapv (fn [part]
                                (if (keyword? part)
                                  (or (part path-params) (part query-params))
                                  part))
                              fulcro-segments)]
    (log/info "Target segment: " target-segment)
    target-segment))

(defn set-router-state! [app prop v]
  (swap! (::app/runtime-atom app)
    (fn [s] (assoc-in s (router-path prop) v))))

(defn update-router-state!
  [app prop f]
  (swap! (::app/runtime-atom app)
    (fn [s] (update-in s (router-path prop) f))))

(defn handle-redirect
  [app route params]
  (let [params (params)
        {:keys [max-redirect-loop-count redirect-loop-count]} (router-state app)]
    (do (log/info "redirecting to: " route " with params " params)
        (if (> redirect-loop-count max-redirect-loop-count)
          (do
            (log/error (str "The route " route " hit the max redirects limit: " max-redirect-loop-count))
            (set-router-state! app :redirect-loop-count 0))
          (do
            (update-router-state! app :redirect-loop-count inc)
            (js/setTimeout #(rfe/replace-state route params)))))))

;; Looks like the first match comes in as nil when init! is called.
;; read from the current url in that case.
;; not sure if nil is passed only if the current url doesn't match any
;; of your routes. Try it out.
(defn on-match
  ""
  [app m]
  (log/info "on-match called with: " m)
  (let [{:keys [reitit-router]} (router-state app)
        {:keys [path] :as m} (or m {:path (g/get js/location "pathname")})
        has-match? (rf/match-by-path reitit-router path)]

    (log/info "router, match: " m)
    (if has-match?
      (if-let [{:keys [route params]} (get-in m [:data :redirect-to])]
        (handle-redirect app route params)

        ;; Path matched
        (let [fulcro-segment (fulcro-segment-from-match m)]
          (log/info "Invoking Fulcro change route with " fulcro-segment)
          (set-router-state! app :current-fulcro-route fulcro-segment)
          (dr/change-route! app fulcro-segment)))
      ;; unknown page, redirect to root
      (do
        (log/info "No fulcro route matched the current URL, changing to the default route.")
        (js/setTimeout #(rfe/push-state :default))))))

(defn current-route [this]
  (some-> (dr/current-route this this) first keyword))

(defn current-app-route [app]
  (dr/current-route app))

(defn current-route-from-url [app]
  (rf/match-by-path (router-state app :reitit-router) (g/get js/location "pathname")))

(defn current-route-name
  "Returns the keyword name of the current route as determined by the URL path."
  [app]
  (some-> (current-route-from-url app) :data :name))

(defn route=url?*
  [app route-key params {{curr-name :name} :data curr-params :path-params}]
  (let [routes-by-name (router-state app :routes-by-name)]
    (boolean
      (when-let [{:keys [name]} (routes-by-name route-key)]
        (and
          (= name curr-name)
          (= params curr-params))))))

(defn route=url?
  "predicate does the :key like :goals {:date \"2020-05-20\"}
  match current reitit match of the url"
  [app route-key params]
  (route=url?* app route-key params (current-route-from-url app)))

(comment (route=url? :goals {:date "2020-05-12"}))

(>defn change-route!
  "Invokes reitit-fe-easy/push-state unless the current URL is the route-key already."
  ([app route-key]
   [app/fulcro-app? keyword? => any?]
   (let [router         (router-state app)
         routes-by-name (:routes-by-name router)
         {:keys [name] :as route} (get routes-by-name route-key)]
     (when-not (route=url? app route-key {})
       (log/info "Changing route to: " route)
       (rfe/push-state name))))

  ([app route-key params]
   [app/fulcro-app? keyword? map? => any?]
   (let [routes-by-name (router-state app :routes-by-name)
         {:keys [name] :as route} (get routes-by-name route-key)]
     (when-not (route=url? app route-key params)
       (log/info "Changing route to : " route)
       (log/info "push state : " name " params: " params)
       (rfe/push-state name params)))))

(>defn start-router!
  "Starts reitit router listening to URL changes."
  [app]
  [app/fulcro-app? => any?]
  (let [reitit-router (router-state app :reitit-router)]
    (log/info "Starting router: " reitit-router)
    (rfe/start! reitit-router (partial on-match app) {:use-fragment false})))

;; todo support merging in the new routes instead of overwriting.

(defn register-routes!
  "swap!s in state for the router into the fulcro runtime-atom."
  [app routes]
  (let [{::app/keys [runtime-atom]} app]
    (swap! runtime-atom
      (fn [s]
        (if (router-registered? app)
          (let [curr-route-state (router-state app)]
            (assoc-router-state s
              (fu/deep-merge (initial-router-state routes) curr-route-state)))
          (assoc-router-state s (initial-router-state routes)))))))

(>defn gather-routes
  "Constructs a data structure of routes as used by reitit from a Fulcro Router
  assembles reitit routes from dv.fulcro-reitit/route data specified on each route target."
  [fulcro-router]
  [dr/router? => vector?]
  (let [{:keys [router-targets]} (c/component-options fulcro-router)]
    (reduce
      (fn [acc t]
        (if-let [{::keys [route]} (c/component-options t)]
          (let [f (first route)]
            (log/info "route: " route)
            (cond
              (string? f) (conj acc route)
              (vector f) (into acc route)
              :else
              (throw (js/Error. (str (c/component-name t) " has " ::route "specified, but is in invalid format: " (pr-str route))))))
          (log/warn (str "Route target " (c/component-name t) " has no path info specified."))))
      []
      router-targets)))

;; it would be nice to not lose state in hot reloads
;; so instead of having register-routes! using init-router-state
(>defn register-fulcro-router!
  "Takes a fulcro app and a dr/defrouter component. Gathers reitit route data from the
  route-targets on the fulcro router and registers a reitit router within the fulcro application."
  [app fulcro-router]
  [app/fulcro-app? dr/router? => any?]
  (let [routes (gather-routes fulcro-router)]
    (log/info "Registering reitit routes: " routes)
    (register-routes! app routes)))

(defn register-and-start-router! [app routes]
  (register-routes! app routes)
  (start-router! app))
