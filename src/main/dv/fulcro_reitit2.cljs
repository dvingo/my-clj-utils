(ns dv.fulcro-reitit2
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
    [dv.fulcro-util :as fu]
    [reitit.frontend.easy :as rfe]
    [taoensso.timbre :as log]))

;;;; why not just put the state in the fulcro state atom?
;; it's a valid question - one idea is to swap directly against the
;; state atom without triggering UI updates.

;    https://book.fulcrologic.com/#_swapping_on_the_state_atom
;;;;

(defn reitit-router? [x] (satisfies? r/Router x))

(>def ::name keyword?)
(>def ::segment (s/or :vec (s/coll-of (s/or :s string? :k keyword? :fn fn?)) :fn fn?))
(>def ::route-map (s/keys :req-un [::name ::segment] :opt-un [::redirect-to]))
(>def ::route (s/cat :s string? :r ::route-map))

(comment
  (let [routes
        [["/" {:name :root :segment ["tasks"]}]
         ["/tasks" {:name :tasks :segment ["tasks"]}]
         ["/calendar" {:segment ["calendar"]}
          ["" {:name :calendar :segment [(fn [_] "202020")]}]
          ["/:date" {:name :calendar-date :segment [:date]}]]
         ["/signup" {:name :signup :segment ["signup"]}]]
        ]
    (init-router-state {} routes)))

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

;::fr/router :router :reitit-router reitit-router
;[::fr/router :router :routes-by-name] routes-by-name
;[::fr/router :router :current-fulcro-route] []
;[::fr/router :router :redirect-loop-count] 0
;[::fr/router :router :max-redirect-loop-count] max-redirect-loop-count

(defn get-state [s] (if (satisfies? IDeref s) @s s))

(def router-state-keys
  [:reitit-router
   :routes-by-name
   :current-fulcro-route
   :current-fulcro-route
   :redirect-loop-count
   :max-redirect-loop-count])

(>def ::deref-or-map (s/or :m map? :d #(satisfies? IDeref %)))
(>def ::state-arg (s/or :d #(satisfies? IDeref %) :app app/fulcro-app? :m map?))

(>defn ->map
  "Get fulcro-state atom's underlying map.
  can pass:
  a map - returns the map
  an atom - derefs
  fulcro app - get state-atom and deref"
  [v]
  [::state-arg => any?]
  (when (s/valid? ::state-arg v)
    (let [[k v] (s/conform ::state-arg v)]
      (log/info "CONFORMED key: " k)
      (log/info "CONFORMED: " v)
      (case k
        :m v
        :d @v
        :app @(::app/state-atom v)))))

(defn router-path
  ([]
   [::router :router])
  ([prop]
   ;[keyword? => (s/tuple qualified-keyword? keyword? keyword?)]
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

(defn init-router-state
  "Takes a map - your app state atom's value - assoc-in router state needed to work."
  [s routes]
  (let [reitit-router  (rf/router routes)
        routes-by-name (make-routes-by-name reitit-router)]
    (-> s
      (assoc-in [::router :router :reitit-router] reitit-router)
      (assoc-in [::router :router :routes-by-name] routes-by-name)
      (assoc-in [::router :router :current-fulcro-route] [])
      (assoc-in [::router :router :redirect-loop-count] 0)
      (assoc-in [::router :router :max-redirect-loop-count] *max-redirect-loop-count*))))

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


(defn set-route-state! [app prop v]
  (swap! (::app/runtime-atom app)
    (fn [s] (assoc-in s (router-path prop) v))))

(defn update-route-state!
  [app prop f]
  (swap! (::app/runtime-atom app)
    (fn [s] (update-in s (router-path prop) f))))

(defn handle-redirect [app route params]
  (let [params (params)
        {:keys [max-redirect-loop-count redirect-loop-count]} (router-state app)]
    (do (log/info "redirecting to: " route " with params " params)
        (if (> redirect-loop-count max-redirect-loop-count)
          (do
            (log/error (str "The route " route " hit the max redirects limit: " max-redirect-loop-count))
            (set-route-state! app :redirect-loop-count 0))
          (do
            (update-route-state! app :redirect-loop-count inc)
            #_(vswap! (router-state app :redirect-loop-count) inc)
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
        (handle-redirect app route params)
        (let [fulcro-segment       (fulcro-segment-from-match m)
              current-fulcro-route (router-state app :current-fulcro-route)]
          (log/info "Invoking Fulcro change route with " fulcro-segment)
          (set-route-state! app :current-fulcro-route fulcro-segment)
          (dr/change-route! app fulcro-segment))))))

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
   (let [routes-by-name (router-state app :routes-by-name )
           {:keys [name] :as route} (get routes-by-name route-key)]
       (when-not (route=url? app route-key params)
         (log/info "Changing route to : " route)
         (log/info "push state : " name " params: " params)
         (rfe/push-state name params)))))
;;;;;;;;;;; trying storing state in t

(>defn start-router!
  "Starts reitit router listening to URL changes."
  [app]
  [app/fulcro-app? => any?]
  (let [reitit-router (router-state app :reitit-router)]
    (log/info "Starting router: " reitit-router)
    (rfe/start! reitit-router (partial on-match app) {:use-fragment false})))

(defn register-router!
  "swap!s initial state for the router into the fulcro runtime-atom."
  [app routes]
  (let [{::app/keys [runtime-atom]} app]
    (swap! runtime-atom
      (fn [s] (init-router-state s routes)))))

(defn register-and-start-router! [app routes]
  (register-router! app routes)
  (start-router! app))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
" I still think this setup would work if I store the router state on the application
and not in the fulcro state-atom. - The fulcro app needs to be created at load time because

it needs to be used in will-enter (r/fulcro-segment SPA :my-route-name)
"
;;
