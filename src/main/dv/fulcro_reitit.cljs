(ns dv.fulcro-reitit
  (:require
    [cljs.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as c :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
    [com.fulcrologic.guardrails.core :refer [>defn => | ? >def]]
    [dv.fulcro-util-common :as fu]
    [edn-query-language.core :as e]
    [goog.object :as g]
    [reitit.coercion :as coercion]
    [reitit.core :as r]
    [reitit.frontend :as rf]
    [reitit.frontend.easy :as rfe]
    [taoensso.timbre :as log]))

(comment
  (r/router
    [["/" {:name :root, :segment ["tasks"]}]
     ["/tasks" {:name :tasks, :segment ["tasks"]}
      ["/:task-id/edit" {:segment ["edit" :task-id]}]
      ["/:task-id" {:segment ["view" :task-id]}]]
     #_["/signup" {:name :signup, :segment ["signup"]}]
     #_["/calendar"
        {:segment ["calendar"]}
        ["" {:name :calendar, :segment ["cal"]}]
        ["/:date" {:name :calendar-date, :segment [:date]}]]])
  )

(defn reitit-router? [x] (satisfies? r/Router x))

(>def ::name keyword?)
(>def ::segment (s/coll-of (s/or :s string? :k keyword? :fn fn?) :type vector?))
(>def ::route-map (s/keys :req-un [::segment] :opt-un [::name ::redirect-to]))
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

(>def ::comp-or-app (s/or :c c/component? :a app/fulcro-app?))

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

(defn init-router-and-state
  "Returns a map representing the state of the router.
   Constructs a new reitit router from the supplied routes and initializing state management values."
  ([routes] (init-router-and-state routes {}))
  ([routes opts]
   (let [reitit-router (rf/router routes opts)]
     {:reitit-router           reitit-router
      :routes-by-name          (make-routes-by-name reitit-router)
      :current-fulcro-route    []
      :redirect-loop-count     0
      :max-redirect-loop-count *max-redirect-loop-count*})))

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
   [::comp-or-app => ::router-state]
   (let [app (c/any->app app)]
     (if (router-registered? app)
       (-> app
         router-state*
         (select-keys router-state-keys))
       (throw (js/Error. "No router registered on fulcro app.")))))
  ([app p]
   [::comp-or-app keyword? => any?]
   (let [app (c/any->app app)]
     (if (router-registered? app)
       (get (router-state* app) p)
       (throw (js/Error. "No router registered on fulcro app."))))))

(defn reitit-routes
  "Returns the reitit routes currently registered as data via reitit.core/routes."
  [app]
  (-> app (router-state :reitit-router) (r/routes)))

(defn route-segment
  [app name]
  (if-let [segment (some-> app (router-state :routes-by-name) name :segment)]
    segment
    (throw (js/Error. (str "No matching fulcro segment for route: " (pr-str name))))))

(>defn route-href
  ([id]
   [keyword? => string?]
   (rfe/href id))
  ([id props]
   [keyword? any? => string?]
   (rfe/href id props))
  ([id props query]
   [keyword? map? map? => string?]
   (rfe/href id props query)))

(defn construct-fulcro-segments [match-data]
  (let [segment (-> match-data :data :segment)]
    (mapv
      (fn [s] (cond-> s (fn? s) (apply [match-data])))
      segment)))

(>defn current-fulcro-route
  [app]
  [::comp-or-app => vector?]
  (-> (c/any->app app) router-state :current-fulcro-route))

;; todo support query-params also, by merging

(>def ::fulcro-route-segment (s/or :string string? :kw simple-keyword?))

(defn fulcro-segment-from-match
  [{:keys [path-params query-params] :as m}]
  (let [fulcro-segments (construct-fulcro-segments m)
        ;; fill in any dynamic path segments with their values from the reitit match
        target-segment  (->> fulcro-segments
                          (mapv (fn [part]
                                  (when-not (s/valid? ::fulcro-route-segment part)
                                    (throw (js/Error. (str "\nRoute returned an invalid fulcro route segment: " (pr-str part)
                                                        "\n\nRoute is: " (:data m)
                                                        ".\n\nIt should be a keyword or string (or a function that returns one of these).\n"))))
                                  (if (keyword? part)
                                    (or (part path-params) (part query-params))
                                    part))))]
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
        (let [fulcro-segment (fulcro-segment-from-match m)
              {:keys [parameters]} m
              parameters     (merge (:path parameters) (:query parameters))]
          (log/info "Invoking Fulcro change route with " fulcro-segment)
          (set-router-state! app :current-fulcro-route fulcro-segment)
          (dr/change-route! app fulcro-segment parameters)))
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
   [::comp-or-app keyword? => any?]
   (change-route! app route-key {}))

  ([app route-key params]
   [::comp-or-app keyword? map? => any?]
   (let [app            (c/any->app app)
         routes-by-name (router-state app :routes-by-name)
         {:keys [name] :as route} (get routes-by-name route-key)]
     (when-not (route=url? app route-key params)
       (when-not route (throw (js/Error. (str "Unknown reitit route for key: " route-key "\n"
                                           "Your routes are:\n" (reduce (fn [a r] (str a (pr-str r) "\n")) "" (reitit-routes app))))))
       (log/info "Changing route to : " route)
       (log/info "push state : " name " params: " params)
       (rfe/push-state name params)))))

(defn start-router!* [app]
  (let [reitit-router (router-state app :reitit-router)]
    (log/info "Starting router: " reitit-router)
    (rfe/start! reitit-router (partial on-match app) {:use-fragment false})))

(>defn start-router!
  "Starts reitit router listening to URL changes."
  [app]
  [(s/or :app app/fulcro-app? :var var?) => any?]
  (log/info "var? " (pr-str (var? app)))
  (if (var? app) (js/setTimeout #(start-router!* @app))
                 (start-router!* app)))

;; todo support merging in the new routes instead of overwriting.

(defn save-router-state*! [app routes opts]
  (let [{::app/keys [runtime-atom]} app]
    (swap! runtime-atom
      (fn [s]
        (if (router-registered? app)
          (let [curr-route-state (router-state app)]
            (assoc-router-state s
              (fu/deep-merge (init-router-and-state routes opts) curr-route-state)))
          (assoc-router-state s (init-router-and-state routes opts)))))))

(defn register-routes!
  "swap!s in state for the router into the fulcro runtime-atom."
  ([app routes]
   (register-routes! app routes {:compile coercion/compile-request-coercers}))
  ([app routes opts]
   (if (var? app)
     (js/setTimeout #(save-router-state*! @app routes opts))
     (save-router-state*! app routes opts))))

(>defn route-target?
  [c]
  [fn? => boolean?]
  (boolean (c/component-options c ::route)))

(defn comp->ast [c]
  (-> c c/get-query e/query->ast))

(defn not-empty? [c] (boolean (seq c)))

(defn get-routers-from-query
  "Takes a component returns fulcro routers (defrouter) that are joined in the components query or nil if none."
  [c]
  (->> (comp->ast c) :children
    (filter
      (fn [c]
        ;; when there is a component and that component is a router
        (when-let [c2 (:component c)]
          (and (= (:type c) :join) (dr/router? c2)))))
    (mapv :component)
    not-empty))

(defn concat-sub-routes
  "
  want to go from:
  ['/tasks' {:name :tasks :segment ['tasks']}]

  to this:
  ['/tasks' {:segment ['tasks']} ['' {:name :tasks}]]
  in order to get segment and names to work when expanded by reitit.
  "
  [parent children]
  (log/info "concat sub routes parent: " (pr-str parent))
  (log/info "concat sub routes children: " (pr-str children))
  (let [[route-path {:keys [name] :as m}] parent
        ;; if there is name and we are nesting, then need to reconfigure
        out (if name
              (let [parent [route-path (dissoc m :name)]]
                (println "have name")
                (vec (concat parent (reduce into [["" {:name name}]] children))))
              (do
                (println "no name")
                (reduce into parent children)))]
    (println "concat sub ret: ")
    out))

(comment

  (concat-sub-routes
    ["/tasks" {:segment ["tasks"]} ["" {:name :tasks}]]
    []
    )

  (gather-recursive TopRouter)
  (reduce into
    ["/:task-id/edit" {:name :edit-task :segment ["hi"]}]
    [[["/nested/another" {:name :nested :segment ["nested"]}]]]))

(defn gather-recursive
  [fulcro-router]
  ;; for each target, if it contains a router in its query
  ;; then you call gather-routes on that nested router
  ;; and insert them in nested position in the resulting reitit-routes data structure
  (let [router-targets (dr/get-targets fulcro-router)]
    (reduce
      (fn [acc t]
        (log/info "target: " (c/component-name t))
        (if-let [{::keys [route] :keys [route-segment]} (c/component-options t)]
          (let [[f route-params] route]
            (when-not route-segment
              (throw (js/Error. (str "Missing fulcro component option :route-segment attribute on target " (c/component-name t)))))
            (log/info "route: " route)
            ;; we don't want to throw but we should do something sensible if your fulcro
            ;; router has no reitit routes specified.
            ;; it makes sense to not need a reitit route on all routes in fulcro router.
            ;; but if there are none this should be an error - you shouldn't be initializing
            ;; reitit if there are no routes
            (when-not route (throw (js/Error. (str "No reitit route on target " (c/component-name t) "\n\nAdd the attribute " (pr-str ::route) " to your component.\n"
                                                "Do not forget the fulcro :route-segment as well. :)\n"))))
            (cond
              ;; todo assert that the route portion at each level is "flat" - the nesting is now from the components
              ;; rendering each other. If there is nesting in the data, that doesn't make sense so throw an error.

              ;; only one route
              (string? f)
              (do
                (when-not (contains? route-params :segment) (throw (js/Error. (str "Missing :segment attribute in reitit data map for target " (c/component-name t)))))
                (when-not (contains? route-params :name) (throw (js/Error. (str "Missing :name attribute in reitit data map for target " (c/component-name t)))))
                (if-let [nested-routers (get-routers-from-query t)]
                  (let [sub-routes (map gather-recursive nested-routers)]
                    ;(log/info "one: route" route)
                    ;(log/info "one: sub-routes" sub-routes)
                    (into acc [(concat-sub-routes route sub-routes)]))
                  (conj acc route)))

              ;; many routes
              (vector? f)
              (if-let [nested-routers (get-routers-from-query t)]
                (let [
                      _                (log/info "has many")
                      sub-routes       (map gather-recursive nested-routers)
                      alias-routes     (filter #(:alias (meta %)) route)
                      non-alias-routes (remove #(:alias (meta %)) route)
                      true-route       (last non-alias-routes)
                      w-nested         [(concat-sub-routes true-route sub-routes)]
                      next-data        (vec (concat (into acc alias-routes) w-nested))]
                  (log/info "non-alias routes: " non-alias-routes)
                  (log/info "next-data : " next-data)
                  (when (> 1 (count non-alias-routes))
                    (log/error (str "Component: " (c/component-name t) " has more than one route specified.")))
                  ;(log/info "many: route" route)
                  ;(log/info "many true route: " true-route)
                  ;(log/info "many: sub-routes" sub-routes)
                  next-data)
                (into acc route))
              :else
              (throw (js/Error. (str (c/component-name t) " has " ::route "specified, but is in invalid format: " (pr-str route))))))
          (log/warn (str "Route target " (c/component-name t) " has no path info specified."))))
      []
      router-targets)
    ))


;; The next step is to
;; support this process at runtime to register new subtrees of UI that register
;; new nested routes, for example loading an edit subpage lazily
;; but only used by small number of users, so don't need the routes to exist at first.

(>defn register-fulcro-router!
  "Takes a fulcro app and a dr/defrouter component. Gathers reitit route data from the
  route-targets on the fulcro router and registers a reitit router within the fulcro application."
  [app fulcro-router]
  [(s/or :app app/fulcro-app? :var var?) dr/router? => any?]
  (let [routes (gather-recursive fulcro-router)]
    (log/info "Registering reitit routes: " routes)
    (register-routes! app routes)))

(defn register-and-start-router! [app routes]
  (register-routes! app routes)
  (start-router! app))

;; todo
;; there are some fulcro applications which will set the fulcro app asynchronously, which prevents registering the
;; routers on load of the script.One idea is to have a register-fulcro-router-delay
;; or similar name that will enqueue the router and then later you can have something like run-register-enqueued-routers!
;; or even just do it as part of start-router and then have a start-router! variant like start-router-async!


;; as a nice behavior to prevent forgetting one more thing to do when adding a route:
;; have this library set the :route-segment component option - or throw if it is not present on a route.
;; this is a source of bugs

;; support passing {:compile coercion/compile-request-coercers} when registering routes
