(ns dv.fulcro-entity-state-machine
  (:require
    [clojure.walk :as walk]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as c]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [dv.fulcro-util :as fu]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]))

(defn actor->inst [actor-kw env]
  (comp/ident->any (::sm/app env) (sm/actor->ident env actor-kw)))

(defn get-active-state [env]
  (get-in env [::sm/state-map ::sm/asm-id (sm/asm-id env) ::sm/active-state]))

(defn assoc-active-state [env]
  (let [cur-state (get-active-state env)]
    (-> env
      (sm/assoc-aliased :machine-state cur-state)
      (sm/assoc-aliased :loading? (= :state/submitting cur-state)))))

(defn global-handler
  "Sets alias machine-state to new state at end of current handler run."
  [f] #(assoc-active-state (f %)))

(defn activate [state]
  (global-handler #(sm/activate % state)))

(defn target [state]
  {::sm/handler (activate state)})

(defn kw-namespace [k] (and (keyword? k) (namespace k)))

;; just so you don't consume the stack

;; taken from fulcro.application ns

(defn client-only-value? [k]
  ;(println "checking key: " k)
  (let [ns       (some-> k kw-namespace)
        ident-ns (when (eql/ident? k) (some-> (first k) kw-namespace))
        v        (or
                   (and
                     (string? ns)
                     (or
                       (= "ui" ns)
                       (str/starts-with? ns "com.fulcrologic.fulcro.")))
                   (and
                     (string? ident-ns)
                     (or
                       (= "ui" ident-ns)
                       (str/starts-with? ident-ns "com.fulcrologic.fulcro."))))
        ]
    ;(println "ret: " v)
    v))

;(defn elide-client-only-values
;  "Takes a map and recursively removes and keys that should not be sent over the network."
;  [m]
;  (reduce
;    (fn [acc [k v]]
;      (if-not (client-only-value? k)
;        (cond
;          (map? v) (assoc acc k (elide-client-only-values v))
;          (vector? v) (assoc acc k (mapv elide-client-only-values v))
;          :else (assoc acc k v))
;        acc))
;    {} m))


(defn elide-client-only-values*
  "Takes a map, removes keys that should not be sent over the network."
  [m]
  (reduce
    (fn [acc [k v]]
      (if (client-only-value? k) acc (assoc acc k v)))
    {} m))

(defn elide-client-only-values
  [m]
  (walk/postwalk
    (fn [x] (if (map? x) (elide-client-only-values* x) x))
    m))

(defn handle-submit
  [{::sm/keys [event-data] :as env}]
  (log/info "in handle-submit")
  (let [{:keys [entity remote-mutation mutation target target-xform on-reset-mutation on-reset creating?]} event-data
        form-cls      (sm/actor-class env :actor/form)
        form-instance (actor->inst :actor/form env)
        item-cls      (sm/actor-class env :actor/new-item)
        ;; for some reason the default global-eql-transform is not eliding nested keys
        remote-entity (elide-client-only-values entity)]
    (when mutation (c/transact! form-instance `[(~mutation)]))
    (-> env
      (cond->
        (some? on-reset-mutation) (sm/store :on-reset-mutation on-reset-mutation)
        (some? on-reset) (sm/store :on-reset on-reset))
      (sm/assoc-aliased :server-msg "")
      (cond->
        (some? remote-mutation)
        (sm/trigger-remote-mutation
          :actor/form
          remote-mutation
          (merge
            {::m/returning    item-cls
             ::sm/ok-event    :event/success
             ::sm/ok-data     {:form-cls form-cls :entity entity :target target :target-xform target-xform :creating? creating?}
             ::sm/error-event :event/failed}
            remote-entity)))
      (sm/activate :state/submitting)
      (assoc-active-state))))

(defstatemachine form-machine
  {::sm/actor-names
   #{:actor/form
     :actor/new-item}

   ::sm/aliases
   {:server-msg    [:actor/form :server/message]
    :machine-state [:actor/form :ui/machine-state]
    :loading?      [:actor/form :ui/loading?]}

   ::sm/states
   {:initial
    {::sm/target-states #{:state/not-submitting}
     ::sm/events        {::sm/started (target :state/not-submitting)}}

    :state/not-submitting
    {::sm/events {:event/submit
                  {::sm/target-states #{:state/submitting}
                   ::sm/handler       handle-submit}}}

    :state/submitting
    {::sm/events
     {:event/success {::sm/handler
                      (global-handler
                        (fn [{{:keys [form-cls entity target target-xform creating?]} ::sm/event-data :as env}]
                          (log/info "SUBMIT SUCCESS")
                          ;; todo scroll to top of window
                          (-> env
                            (sm/apply-action
                              #(apply merge/merge-component
                                 (into [% form-cls (cond-> entity (some? target-xform) target-xform)] (fu/map->vec target))))
                            (sm/apply-action
                              #((if creating? fs/pristine->entity* fs/entity->pristine*) % (sm/actor->ident env :actor/form)))
                            (sm/apply-action #(fs/clear-complete* % (sm/actor->ident env :actor/form)))
                            (sm/assoc-aliased :server-msg "Success")
                            (sm/set-timeout :clear-msg-timer :event/reset {:entity entity} 2000)
                            (sm/activate :state/success))))}
      :event/failed  {::sm/handler
                      (global-handler
                        (fn [env]
                          (log/info "SUBMIT FAILED, env: " env)
                          (let [msg (fu/get-server-mutation-err env)]
                            (-> env
                              (sm/assoc-aliased :server-msg msg)
                              (sm/activate :state/failed)))))}}}

    :state/success
    {::sm/events
     {:event/reset
      {::sm/handler
       (global-handler
         (fn [{{:keys [entity]} ::sm/event-data :as env}]
           (let [instance (actor->inst :actor/form env)]
             (if (comp/mounted? instance)
               (do (log/info "Is mounted")
                   (when-let [reset-mutation (sm/retrieve env :on-reset-mutation)]
                     (log/info " RESET MUTATION: " reset-mutation)
                     (comp/transact! instance `[(~reset-mutation)]))
                   (when-let [on-reset (sm/retrieve env :on-reset)]
                     (log/info "Calling form reset fn")
                     (js/setTimeout #(on-reset entity))))
               (do (log/info "NOT mounted")))
             (sm/activate env :state/not-submitting))))}}}

    :state/failed
    {::sm/events {:event/reset  (target :state/not-submitting)
                  :event/submit {::sm/handler handle-submit}}}}})

;; todo >defn
(defn submit-entity!
  "target - fulcro target path
  target-xform - a pure function that takes the entity and transforms it in some way before it is passed to target.
  creating? - if true the form will be reset to the pristine state after successfully creating the entity
              if false the form pristine state will be updated to be the new entity state.
  "

  [this {:keys [machine remote-mutation mutation on-reset-mutation on-reset entity target target-xform creating?]}]
  (assert machine) (assert entity)
  (when (and remote-mutation mutation)
    (throw (fu/error "You can only pass a mutation or a remote-mutation, but not both.")))

  ;(log/info "submit-entity! state:" (sm/get-active-state this machine))
  (sm/trigger! this machine :event/submit
    (cond->
      {:entity            entity
       :on-reset          on-reset
       :on-reset-mutation on-reset-mutation
       :target            target
       :target-xform      target-xform
       :creating?         creating?}
      remote-mutation (assoc :remote-mutation remote-mutation)
      mutation (assoc :mutation mutation))))

(defn begin! [this id new-entity]
  (sm/begin! this form-machine id
    {:actor/form     this
     :actor/new-item new-entity}))


;; testing elide-values
(comment
  (elide-client-only-values
    {:habit/id            #uuid "e777425f-e28e-45b0-9ccf-f007b603d02f",
     :habit/duration      nil,
     :habit/starts-on     "UnknownTransitType: 2020-08-06",
     :habit/criteria-num  2,
     :habit/description   "Right then",
     :habit/active?       true,
     :habit/repeats-every "UnknownTransitType: P1D",
     :habit/tasks
                          [{:task/subtasks                [],
                            :ui/has-duration?             false,
                            :ui/has-scheduled-at?         false,
                            :ui/show-delete-confirmation? false,
                            :task/duration                "UnknownTransitType: PT1M",
                            :com.fulcrologic.fulcro.algorithms.form-state/config
                                                          {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                  [:task/id #uuid "1f2400de-af54-42e1-852d-48287d69f0d4"],
                                                           :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                  #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                    :task/description
                                                                                                                    :task/scheduled-at},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/complete?
                                                                                                                  #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                    :task/description
                                                                                                                    :task/scheduled-at},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                  {:ui/has-duration?     false,
                                                                                                                   :ui/has-scheduled-at? false,
                                                                                                                   :task/duration        "UnknownTransitType: PT1M",
                                                                                                                   :task/description     "chagnge",
                                                                                                                   :task/scheduled-at    "UnknownTransitType: [object Object]"}},
                            :ui/machine-state             :state/not-submitting,
                            :task/id                      #uuid "1f2400de-af54-42e1-852d-48287d69f0d4",
                            :ui/subtask-form
                                                          {:task/id              #uuid "6bde87a7-f6ef-4920-8828-404345282259",
                                                           :task/description     "",
                                                           :task/duration        "UnknownTransitType: PT1M",
                                                           :task/scheduled-at    "UnknownTransitType: [object Object]",
                                                           :ui/show-form-debug?  false,
                                                           :ui/has-duration?     false,
                                                           :ui/has-scheduled-at? false,
                                                           :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                                 {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                         [:task/id #uuid "6bde87a7-f6ef-4920-8828-404345282259"],
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                         #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                           :task/description
                                                                                                                                           :task/scheduled-at},
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                         {:ui/has-duration?     false,
                                                                                                                                          :ui/has-scheduled-at? false,
                                                                                                                                          :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                          :task/description     "",
                                                                                                                                          :task/scheduled-at    "UnknownTransitType: [object Object]"}}},
                            :ui/show-form-debug?          false,
                            :task/description             "chagnge",
                            :task/scheduled-at            "UnknownTransitType: [object Object]",
                            :db/updated-at                #inst "2020-08-06T20:31:24.755-00:00",
                            :db/created-at                #inst "2020-08-06T19:47:07.686-00:00"}
                           {:task/subtasks                [],
                            :ui/has-duration?             false,
                            :ui/has-scheduled-at?         false,
                            :ui/show-delete-confirmation? false,
                            :task/duration                "UnknownTransitType: PT1M",
                            :com.fulcrologic.fulcro.algorithms.form-state/config
                                                          {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                  [:task/id #uuid "8cf7f2eb-b095-4e36-a9a4-5f424cccc48c"],
                                                           :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                  #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                    :task/description
                                                                                                                    :task/scheduled-at},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/complete?
                                                                                                                  #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                    :task/description
                                                                                                                    :task/scheduled-at},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                           :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                  {:ui/has-duration?     false,
                                                                                                                   :ui/has-scheduled-at? false,
                                                                                                                   :task/duration        "UnknownTransitType: PT1M",
                                                                                                                   :task/description     "Drink 1 liter of water",
                                                                                                                   :task/scheduled-at    "UnknownTransitType: [object Object]"}},
                            :ui/machine-state             :state/not-submitting,
                            :task/id                      #uuid "8cf7f2eb-b095-4e36-a9a4-5f424cccc48c",
                            :ui/subtask-form
                                                          {:task/id              #uuid "a8bdce00-bce1-44bf-b749-602db3b03a3a",
                                                           :task/description     "",
                                                           :task/duration        "UnknownTransitType: PT1M",
                                                           :task/scheduled-at    "UnknownTransitType: [object Object]",
                                                           :ui/show-form-debug?  false,
                                                           :ui/has-duration?     false,
                                                           :ui/has-scheduled-at? false,
                                                           :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                                 {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                         [:task/id #uuid "a8bdce00-bce1-44bf-b749-602db3b03a3a"],
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                         #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                           :task/description
                                                                                                                                           :task/scheduled-at},
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                                  :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                         {:ui/has-duration?     false,
                                                                                                                                          :ui/has-scheduled-at? false,
                                                                                                                                          :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                          :task/description     "",
                                                                                                                                          :task/scheduled-at    "UnknownTransitType: [object Object]"}}},
                            :ui/show-form-debug?          false,
                            :task/description             "Drink 1 liter of water",
                            :task/scheduled-at            "UnknownTransitType: [object Object]",
                            :db/updated-at                #inst "2020-08-06T19:47:07.696-00:00",
                            :db/created-at                #inst "2020-08-06T19:47:07.696-00:00"}],
     :habit/criteria      :min-of})
  )
