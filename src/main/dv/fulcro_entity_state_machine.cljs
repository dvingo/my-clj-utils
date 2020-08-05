(ns dv.fulcro-entity-state-machine
  (:refer-clojure :exclude [uuid])
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [dv.fulcro-util :as fu]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.components :as c]))

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

(defn handle-submit
  [{::sm/keys [event-data] :as env}]
  (log/info "in handle-submit")
  (let [{:keys [entity remote-mutation mutation target on-reset-mutation on-reset creating?]} event-data
        form-cls      (sm/actor-class env :actor/form)
        form-instance (actor->inst :actor/form env)
        item-cls      (sm/actor-class env :actor/new-item)]
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
             ::sm/ok-data     {:form-cls form-cls :entity entity :target target :creating? creating?}
             ::sm/error-event :event/failed}
            entity)))
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
                        (fn [{{:keys [form-cls entity target creating?]} ::sm/event-data :as env}]
                          (log/info "SUBMIT SUCCESS")
                          ;; todo scroll to top of window
                          (-> env
                            (sm/apply-action
                              #(apply merge/merge-component (into [% form-cls entity] (fu/map->vec target))))
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
  [this {:keys [machine remote-mutation mutation on-reset-mutation on-reset entity target creating?]}]
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
       :creating?         creating?}
      remote-mutation (assoc :remote-mutation remote-mutation)
      mutation (assoc :mutation mutation))))

(defn begin! [this id new-entity]
  (sm/begin! this form-machine id
    {:actor/form     this
     :actor/new-item new-entity}))


