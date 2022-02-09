(ns dv.fulcro-entity-state-machine
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as c]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [dv.fulcro-util :as fu]
    [taoensso.timbre :as log]))

(defn actor->inst [actor-kw env]
  (comp/ident->any (::sm/app env) (sm/actor->ident env actor-kw)))

(defn get-active-state [env]
  (get-in env [::sm/state-map ::sm/asm-id (sm/asm-id env) ::sm/active-state]))

(defn assoc-active-state
  [env]
  ;(log/info "assoc active state sf actor -> comp " (sm/asm-value env ::sm/actor->component-name))
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
  (let [{:keys [entity remote-mutation mutation target target-xform on-success on-reset-mutation on-reset creating?]} event-data
        form-cls      (sm/actor-class env :actor/form)
        form-instance (actor->inst :actor/form env)
        item-cls      (sm/actor-class env :actor/new-item)
        remote-entity (fu/elide-client-only-values entity)]
    ;(log/info "sf actor -> comp " (sm/asm-value env ::sm/actor->component-name))
    ;(log/info "ITEM CLAS: " item-cls)
    ;(log/info "form CLAS: " form-cls)
    (when mutation (c/transact! form-instance `[(~mutation)]))
    (-> env
      (cond->
        (some? on-reset-mutation) (sm/store :on-reset-mutation on-reset-mutation)
        (some? on-reset) (sm/store :on-reset on-reset)
        (some? on-success) (sm/store :on-success on-success))
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
                          (let [on-success (sm/retrieve env :on-success)]
                            (log/info "SUBMIT SUCCESS")
                            ;; todo scroll to top of window
                            (-> env
                              (sm/apply-action
                                #(apply merge/merge-component
                                   (into [% form-cls (cond-> entity (some? target-xform) target-xform)] (fu/map->vec target))))
                              (sm/apply-action
                                #((if creating? fs/pristine->entity* fs/entity->pristine*) % (sm/actor->ident env :actor/form)))
                              (sm/apply-action #(fs/clear-complete* % (sm/actor->ident env :actor/form)))
                              (cond-> on-success (sm/apply-action on-success))
                              (sm/assoc-aliased :server-msg "Success")
                              (sm/set-timeout :clear-msg-timer :event/reset {:entity entity} 2000)
                              (sm/activate :state/success)))))}
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
  on-success - optional state mutation helper from state-map -> state-map (called via uism/apply-action)
  on-reset - a callback that will be passed the :entity passed to this function after it has been successfully saved.
              This is invoked via js/setTimeout and it is expected that this function will side-effect.
  creating? - if true the form will be reset to the pristine state after successfully creating the entity
              if false the form pristine state will be updated to be the new entity state.
  "
  [this {:keys [machine remote-mutation mutation on-reset-mutation on-reset on-success entity target target-xform creating?]}]
  (assert machine) (assert entity)
  (when (and remote-mutation mutation)
    (throw (fu/error "You can only pass a mutation or a remote-mutation, but not both.")))
  ;(log/info "submit-entity! state:" (sm/get-active-state this machine))
  (let [machine-in-db (get-in (app/current-state this) [::sm/asm-id machine])]
    (when (nil? machine-in-db) (throw (js/Error. (str "No machine in fulcro db with ID: " machine "\nDid you forget to start it?"))))
    (sm/trigger! this machine :event/submit
      (cond->
        {:entity            entity
         :on-reset          on-reset
         :on-reset-mutation on-reset-mutation
         :on-success        on-success
         :target            target
         :target-xform      target-xform
         :creating?         creating?}
        remote-mutation (assoc :remote-mutation remote-mutation)
        mutation (assoc :mutation mutation)))))

(defn begin! [this id new-entity]
  (log/info "starting form machine with new entity: " new-entity)
  (when-not
    ((:ident (c/component-options new-entity)) {})
    (throw (js/Error. (str "The return component you passed must have a singleton ident or be a component instance: " (pr-str new-entity)))))
  (sm/begin! this form-machine id
    {:actor/form     this
     :actor/new-item new-entity}))

(def query
  [:server/message
   :ui/machine-state
   :ui/loading?])
