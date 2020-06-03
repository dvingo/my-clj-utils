(ns dv.fulcro-entity-state-machine
  (:refer-clojure :exclude [uuid])
  (:require
    ["react" :as react]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [dv.fulcro-util :as cu]
    [taoensso.timbre :as log]
    [goog.object :as gobj]
    [tick.alpha.api :as t])
  (:require-macros [space.matterandvoid.client.util]))

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
  (let [{:keys [entity remote-mutation append on-reset-mutation on-reset]} event-data
        form-cls (sm/actor-class env :actor/form)
        item-cls (sm/actor-class env :actor/new-item)]
    ;(log/info "Event data is: " event-data)
    ;(log/info "ENV " env)

    (-> env
      (cond->
        (some? on-reset-mutation) (sm/store :on-reset-mutation on-reset-mutation)
        (some? on-reset) (sm/store :on-reset on-reset))
      (sm/assoc-aliased :server-msg "")
      (sm/trigger-remote-mutation
        :actor/form
        remote-mutation
        (merge
          {::m/returning    item-cls
           ::sm/ok-event    :event/success
           ::sm/ok-data {:form-cls form-cls :entity entity :append append}
           ::sm/error-event :event/failed}
          entity))
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
                        (fn [{{:keys [form-cls entity append]} ::sm/event-data :as env}]
                          (log/info "SUBMIT SUCCESS")
                          ;; todo scroll to top of window
                          (-> env
                            (sm/apply-action #(merge/merge-component % form-cls entity :append append))
                            (sm/apply-action #(cu/reset-form* % (sm/actor->ident env :actor/form)))
                            (sm/assoc-aliased :server-msg "Success")
                            (sm/set-timeout :clear-msg-timer :event/reset {} 2000)
                            (sm/activate :state/success))))}
      :event/failed  {::sm/handler
                      (global-handler
                        (fn [env]
                          (log/info "SUBMIT FAILED: " env)
                          (let [msg (cu/get-server-mutation-err env)]
                            (-> env
                              (sm/assoc-aliased :server-msg msg)
                              (sm/activate :state/failed)))))}}}

    :state/success
    {::sm/events
     {:event/reset
      {::sm/handler
       (global-handler
         (fn [env]
           (log/info "HANDLING RESET")
           (let [instance (actor->inst :actor/form env)]
             (if (comp/mounted? instance)
               (do (log/info "Is mounted")
                   (when-let [reset-mutation (sm/retrieve env :on-reset-mutation)]
                     (log/info " RESET MUTATION: " reset-mutation)
                     (comp/transact! instance `[(~reset-mutation)]))
                   (when-let [on-reset (sm/retrieve env :on-reset)]
                     (log/info "Calling form reset fn")
                     (js/setTimeout on-reset))
                   (sm/activate env :state/not-submitting))
               (do (println "NOT mounted")
                   env)))))}}}

    :state/failed
    {::sm/events {:event/reset  (target :state/not-submitting)
                  :event/submit {::sm/handler handle-submit}}}}})

;; todo >defn
(defn submit-entity!
  [this {:keys [machine remote-mutation on-reset-mutation on-reset entity target]}]
  (assert machine) (assert entity) (assert target)
  (sm/trigger! this machine :event/submit
    (merge {:entity            entity
            :remote-mutation   remote-mutation
            :on-reset          on-reset
            :on-reset-mutation on-reset-mutation}
      target)))

;; TODO generate the machine id so the component code doesn't
;; need to know about it.
(defn begin! [this id new-entity]
  (sm/begin! this form-machine id
    {:actor/form     this
     :actor/new-item new-entity}))


