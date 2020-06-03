(ns dv.fulcro-utils
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :refer [defmutation]]
    [com.fulcrologic.guardrails.core :refer [>defn =>]]))

(s/def ::props (s/coll-of simple-symbol? :count 1))

(comment (s/conform ::props '[params]))

(s/def ::arrow #(= '=> %))
(comment (s/conform ::arrow '=>))

(s/def ::thread-forms (s/* any?))
(comment
  (s/conform ::thread-forms '(update-math* (assoc-math* :math/game-state :not-running)))
  (s/conform ::thread-forms '((assoc-math* :math/game-state :not-running)))
  (s/explain ::thread-forms '((assoc-math* :math/game-state :not-running))))

(s/def ::body (s/cat :thread-forms ::thread-forms))
(s/def ::params (s/cat :name simple-symbol? :props ::props :body ::body))

(comment
  (def body '(=> [params] (assoc-math* :math/game-state :not-running)))
  (s/conform ::body body)
  (s/explain ::body body))

(defmacro defm
  "define a mutation for the simple case of threading state through forms.
  (defm pause-game [_] (assoc-math* :math/game-state :not-running))"
  [name props & body]
  (let [conformed (s/conform ::body body)
        {:keys [thread-forms]} conformed
        props     (s/assert ::props props)]
    `(com.fulcrologic.fulcro.mutations/defmutation ~name ~props
       (~'action ~'[{:keys [state]}]
         (cljs.core/swap! ~'state
           (cljs.core/fn [s#] (-> s# ~@thread-forms)))))))

(s/fdef defm :args ::params :ret any?)

(comment
  (macroexpand-1 '(defm pause-game [_] (assoc-math* :math/game-state :not-running)))
  (macroexpand-1 '(defm pause-game [_]
                    (assoc-math* :math/game-state :not-running)
                    (assoc-math* :math/game-state :not-running)))
  )

(s/def ::state simple-symbol?)

(defmacro ->s!
  ""
  [state & thread-forms]
  `(swap! ~state
     (fn [s#] (-> s# ~@thread-forms))))

(s/def ::->params (s/cat :state ::state :thread-forms ::thread-forms))
(s/fdef ->s! :args ::->params :ret any?)
(comment
  (macroexpand-1
    '(->s! state
       reset-cells*
       (assoc-math* :math/game-state :running)
       (assoc-math* :math/time-left countdown-seconds))))

(defmacro deflocalmutation
  "name and body, fn that takes state map and props + env in one map"
  [name f]
  `(let []
     (defmutation ~name
       [params#]
       (~'action [{:keys [~'state ~'ref] :as env#}]
         (do
           ;(log/info "IN MUTATION " ~name " params: " params#)
           (swap! ~'state
             (fn [s#]
               (~f s# (merge env# params#)))))))

     (defn ~(symbol (str name "!"))
       ([this#]
        (comp/transact!! this# [(~name)]))
       ([this# props#]
        (assert (map? props#))
        (comp/transact!! this# [(~name props#)])))))

;; todo the idea here is to generate a component for the usual form save interaction
;; components that ask for all the props and activate state machines that handle the logic.
;; needs more time in the oven
(defmacro def-return-entity
  [name ident fields]
  `(defsc ~name [_# _#]
     {:query (fn [_#]
               (conj ~fields :server/message :server/error?))
      :ident ~ident}))

(comment
  (macroexpand-1
    '(def-return-entity TaskItemReturn
       [:component/id :task-item-return]
       app/all-task-fields)))
