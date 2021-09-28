(ns dv.spec-util
  (:refer-clojure :exclude [keys])
  (:require
    [clojure.spec.alpha :as s]
    [dv.fulcro-util-common :as fu]
    [taoensso.timbre :as log]))

(defmacro to-many-ref-type
  [spec]
  `(s/coll-of
     (s/or
       :id fu/id?
       :ident ::fu/ident
       :pathom-join-map (s/map-of qualified-keyword? uuid?)
       ~(keyword (name spec)) ~spec)
     :type vector?))

(defmacro to-one-ref-type
  [spec]
  (let [nm   (name spec)
        idkw (keyword nm "id")
        nmkw (keyword nm)]
    `(s/or
       :id uuid?
       :ident ::fu/ident
       :map map?
       ~(keyword (str nm "-ref")) (s/map-of ~idkw fu/id?)
       :pathom-join-map (s/map-of qualified-keyword? uuid?)
       ~nmkw ~spec)))

(comment
  (macroexpand-1 '(to-one-ref-type ::habit)))

(def global-keys [:db/created-at :db/updated-at :crux.db/id])

(defmacro get-global-keys [] global-keys)

(defmacro def-domain-map
  "Return s/keys :req for fields supports passing in symbols for keys"
  ([spec required]
   (let [req (eval required)]
     `(s/def ~spec (s/keys :req ~req))))
  ([spec required opt]
   (let [req        (eval required)
         opt        (eval opt)
         global-opt (eval global-keys)]
     `(s/def ~spec (s/keys :req ~req :opt ~(into opt global-opt))))))

(defmacro def-domain-map2
  "Return s/keys :req for fields key vectors must be literals"
  ([spec required]
   (let [req required]
     `(s/def ~spec (s/keys :req ~req))))
  ([spec required opt]
   (let [req        required
         opt        opt
         global-opt global-keys]
     `(s/def ~spec (s/keys :req ~req :opt ~(into opt global-opt))))))

(comment
  (def fields [:one :two])
  (macroexpand-1 '(def-domain-map ::task fields))
  (let [task-fields [:ab :cd :ef]]
    (macroexpand-1 `(def-domain-map ::task ~task-fields)
      )))

(defmacro keys [ks]
  `(s/keys :req ~(eval ks) :opt ~(eval global-keys)))

;https://corfield.org/blog/2019/09/13/using-spec/
(comment

  ;; I'm thinking of using macros to generate pathom queries
  ;; crux crud operations
  ;; things like the find-by-description helper function for a user etc.
  (s/describe ::habit-record)

  (s/form ::habit-record)
  (s/get-spec ::habit-record)

  ;; instead of def'ing the list of fields
  ;; i think the way to go is to write
  ;; the spec literals and then
  ;; create the defs for the fields
  ;(second (s/describe ::habit))

  ;; can use spec to parse the output of spec/form s/cat + friends
  )


(comment

  (str (symbol (name :be.produktiv.ad-hoc-tasks.ad-hoc-tasks-data-model/ad-hoc-task)))
  )

(defmacro defentity
  "kw "
  [spec-kw kw-required required-keys kw-optional optional-keys]
  (let [all-keys (into required-keys optional-keys)
        sym-str  (name spec-kw)
        sym      (symbol sym-str)
        id-kw    (keyword sym-str "id")]
    (log/info "here 1")
    `(let []
       (def ~(symbol (str "all-" sym "-keys")) ~(into required-keys optional-keys))
       (def ~(symbol (str "db-" sym "-keys")) ~(into all-keys global-keys))

       ~(log/info "here 2")
       ;~(def-domain-map2 spec-sym required-keys optional-keys)

       ;; todo arglist
       (def ~(symbol (str sym "-ident"))
         ~(str "Returns [" sym "/id] for the passed in map or id")
         (fu/->ident ~id-kw))

       ;`(>defn make-task
       ;  [{~(keyword sym-str "keys") ~all-keys
       ;    :or        {id (fu/uuid)}}]
       ;  [map? => ~spec-kw]
       ;  (cond->
       ;    {:task/id          id
       ;     :task/description description}
       ;    ~(interleave...)
       ;    (some? global?) (assoc :task/global? global?)
       ;    (some? duration) (assoc :task/duration duration)
       ;    (some? scheduled-at) (assoc :task/scheduled-at scheduled-at)))
       ;
       ;(defn fresh-task [props]
       ;  (make-task (merge props {:task/id (fu/uuid)})))

       ;#?(:clj (cu/gen-make-db-entity make-db-task ::task))

       )))



(comment
  (macroexpand-1
    '(defentity :be.produktiv.ad-hoc-tasks.ad-hoc-tasks-data-model/ad-hoc-task
       :required [:task/id :task/description]
       :optional [:task/duration :task/scheduled-at]

       )))
