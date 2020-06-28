(ns dv.spec-util
  (:refer-clojure :exclude [keys])
  (:require
    [clojure.spec.alpha :as s]
    [dv.fulcro-util :as fu]))

(defmacro to-many-ref-type
  [spec]
  `(s/coll-of (s/or :id fu/id? :ident ::fu/ident ~(keyword (name spec)) ~spec) :type vector?))

(defmacro to-many-ident-ref-type
  [spec]
  `(s/coll-of ::fu/ident :type vector?))

(defmacro to-one-ref-type
  [spec]
  (let [nm   (name spec)
        idkw (keyword nm "id")
        nmkw (keyword nm)]
    `(s/or :id fu/id? ~(keyword (str nm "-ref")) (s/map-of ~idkw fu/id?) ~nmkw ~spec)))

(comment
  (macroexpand-1 '(to-one-ref-type ::habit)))

(def global-keys [:db/created-at :db/updated-at :crux.db/id])

(defmacro get-global-keys [] global-keys)

(defmacro def-domain-map
  "Return s/keys :req for fields"
  ([spec required]
   (let [req (eval required)]
     `(s/def ~spec (s/keys :req ~req))))
  ([spec required opt]
   (let [req        (eval required)
         opt        (eval opt)
         global-opt (eval global-keys)]
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

