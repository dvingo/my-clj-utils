(ns dv.fulcro-util-common
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as c :refer [defsc]]
    [com.fulcrologic.fulcro.dom.events :as e]
    [com.fulcrologic.guardrails.core :refer [>defn >def => | ?]]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]))

(defn error [& msg]
  #?(:cljs (js/Error. (apply str msg))
     :clj (RuntimeException. (apply str msg))))

(defn conj-vec
  "Returns a map
   adds an element val to the given entity at the prop fkw"
  [entity fkw val]
  (update entity fkw #(conj (or % []) val)))

(defn conj-set [entity fkw val]
  (update entity fkw #(conj (or (set %) #{}) val)))

(defn map->vec [m]
  (vec (mapcat identity m)))

(defn map-vals [f m]
  (into {} (map (juxt key (comp f val))) m))

(defn id? [id]
  (or (keyword? id) (uuid? id)))

(>def ::ident (s/tuple qualified-keyword? id?))
(>def ::coll-of-idents (s/coll-of ::ident :kind vector?))
(defn coll-of-idents? [v] (s/valid? ::coll-of-idents v))

(defn ref?
  "Return true if v is an ident
  if you pass kw-id the first element of v must by kw-id."
  ([v] (s/valid? ::ident v))
  ([kw-id v]
   (and (s/valid? ::ident v)
     (= (first v) kw-id))))

(>defn ->ident
  "Given a kw that is the id prop, and a map or id, return ident."
  ([kw]
   [keyword? => fn?]
   (fn
     ([m] (->ident kw m))
     ([id v] [kw id v])))
  ([kw v]
   [keyword? (s/or :id id? :m map? :ident ::ident) => ::ident]
   (if (ref? kw v)
     v
     [kw (if (map? v) (kw v) v)])))

(>defn ->idents
  "id-kw is id prop to use in nested collection found at property kw of map m."
  [id-kw m kw]
  [qualified-keyword? map? qualified-keyword? => map?]
  (cond-> m
    (some? (get m kw))
    (update kw
      (fn [vs]
        (when (not (coll? vs))
          (throw (error (str "Property is not a collection: " (pr-str kw) " val: " (pr-str vs)))))
        (mapv #(->ident id-kw %) vs)))))

(defn ref->id
  "ident [:prop id] => id"
  [v] (cond-> v (s/valid? ::ident v) second))

(defn ident->id
  "ident [:prop id] => id"
  [v] (cond-> v (s/valid? ::ident v) second))


(defn deep-merge [x y]
  (cond
    (and (map? x) (map? y)) (merge-with deep-merge x y)
    (and (map? x) (nil? y)) x
    (and (map? y) (nil? x)) y
    :else y))

(defn server-error [msg]
  {:server/message msg
   :server/error?  true})

(defn remove-missing
  "Remove properties from map which have value ::merge/not-found"
  [m]
  (reduce
    (fn [acc [k v]]
      (cond
        (= ::merge/not-found v) acc

        (and (vector? v) (every? map? v))
        (assoc acc k (mapv remove-missing v))

        (map? v)
        (assoc acc k (remove-missing v))

        :else (assoc acc k v)))
    {}
    m))

(comment
  (remove-missing {:field-1 ::merge/not-found})
  (remove-missing nil)
  (remove-missing
    {:a ::merge/not-found
     :c :other
     :d [{:abc ::merge/not-found :xyz :lsasdf}]
     :b {:x ::merge/not-found :z "hi"}
     }))

