(ns dv.fulcro-util-common
  (:refer-clojure :exclude [uuid ident?])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as c :refer [defsc]]
    [com.fulcrologic.fulcro.dom.events :as e]
    [com.fulcrologic.guardrails.core :refer [>defn >def => | ?]]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log])
  #?(:clj
     (:import [java.util UUID])))

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

(defn into-vec
  "Takes collections and primitives and filters out nil values non-recursively into one vector."
  [& args]
  (vec
    (mapcat
      #(filter some? (if (coll? %) % [%]))
      args)))

(comment
  (into-vec [1 2 nil 3] nil 5 [1 1]))

(defn id? [id] (or (keyword? id) (uuid? id)))


(defn server-error [msg]
  {:server/message msg
   :server/error?  true})
(>def ::ident (s/tuple qualified-keyword? id?))
(>def ::coll-of-idents (s/coll-of ::ident :kind vector?))
(defn coll-of-idents? [v] (s/valid? ::coll-of-idents v))

(defn ident? [v] (s/valid? ::ident v))

(s/def ::prop-path (s/tuple qualified-keyword? id? qualified-keyword?))

(defn prop-path? [v]
  (s/valid? ::prop-path v))

;; spec for: [prop val]

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

(s/def ::str-or-num (s/or :s string? :n number?))

(defn deep-merge [x y]
  (cond
    (and (map? x) (map? y)) (merge-with deep-merge x y)
    (and (map? x) (nil? y)) x
    (and (map? y) (nil? x)) y
    :else y))

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

(defn filter-by-ns
  "a-ns is either a set of strings or one string."
  [a-ns m]
  (let [ns-set (if (string? a-ns) #{a-ns} a-ns)]
    (->>
      (keys m)
      (filter #(contains? ns-set (namespace %)))
      (select-keys m))))

(comment
  (filter-by-ns
    #{"habit-record" "task-record"}
    {:habit-record/id   "id"
     :other-key/val     5
     :task-record/id    "task id"
     :habit-record/name "name"})
  )
(comment
  ;(filter-by-ns
  ;  #{"habit-record"
  ;    "task-record"}
  ;  {:habit/id
  ;                       {#uuid "6a35e95f-b926-4bdf-9c34-761d8b35c903"
  ;                        {:habit/id            #uuid "6a35e95f-b926-4bdf-9c34-761d8b35c903",
  ;                         :habit/starts-on     #time/date "2020-08-26",
  ;                         :habit/criteria-num  2,
  ;                         :habit/description   "Stretch twice",
  ;                         :habit/active?       true,
  ;                         :habit/repeats-every #time/period "P1D",
  ;                         :db/updated-at       #inst "2020-08-26T17:39:14.074-00:00",
  ;                         :habit/tasks
  ;                                              [[:task/id #uuid "5aade06c-f9f0-4054-8c70-a99f613816e5"]
  ;                                               [:task/id #uuid "3cf00493-0d05-4aa3-9164-c73fa5c72c86"]],
  ;                         :habit/criteria      :exactly,
  ;                         :db/created-at       #inst "2020-08-26T17:39:14.074-00:00"}},
  ;   :habit-record/date  #time/date "2020-08-27",
  ;   :habit-record/id    #uuid "182a27d6-3e0c-4d31-ac47-5df0f706d432",
  ;   :task/id
  ;                       {#uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059",
  ;                         :task/description  "Left pigeon",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.836-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.836-00:00"},
  ;                        #uuid "d6a2a972-2738-4c24-b7ad-821726aad2af"
  ;                        {:task/id           #uuid "d6a2a972-2738-4c24-b7ad-821726aad2af",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "395ccac3-c1db-4a38-9680-96dcd8a7f402"
  ;                        {:task/id           #uuid "395ccac3-c1db-4a38-9680-96dcd8a7f402",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35",
  ;                         :task/description  "Groin split",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.892-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.892-00:00"},
  ;                        #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5",
  ;                         :task/description  "Right hamstring",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.883-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.883-00:00"},
  ;                        #uuid "6d60ad5c-bc53-4ebc-80fe-d70d545921ca"
  ;                        {:task/id           #uuid "6d60ad5c-bc53-4ebc-80fe-d70d545921ca",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "0b1efc93-bd08-4131-9b9f-578752ffdb61"
  ;                        {:task/id           #uuid "0b1efc93-bd08-4131-9b9f-578752ffdb61",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "8bf81b38-0da7-46b4-9e14-50724d48f83c"
  ;                        {:task/id           #uuid "8bf81b38-0da7-46b4-9e14-50724d48f83c",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "92494017-2b19-42ed-8765-eb27cf12c463"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "92494017-2b19-42ed-8765-eb27cf12c463",
  ;                         :task/description  "Left warrior",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.864-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.864-00:00"},
  ;                        #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960",
  ;                         :task/description  "Butterfly on back",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.904-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.904-00:00"},
  ;                        #uuid "4e48a155-4842-46a6-9c5c-ec6dce921335"
  ;                        {:task/id           #uuid "4e48a155-4842-46a6-9c5c-ec6dce921335",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513",
  ;                         :task/description  "Left hamstring",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.873-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.873-00:00"},
  ;                        #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89",
  ;                         :task/description  "Square hips left",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.913-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.913-00:00"},
  ;                        #uuid "3cf00493-0d05-4aa3-9164-c73fa5c72c86"
  ;                        {:task/subtasks
  ;                                            [[:task/id #uuid "8a7d548a-6994-4397-9be3-928b87efef7d"]
  ;                                             [:task/id #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b"]
  ;                                             [:task/id #uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059"]
  ;                                             [:task/id #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956"]
  ;                                             [:task/id #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32"]
  ;                                             [:task/id #uuid "92494017-2b19-42ed-8765-eb27cf12c463"]
  ;                                             [:task/id #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513"]
  ;                                             [:task/id #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5"]
  ;                                             [:task/id #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35"]
  ;                                             [:task/id #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960"]
  ;                                             [:task/id #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89"]
  ;                                             [:task/id #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4"]],
  ;                         :task/duration     #time/duration "PT20M",
  ;                         :task/id           #uuid "3cf00493-0d05-4aa3-9164-c73fa5c72c86",
  ;                         :task/description  "Stretching session",
  ;                         :task/scheduled-at #time/offset "nil PT20H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.945-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.945-00:00"},
  ;                        #uuid "5aade06c-f9f0-4054-8c70-a99f613816e5"
  ;                        {:task/subtasks
  ;                                            [[:task/id #uuid "8a7d548a-6994-4397-9be3-928b87efef7d"]
  ;                                             [:task/id #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b"]
  ;                                             [:task/id #uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059"]
  ;                                             [:task/id #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956"]
  ;                                             [:task/id #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32"]
  ;                                             [:task/id #uuid "92494017-2b19-42ed-8765-eb27cf12c463"]
  ;                                             [:task/id #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513"]
  ;                                             [:task/id #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5"]
  ;                                             [:task/id #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35"]
  ;                                             [:task/id #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960"]
  ;                                             [:task/id #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89"]
  ;                                             [:task/id #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4"]],
  ;                         :task/duration     #time/duration "PT20M",
  ;                         :task/id           #uuid "5aade06c-f9f0-4054-8c70-a99f613816e5",
  ;                         :task/description  "Stretching session",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.932-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.932-00:00"},
  ;                        #uuid "1c7785db-745e-4d58-880c-6070ad6971fd"
  ;                        {:task/id           #uuid "1c7785db-745e-4d58-880c-6070ad6971fd",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "c11dd95b-dd32-463b-a62a-f95dfa627e81"
  ;                        {:task/id           #uuid "c11dd95b-dd32-463b-a62a-f95dfa627e81",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "8a7d548a-6994-4397-9be3-928b87efef7d"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/id           #uuid "8a7d548a-6994-4397-9be3-928b87efef7d",
  ;                         :task/description  "Neck stretch",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.816-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.816-00:00"},
  ;                        #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32",
  ;                         :task/description  "Right warrior",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.855-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.855-00:00"},
  ;                        #uuid "86ed2803-7008-4032-bd1f-e706cf5b75f9"
  ;                        {:task/id           #uuid "86ed2803-7008-4032-bd1f-e706cf5b75f9",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4",
  ;                         :task/description  "Square hips right",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.922-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.922-00:00"},
  ;                        #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/id           #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b",
  ;                         :task/description  "Touch your toes",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.824-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.824-00:00"},
  ;                        #uuid "5b78a576-7c67-4163-9fed-5d7966704416"
  ;                        {:task/id           #uuid "5b78a576-7c67-4163-9fed-5d7966704416",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "179b6dc7-be1a-49c9-b391-c09f0410e16a"
  ;                        {:task/id           #uuid "179b6dc7-be1a-49c9-b391-c09f0410e16a",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "a16599ea-def4-405e-9138-b0d7c5d51908"
  ;                        {:task/id           #uuid "a16599ea-def4-405e-9138-b0d7c5d51908",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956"
  ;                        {:task/subtasks     [],
  ;                         :task/duration     #time/duration "PT2M",
  ;                         :task/id           #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956",
  ;                         :task/description  "Right pigeon",
  ;                         :task/scheduled-at #time/offset "nil PT10H",
  ;                         :db/updated-at     #inst "2020-08-26T17:39:13.846-00:00",
  ;                         :db/created-at     #inst "2020-08-26T17:39:13.846-00:00"},
  ;                        #uuid "3367dd67-47d3-405c-96ac-32ec53b28dd5"
  ;                        {:task/id           #uuid "3367dd67-47d3-405c-96ac-32ec53b28dd5",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"},
  ;                        #uuid "230bca48-dfc6-41d6-b6d1-ac4cf7ad9856"
  ;                        {:task/id           #uuid "230bca48-dfc6-41d6-b6d1-ac4cf7ad9856",
  ;                         :task/description  "",
  ;                         :task/duration     #time/duration "PT1M",
  ;                         :task/scheduled-at #time/offset "nil PT10H"}},
  ;   :habit-record/task-records
  ;                       [[:task-record/id #uuid "bb56c129-6f65-402b-a00b-2680c2f42525"]
  ;                        [:task-record/id #uuid "711dcffa-9526-41ce-9849-876b5c167c15"]],
  ;   :habit-record/state :incomplete,
  ;   :task-record/id
  ;                       {#uuid "ad689e78-3e04-485b-a52e-03cd84e70c25"
  ;                        {:task-record/id              #uuid "ad689e78-3e04-485b-a52e-03cd84e70c25",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:46.151",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "a6ab0152-bca8-4362-b9d8-e12c2588f716"
  ;                        {:task-record/id              #uuid "a6ab0152-bca8-4362-b9d8-e12c2588f716",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:13.966",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "d5c535a1-7347-462a-9d0f-2c3a30089b8e"
  ;                        {:task-record/id              #uuid "d5c535a1-7347-462a-9d0f-2c3a30089b8e",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:59.636",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "8aba2053-2e2f-44ad-9b79-e7abcd9ebe7b"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "fb40b15a-e04f-4e08-b6d8-1e404737bc7c"
  ;                        {:task-record/id              #uuid "fb40b15a-e04f-4e08-b6d8-1e404737bc7c",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:09.043",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "b026b078-466e-4589-91de-d5a7d9f7edbd"
  ;                        {:task-record/id              #uuid "b026b078-466e-4589-91de-d5a7d9f7edbd",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:47.648",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "cff6c9c2-a5dc-4ed6-9b78-2951c9e02b89"
  ;                        {:task-record/id              #uuid "cff6c9c2-a5dc-4ed6-9b78-2951c9e02b89",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:12.897",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "bc814d5c-8899-4ae7-a701-7db3a1dc84a8"
  ;                        {:task-record/id              #uuid "bc814d5c-8899-4ae7-a701-7db3a1dc84a8",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:55.631",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "eee2ab3a-ddbc-4c40-9ed0-9dd12ffb4513"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "8e5c3b8c-96f7-4473-b9c4-7d4d0dfb0d6d"
  ;                        {:task-record/id              #uuid "8e5c3b8c-96f7-4473-b9c4-7d4d0dfb0d6d",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:55.006",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "758c9b1c-83c2-4720-a64c-ad6a8f6fe758"
  ;                        {:task-record/id              #uuid "758c9b1c-83c2-4720-a64c-ad6a8f6fe758",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:12.276",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "1487b0ba-efe4-4bf5-be16-049d2c7d560c"
  ;                        {:task-record/id              #uuid "1487b0ba-efe4-4bf5-be16-049d2c7d560c",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:05.415",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "6d2523df-cba9-4c39-b5be-300bdb512dab"
  ;                        {:task-record/id              #uuid "6d2523df-cba9-4c39-b5be-300bdb512dab",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:11.112",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "589ddf2b-05d6-45e9-9443-467c51a86df5"
  ;                        {:task-record/id              #uuid "589ddf2b-05d6-45e9-9443-467c51a86df5",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:46.884",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "68f9c29d-1eb6-46f4-89ef-e0139937bf89"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "ec2ccf5f-e7e2-4425-9472-74cda561a927"
  ;                        {:task-record/id              #uuid "ec2ccf5f-e7e2-4425-9472-74cda561a927",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:51.609",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "3f4ec2da-059e-422b-a6b7-e8044699a278"
  ;                        {:task-record/id              #uuid "3f4ec2da-059e-422b-a6b7-e8044699a278",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:06.473",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "a9dff527-fe53-4d6d-8e12-df1c4cbd8960"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "cb037cc1-72d3-4a92-aa1d-194110e9eb23"
  ;                        {:task-record/id              #uuid "cb037cc1-72d3-4a92-aa1d-194110e9eb23",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:56.504",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "92494017-2b19-42ed-8765-eb27cf12c463"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "dc41814d-5d25-46ce-9f11-b10964d5eb42"
  ;                        {:task-record/id              #uuid "dc41814d-5d25-46ce-9f11-b10964d5eb42",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:08.450",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "9f728c7d-a2cb-4b7d-98f0-b2c3c6c7f4a5"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "c7167a37-eb3a-4d1f-be56-01a00fe82855"
  ;                        {:task-record/id              #uuid "c7167a37-eb3a-4d1f-be56-01a00fe82855",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:58.095",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "882f1611-546d-4623-9e72-c6f0f8d2e956"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "c317d926-1c64-40f6-94a5-9ae1df235a41"
  ;                        {:task-record/id              #uuid "c317d926-1c64-40f6-94a5-9ae1df235a41",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:06.974",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "29a4757a-d4ee-47ff-8e22-42d1458fad35"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "8e3b782c-5c3f-4abd-ba09-c262f3499329"
  ;                        {:task-record/id              #uuid "8e3b782c-5c3f-4abd-ba09-c262f3499329",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T20:48:13.493",
  ;                         :task-record/state           :incomplete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "8a7d548a-6994-4397-9be3-928b87efef7d"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "d1bef0fc-daf6-40ce-a3f8-f8612c13432a"
  ;                        {:task-record/id              #uuid "d1bef0fc-daf6-40ce-a3f8-f8612c13432a",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:00.701",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "8a7d548a-6994-4397-9be3-928b87efef7d"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "bb56c129-6f65-402b-a00b-2680c2f42525"
  ;                        {:task-record/id    #uuid "bb56c129-6f65-402b-a00b-2680c2f42525",
  ;                         :task-record/date  #time/date "2020-08-27",
  ;                         :task-record/state :incomplete,
  ;                         :task-record/task
  ;                                            [:task/id #uuid "5aade06c-f9f0-4054-8c70-a99f613816e5"],
  ;                         :task-record/subtask-records
  ;                                            [[:task-record/id #uuid "8e3b782c-5c3f-4abd-ba09-c262f3499329"]
  ;                                             [:task-record/id #uuid "a6ab0152-bca8-4362-b9d8-e12c2588f716"]
  ;                                             [:task-record/id #uuid "cff6c9c2-a5dc-4ed6-9b78-2951c9e02b89"]
  ;                                             [:task-record/id #uuid "758c9b1c-83c2-4720-a64c-ad6a8f6fe758"]
  ;                                             [:task-record/id #uuid "6d2523df-cba9-4c39-b5be-300bdb512dab"]
  ;                                             [:task-record/id #uuid "ccc91142-c5b9-4028-aa5f-062c51e834dd"]
  ;                                             [:task-record/id #uuid "fb40b15a-e04f-4e08-b6d8-1e404737bc7c"]
  ;                                             [:task-record/id #uuid "dc41814d-5d25-46ce-9f11-b10964d5eb42"]
  ;                                             [:task-record/id #uuid "c317d926-1c64-40f6-94a5-9ae1df235a41"]
  ;                                             [:task-record/id #uuid "3f4ec2da-059e-422b-a6b7-e8044699a278"]
  ;                                             [:task-record/id #uuid "1487b0ba-efe4-4bf5-be16-049d2c7d560c"]
  ;                                             [:task-record/id #uuid "be755a09-f598-44b9-8a42-bb6c58c815b0"]]},
  ;                        #uuid "83cce504-3de4-4bf5-a287-cab0be735c2a"
  ;                        {:task-record/id              #uuid "83cce504-3de4-4bf5-a287-cab0be735c2a",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:58.648",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "6d82023d-2ef0-4a1e-8dcd-ad315415b059"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "ccc91142-c5b9-4028-aa5f-062c51e834dd"
  ;                        {:task-record/id              #uuid "ccc91142-c5b9-4028-aa5f-062c51e834dd",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:09.996",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "92494017-2b19-42ed-8765-eb27cf12c463"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "015b06ca-67ae-4293-9c3a-a443415855ff"
  ;                        {:task-record/id              #uuid "015b06ca-67ae-4293-9c3a-a443415855ff",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:15:57.124",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "63b68a66-4e6b-4873-8097-e99432c8bc32"],
  ;                         :task-record/subtask-records []},
  ;                        #uuid "711dcffa-9526-41ce-9849-876b5c167c15"
  ;                        {:task-record/id    #uuid "711dcffa-9526-41ce-9849-876b5c167c15",
  ;                         :task-record/date  #time/date "2020-08-27",
  ;                         :task-record/state :complete,
  ;                         :task-record/task
  ;                                            [:task/id #uuid "3cf00493-0d05-4aa3-9164-c73fa5c72c86"],
  ;                         :task-record/subtask-records
  ;                                            [[:task-record/id #uuid "d1bef0fc-daf6-40ce-a3f8-f8612c13432a"]
  ;                                             [:task-record/id #uuid "d5c535a1-7347-462a-9d0f-2c3a30089b8e"]
  ;                                             [:task-record/id #uuid "83cce504-3de4-4bf5-a287-cab0be735c2a"]
  ;                                             [:task-record/id #uuid "c7167a37-eb3a-4d1f-be56-01a00fe82855"]
  ;                                             [:task-record/id #uuid "015b06ca-67ae-4293-9c3a-a443415855ff"]
  ;                                             [:task-record/id #uuid "cb037cc1-72d3-4a92-aa1d-194110e9eb23"]
  ;                                             [:task-record/id #uuid "bc814d5c-8899-4ae7-a701-7db3a1dc84a8"]
  ;                                             [:task-record/id #uuid "8e5c3b8c-96f7-4473-b9c4-7d4d0dfb0d6d"]
  ;                                             [:task-record/id #uuid "ec2ccf5f-e7e2-4425-9472-74cda561a927"]
  ;                                             [:task-record/id #uuid "b026b078-466e-4589-91de-d5a7d9f7edbd"]
  ;                                             [:task-record/id #uuid "589ddf2b-05d6-45e9-9443-467c51a86df5"]
  ;                                             [:task-record/id #uuid "ad689e78-3e04-485b-a52e-03cd84e70c25"]]},
  ;                        #uuid "be755a09-f598-44b9-8a42-bb6c58c815b0"
  ;                        {:task-record/id              #uuid "be755a09-f598-44b9-8a42-bb6c58c815b0",
  ;                         :task-record/date            #time/date "2020-08-27",
  ;                         :task-record/recorded-at     #time/date-time "2020-08-27T12:16:04.902",
  ;                         :task-record/state           :complete,
  ;                         :task-record/task
  ;                                                      [:task/id #uuid "15cbd103-67b5-4091-8f71-3ee324434ba4"],
  ;                         :task-record/subtask-records []}},
  ;   :habit-record/habit
  ;                       [:habit/id #uuid "6a35e95f-b926-4bdf-9c34-761d8b35c903"]})

  )

(defn move-item-up
  "Move an element to the prior index in a vector"
  [avec index]
  (assert (and (> index 0) (<= index (dec (count avec)))))
  (let [item   (get avec index)
        item-1 (get avec (dec index))]
    (-> avec
      (assoc (dec index) item)
      (assoc index item-1))))
(comment
  (move-item-up [1 2 3 4] -1)
  (move-item-up [1 2 3 4] 4)
  (move-item-up [1 2 3 4] 1))

(defn move-item-down
  "Move an element to the next index in a vector (swap with subsequent item)"
  [avec index]
  (assert (and (>= index 0) (< index (dec (count avec)))))
  (let [item   (get avec index)
        item+1 (get avec (inc index))]
    (-> avec
      (assoc (inc index) item)
      (assoc index item+1))))

(comment
  (move-item-down [1 2 3 4] 0)
  (move-item-down [1 2 3 4] 3)
  (move-item-down [1 2 3 4] 2)
  (move-item-down [1 2 3 4] 1))


#?(:cljs
   (>defn uuid
     "Without args gives random UUID.
      With args, builds UUID based on input (useful in tests)."
     ([] [=> uuid?] (random-uuid))
     ([s] [any? => any?] (cljs.core/uuid s)))

   :clj
   (>defn uuid
     "Without args gives random UUID.
     With args, builds UUID based on input (useful in tests)."
     ([] [=> uuid?] (UUID/randomUUID))
     ([int-or-str]
      [(s/or :i int? :s string?) => uuid?]
      (if (int? int-or-str)
        (UUID/fromString
          (format "ffffffff-ffff-ffff-ffff-%012d" int-or-str))
        (UUID/fromString int-or-str))))
   )
