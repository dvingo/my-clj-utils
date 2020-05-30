(ns dv.crux_pull
  (:require
    [crux.api :as crux]
    [datascript.pull-parser :as dpp]
    [taoensso.timbre :as log])
  (:import [datascript.pull_parser PullSpec]))

;; todo update to use db instead of node so this works for time travel
(def crux-entity)
;(defn crux-entity [id] (crux/entity (crux/db crux-node) id))

(defn- into!
  [transient-coll items]
  (reduce conj! transient-coll items))

(def ^:private ^:const +default-limit+ 1000)

(defn- initial-frame
  [pattern eids multi?]
  {:state     :pattern
   :pattern   pattern
   :wildcard? (:wildcard? pattern)
   :specs     (-> pattern :attrs seq)
   :results   (transient [])
   :kvps      (transient {})
   :eids      eids
   :multi?    multi?
   :recursion {:depth {} :seen #{}}})

(defn- subpattern-frame
  [pattern eids multi? attr]
  (assoc (initial-frame pattern eids multi?) :attr attr))

(defn- reset-frame
  [frame eids kvps]
  (let [pattern (:pattern frame)]
    (assoc frame
      :eids eids
      :specs (seq (:attrs pattern))
      :wildcard? (:wildcard? pattern)
      :kvps (transient {})
      :results (cond-> (:results frame)
                 (seq kvps) (conj! kvps)))))

(defn- push-recursion
  [rec attr eid]
  (let [{:keys [depth seen]} rec]
    (assoc rec
      :depth (update depth attr (fnil inc 0))
      :seen (conj seen eid))))

(defn- seen-eid?
  [frame eid]
  (-> frame
    (get-in [:recursion :seen] #{})
    (contains? eid)))

(defn- pull-seen-eid
  [frame frames eid]
  (when (seen-eid? frame eid)
    (conj frames (update frame :results conj! {:db/id eid}))))

(defn- single-frame-result
  [key frame]
  (some-> (:kvps frame) persistent! (get key)))

(defn- recursion-result [frame]
  (single-frame-result ::recursion frame))

(defn- recursion-frame
  [parent eid]
  (let [attr (:attr parent)
        rec  (push-recursion (:recursion parent) attr eid)]
    (assoc (subpattern-frame (:pattern parent) [eid] false ::recursion)
      :recursion rec)))

(defn- pull-recursion-frame
  [_ [frame & frames]]
  (log/info "pull-recursion-frame")
  (if-let [eids (seq (:eids frame))]
    (do (log/info "eids are: " eids)
        (let [frame (reset-frame frame (rest eids) (recursion-result frame))
              eid   (first eids)]
          (or (pull-seen-eid frame frames eid)
            (conj frames frame (recursion-frame frame eid)))))
    (let [kvps    (recursion-result frame)
          results (cond-> (:results frame)
                    (seq kvps) (conj! kvps))]
      (conj frames (assoc frame :state :done :results results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uses db
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- recurse-attr
  [db attr multi? eids eid parent frames]
  (log/info "recurse-attr")
  (let [{:keys [recursion pattern]} parent
        depth (-> recursion (get :depth) (get attr 0))]
    (if (-> pattern :attrs (get attr) :recursion (= depth))
      (conj frames parent)
      (pull-recursion-frame
        db
        (conj frames parent
          {:state     :recursion :pattern pattern
           :attr      attr :multi? multi? :eids eids
           :recursion recursion
           :results   (transient [])})))))

(let [pattern (PullSpec. true {})]
  (defn- expand-frame
    [parent eid attr-key multi? eids]
    (let [rec (push-recursion (:recursion parent) attr-key eid)]
      (-> pattern
        (subpattern-frame eids multi? attr-key)
        (assoc :recursion rec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uses db
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pull-attr-datoms
  [db attr-key attr eid forward? datoms opts [parent & frames]]
  (log/info "pull-attr-datoms datoms: " datoms " opts: " opts)
  (let [limit    (get opts :limit +default-limit+)
        attr-key (or (:as opts) attr-key)
        found    (not-empty
                   (cond->> (if (coll? datoms) datoms [datoms])
                     limit (into [] (take limit))
                     true (filterv some?)))]
    (if found
      (let [component? (:subpattern opts)
            multi?     (coll? datoms)
            datom-val  (if forward?
                         identity
                         identity
                         ;(fn [d] (.-v ^Datom d))
                         ;(fn [d] (.-e ^Datom d))
                         )]
        (cond
          (contains? opts :subpattern)
          (->> (subpattern-frame (:subpattern opts)
                 (mapv crux-entity found)
                 multi? attr-key)
            (conj frames parent))

          (contains? opts :recursion)
          (recurse-attr db attr-key multi?
            (mapv crux-entity found)
            eid parent frames)

          (and component? forward?)
          (->> found
            (mapv datom-val)
            (expand-frame parent eid attr-key multi?)
            (conj frames parent))

          :else
          (let [single? (not multi?)]
            (->> (cond-> (into [] (map datom-val) found)
                   single? first)
              (update parent :kvps assoc! attr-key)
              (conj frames)))))
      ;; missing attr value
      (->> (cond-> parent
             (contains? opts :default)
             (update :kvps assoc! attr-key (:default opts)))
        (conj frames)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uses db
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pull-attr
  [db spec eid frames]
  (log/info "---------pull-attr " spec " eid: " eid)
  (let [[attr-key opts] spec]
    (if (= :db/id attr-key)
      frames

      (let [attr     (:attr opts)
            forward? (= attr-key attr)
            results  (if forward?
                       (attr-key eid)
                       ;; todo reverse
                       (attr-key eid)
                       ;(db/-datoms db :eavt [eid attr])
                       ;(db/-datoms db :avet [attr eid])
                       )]
        (pull-attr-datoms db attr-key attr eid forward?
          results opts frames)))))

(def ^:private filter-reverse-attrs
  (filter (fn [[k v]] (not= k (:attr v)))))

(defn- expand-reverse-subpattern-frame
  [parent eid rattrs]
  (-> (:pattern parent)
    (assoc :attrs rattrs :wildcard? false)
    (subpattern-frame [eid] false ::expand-rev)))

;; kvps is a transient map
(defn- expand-result
  [frames kvps]
  (let [res
        (->> kvps
          (persistent!)
          (update (first frames) :kvps into!)
          (conj (rest frames)))
        ]
    res))

(defn- pull-expand-reverse-frame
  [db [frame & frames]]
  (->> (or (single-frame-result ::expand-rev frame) {})
    (into! (:expand-kvps frame))
    (expand-result frames)))

(defn- pull-expand-frame
  [db [frame & frames]]
  (if-let [datoms-by-attr (seq (:datoms frame))]
    (let [[attr datoms] (first datoms-by-attr)
          opts (-> frame
                 (get-in [:pattern :attrs])
                 (get attr {}))]
      (pull-attr-datoms db attr attr (:eid frame) true datoms opts
        (conj frames (update frame :datoms rest))))

    (if-let [rattrs (->> (get-in frame [:pattern :attrs])
                      (into {} filter-reverse-attrs)
                      not-empty)]
      (let [frame (assoc frame
                    :state :expand-rev
                    :expand-kvps (:kvps frame)
                    :kvps (transient {}))]
        (->> rattrs
          (expand-reverse-subpattern-frame frame (:eid frame))
          (conj frames frame)))

      (expand-result frames (:kvps frame)))))

(defn- pull-wildcard-expand
  [db frame frames eid pattern]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; uses db
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [datoms eid
        {:keys [attr recursion]} frame
        rec    (cond-> recursion
                 (some? attr) (push-recursion attr eid))]
    (->> {:state     :expand
          :kvps      (transient {:db/id eid})
          :eid       eid
          :pattern   pattern
          :datoms    nil
          :recursion rec}
      (conj frames frame)
      (pull-expand-frame db))))

(defn- pull-wildcard
  [db frame frames]
  (log/info "pull wildcard")
  (let [{:keys [eid pattern]} frame]
    (or (pull-seen-eid frame frames eid)
      (pull-wildcard-expand db frame frames eid pattern))))

(defn- pull-pattern-frame
  [db [frame & frames]]
  (log/info "pull-pattern-frame " frame)
  (if-let [eids (seq (:eids frame))]
    (if (:wildcard? frame)
      (pull-wildcard db
        (assoc frame
          :specs []
          :eid (first eids)
          :wildcard? false)
        frames)
      (if-let [specs (seq (:specs frame))]
        (let [spec       (first specs)
              new-frames (conj frames (assoc frame :specs (rest specs)))]
          (pull-attr db spec (first eids) new-frames))
        (->> frame :kvps persistent! not-empty
          (reset-frame frame (rest eids))
          (conj frames)
          (recur db))))
    (conj frames (assoc frame :state :done))))

(defn- pull-pattern
  [db frames]
  (log/info "pull-pattern----------------------------------pulling frame with state: " (:state (first frames)))
  (case (:state (first frames))
    :expand (recur db (pull-expand-frame db frames))
    :expand-rev (recur db (pull-expand-reverse-frame db frames))
    :pattern (recur db (pull-pattern-frame db frames))
    :recursion (recur db (pull-recursion-frame db frames))
    :done (let [[f & remaining] frames
                result (persistent! (:results f))
                result (mapv #(if (contains? % :db/id)
                                (:db/id %) %) result)
                result (cond-> result (not (:multi? f)) first)]
            (log/info "\n\nDONE : result: " result)
            (if (seq remaining)
              (->> (cond-> (first remaining)
                     result (update :kvps assoc! (:attr f) result))
                (conj (rest remaining))
                (recur db))
              result))))

(defn entity-for-prop
  [db [attr value]]
  (ffirst (crux/q db
            {:find  ['?e]
             :where [['?e attr value]]
             :args  [{'attr attr 'value value}]})))

(defn entity-with-prop
  "Get an entity that has a property with value. copied from crux site
  (entity-with-prop [:email \"myaddress@addres.com\"])"
  ([eid] (entity-with-prop crux-node eid))
  ([crux-node eid]
   (when eid
     (let [db (crux/db crux-node)]
       (if (vector? eid)
         (let [[attr value] eid]
           (recur crux-node (entity-for-prop db [attr value])))
         (crux/entity db eid))))))

(defn start-entity [e]
  (if (vector? e)
    (entity-with-prop e)
    (crux-entity e)))

(defn pull-spec
  [db pattern eids multi?]
  (let [eids (into [] (map start-entity) eids)]
    (pull-pattern db (list (initial-frame pattern eids multi?)))))

(defn pull [db selector eid]
  (pull-spec db (dpp/parse-pull selector) [eid] false))

(defn pull-many [db selector eids]
  (pull-spec db (dpp/parse-pull selector) eids true))

(comment
  (pull crux-node [:task/description] :dan-test1)

  (dpp/parse-pull [:task/description])
  (initial-frame (dpp/parse-pull [:task/description]) [{}] false)
  (subpattern-frame (dpp/parse-pull [:task/description]) [{}] false :task/description)
  (dpp/parse-pull [:task/description])
  (dpp/parse-pull [{:user/habits [:task/id :task/description]} :user/id])

  (dpp/parse-pull [:name {:children 1}])
  (dpp/parse-pull ['*])
  (dpp/parse-pull [{:user/habits ['*]} :user/id])
  (dpp/parse-pull [:name {:children '...}])
  (dpp/parse-pull [{:hi [:prop-a]} :prop-b])
  (dpp/parse-pull [{:hi [:prop-a]} :prop-b])

  (pull crux-node [:name {:children 1}] :task-1)
  (pull crux-node [[:name :as :other] {[:children :limit 2] '...}] :task-1)

  (crux-entity :dan-test1)
  (pull crux-node [:user/id :user/password {:user/tasks ['*]}] :dan-test1)
  (pull crux-node [:user/id :user/password {:user/tasks [:task/description]}] :dan-test1)

  (pull crux-node [[:user/id :as :hi]
                   {[:user/habits :limit 1]
                    [[:habit/schedule2 :default 10]
                     {:habit/task [[:task/description :as :diff]]}]}]
    #uuid"8d5a0a66-e98f-43ff-8803-e411073d0880")

  (pull crux-node [[:user/id :as :hi]
                   {:user/habits [[:habit/schedule2 :default 10]
                                  {:habit/task [[:task/description :as :diff]]}]
                    }] #uuid"8d5a0a66-e98f-43ff-8803-e411073d0880")

  (pull crux-node [:user/email :user/tasks] [:user/email "abc@abc.com"])
  (pull crux-node [:user/id [:user/id2 :default :hi]] #uuid"8d5a0a66-e98f-43ff-8803-e411073d0880")
  )

