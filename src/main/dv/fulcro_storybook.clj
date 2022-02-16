(ns dv.fulcro-storybook
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as c]
    [clojure.spec.alpha :as s]))

(s/def ::args (s/cat :name (s/? symbol?) :forms (s/+ any?)))

(defn fulcro-component* [component-name body]
  (let [dummy-prop (keyword (str (gensym)) (str (gensym)))]
    `(c/defsc ~component-name [~'_ ~'_]
       {:query         [~dummy-prop]
        :initial-state {~dummy-prop nil}}
       ~@body)))

(defmacro make-storym-orig [story--name args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [forms] :as opts} (s/conform ::args args)
        ;_ (println "name: " (:name opts))
        story-name       (or (:name opts) (gensym "story"))
        c-name           (gensym "component")
        fulcro-component (fulcro-component* c-name forms)]
    `(do
       ~fulcro-component
       (def ~story-name (make-story ~c-name))
       ;; this symbol should be fully qualified and 'munged'.
       (goog/exportSymbol ~(str story-name) ~story-name))))

(defn fulcro-component2* [component-name body]
  (let [dummy-prop (keyword (str (gensym)) (str (gensym)))]
    `(c/defsc ~component-name [~'_ ~'_]
       {:query         [~dummy-prop]
        :initial-state {~dummy-prop nil}}
       ~body)))

(defmacro make-storym [story-name body]
  (let [c-name           (gensym "component")
        fulcro-component (fulcro-component2* c-name body)]
    `(do
       ~fulcro-component
       (let [mount-fulcro-app# (make-mount-fulcro-app ~c-name)]
         (defn ~story-name {:export true} []
           (react/createElement mount-fulcro-app#))))))

(defmacro make-storym2 [story-name body]
  (let [c-name           (gensym "component")
        fulcro-component (fulcro-component2* c-name body)]
    `(do
       ~fulcro-component
       (def ~(vary-meta story-name assoc :export true)
         (make-story ~c-name)))))

(defmacro test-it
  [nm v]
  `(def ~(vary-meta nm assoc :export true) ~v))

(defmacro def-fulcro-class [& args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [forms] :as opts} (s/conform ::args args)
        cls-name         (or (:name opts) (gensym "story"))
        fulcro-component (fulcro-component* cls-name forms)]
    fulcro-component))

(defmacro export-default
  [opts]
  (let [ns-name (str/replace (str (-> &env :ns :name)) "." "/")]
    `(def ~(vary-meta 'default assoc :export true)
       (cljs.core/clj->js (assoc ~opts :title ~ns-name)))))
