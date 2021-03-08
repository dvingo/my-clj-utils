(ns dv.fulcro-storybook
  (:require [clojure.spec.alpha :as s]))

(s/def ::args (s/cat :name (s/? symbol?) :forms (s/+ any?)))

(defn fulcro-component* [component-name body]
  (let [dummy-prop (keyword (str (gensym)) (str (gensym)))]
    `(~'defsc ~component-name [~'_ ~'_]
       {:query [~dummy-prop]
        :initial-state {~dummy-prop nil}}
       ~@body)))

(defmacro make-storym-orig [story--name args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [forms] :as opts} (s/conform ::args args)
        ;_ (println "name: " (:name opts))
        story-name (or (:name opts) (gensym "story"))
        c-name (gensym "component")
        fulcro-component (fulcro-component* c-name forms)]
    `(do
       ~fulcro-component
       (def ~story-name (make-story ~c-name))
       ;; this symbol should be fully qualified and 'munged'.
       (goog/exportSymbol ~(str story-name) ~story-name))))

(defmacro make-storym [story-name args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [forms] :as opts} (s/conform ::args args)
        ;_ (println "name: " (:name opts))
        story-name (or (:name opts) (gensym "story"))
        c-name (gensym "component")
        fulcro-component (fulcro-component* c-name forms)]
    `(do
      ~fulcro-component
      (def ~story-name (make-story ~c-name))
      )))

;; the above export doesn't work because shadow-cljs doesn't add it to
;; the module.exports, but the symbol will be in the output.

(defmacro def-fulcro-class [& args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [forms] :as opts} (s/conform ::args args)
        cls-name (or (:name opts) (gensym "story"))
        fulcro-component (fulcro-component* cls-name forms)]
    fulcro-component))
