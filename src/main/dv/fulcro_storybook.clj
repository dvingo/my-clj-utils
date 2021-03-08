(ns dv.fulcro-storybook
  (:require [clojure.spec.alpha :as s]))

(s/def ::args (s/cat :name (s/? symbol?) :forms (s/+ any?)))

(defn fulcro-component* [component-name body]
  (let [dummy-prop (keyword (str (gensym)) (str (gensym)))]
    `(~'defsc ~component-name [~'_ ~'_]
       {:query [~dummy-prop]
        :initial-state {~dummy-prop nil}}
       ~@body)))

(defmacro make-storym [& args]
  (when-not (s/valid? ::args args)
    (throw (ex-info "Invalid arguments to make-story: " {:args args})))
  (let [{:keys [name forms]} (s/conform ::args args)
        story-name (or name (gensym "story"))
        c-name     (gensym "component")
        fulcro-component (fulcro-component* c-name forms)]
    `(do
       ~fulcro-component
       ;; this metatdata is not getting to the output need to use with-meta probably.
       (~'def ^{:export true} ~story-name (make-story ~c-name)))))

(comment
  (macroexpand-1 '(make-storym hello-story [:h1 "hello"]))
  (macroexpand-1 '(make-storym [:h1 "hello"]))
  (s/conform ::args '(name (tv/ui-hello {:arg "a arg"})))
  (s/conform ::args '((tv/ui-hello {:arg "a arg"})))
  )
