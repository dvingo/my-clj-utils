(ns dv.fulcro-storybook
  (:require
    ["react" :as react]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as c]
    [com.fulcrologic.fulcro.dom :as dom]
    [goog.object :as gobj]
    [taoensso.timbre :as log]
    [reagent.dom :as rdom]
    [reagent.core :as r])
  (:require-macros [dv.fulcro-storybook :refer [make-storym def-fulcro-class]]))

(defn get-initial-state [comp params]
  (if (c/has-initial-app-state? comp)
    (c/get-initial-state comp params)
    params))

(defn make-root [Root]
  (let [generated-name (gensym)
        component-key  (keyword "storybook-fulcro" (name generated-name))]
    (c/configure-component! (fn *dyn-root* []) component-key
      {:initial-state (fn [_ params]
                        {:ui/root (or (get-initial-state Root params) {})})
       :query         (fn [_] [:fulcro.inspect.core/app-id {:ui/root (c/get-query Root)}])
       :render        (fn [this]
                        (let [{:ui/keys [root]} (c/props this)
                              Root     (-> Root c/class->registry-key c/registry-key->class)
                              factory  (c/factory Root)
                              computed (c/shared this ::computed)]
                          ;(log/info "The root data is : " root)
                          (if (seq root)
                            (factory
                              (cond-> root computed (c/computed computed))))))})))

(defn make-fulcro-app []
  (app/fulcro-app
    {:render-root! rdom/render
     :render-middleware (fn [this render] (r/as-element (render)))}))

(defn make-story [cls]
  (let [Root (make-root cls)
        new-cls (r/create-class
                  {:component-did-mount
                   (fn [this]
                     (when-let [dom-node (gobj/get this "el")]
                       (log/info "Mounting fulcro story.")
                       (app/mount! (make-fulcro-app) Root dom-node {:initialize-state? true})))
                   :render
                   (fn [this] (dom/div {:ref (fn [r] (gobj/set this "el" r))}))})]
    #(react/createElement new-cls)))
