(ns dv.devcards-fulcro3
  (:require
    [devcards.core :as dc :refer (defcard)]
    [taoensso.timbre :as log])
  (:import [java.util UUID]))

(defmacro make-card
  "Create a devcard for a fulcro component.
  Takes symbol of fulcro component and options."
  ([component]
   `(make-card ~component {}))
  ([component {::keys [wrap-root? root-state use-sablono? use-reagent?]
               :or    {wrap-root? true}
               :as    opts}]
   (let [id         (UUID/randomUUID)
         app        (symbol (str (name component) "devcards-fulcro3-app"))
         config     (gensym "config")]
     `(let [~config
            {::root                      ~component
             ::root-state                ~root-state
             ::wrap-root?                ~wrap-root?
             ::persistence-key           ~id
             ::app                       (cond-> {}
                                           ~use-reagent?
                                           (assoc :render-middleware (fn [_# render#] (@r-as-element (render#))))
                                           ~use-sablono?
                                           (assoc :render-middleware (fn [_# render#] (@html (render#)))))
             :fulcro.inspect.core/app-id ~id}]
        (defonce ~app (upsert-app ~config))
        (defcard ~(symbol (str (name component) "-card"))
          (dc/dom-node
            (fn [_# dom-node#]
              (mount-at ~app
                {::root            ~component
                 ::wrap-root?      ~wrap-root?
                 ::persistence-key ::test1}
                dom-node#))))))))

(defmacro make-reagent-card
  ([component]
   `(make-reagent-card ~component {}))
  ([component opts]
   `(make-card ~component ~(merge opts {::use-reagent? true}))))

(defmacro make-no-root-card
  [component opts]
  `(make-card ~component ~(merge opts {::wrap-root? false})))

(defmacro make-reagent-no-root-card
  ([component]
   `(make-reagent-no-root-card ~component {}))
  ([component opts]
   `(make-card ~component ~(merge opts {::wrap-root? false ::use-reagent? true}))))

(comment
  (macroexpand-1
    (macroexpand-1 '(make-card FulcroDemo))))
