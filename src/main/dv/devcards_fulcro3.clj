(ns dv.devcards-fulcro3
  (:require
    [devcards.core :as dc :refer (defcard)]
    [sablono.core :refer [html]])
  (:import [java.util UUID]))

(defmacro make-card
  "Create a devcard for a fulcro component."
  ([component]
   `(make-card ~component {}))
  ([component {::keys [wrap-root? root-state use-sablono?]
               :or    {wrap-root? true use-sablono? true}
               :as    opts}]
   (let [id     (UUID/randomUUID)
         app    (symbol (str (name component) "devcards-fulcro3-app"))
         config (gensym "config")]
     `(let [~config
            {::root                      ~component
             ::wrap-root?                ~wrap-root?
             ::persistence-key           ~id
             ::app                       (cond-> {} ~use-sablono?
                                           (assoc :render-middleware
                                                  (fn [_# render#] (sablono.core/html (render#)))))
             :fulcro.inspect.core/app-id ~id}]
        (defonce ~app (upsert-app ~config))
        (println "App name is : " '~app)
        (defcard ~(symbol (str (name component) "-card"))
          "# Hellow olrd"
          (dc/dom-node
            (fn [_# dom-node#]
              (mount-at ~app
                {::root            ~component
                 ::wrap-root?      ~wrap-root?
                 ::persistence-key ::test1}
                dom-node#))))))))


(comment
  (macroexpand-1
    (macroexpand-1 '(make-card FulcroDemo))))
