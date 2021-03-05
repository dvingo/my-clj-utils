(ns dv.fulcro-reitit-test
  (:require
    [dv.fulcro-reitit :as fr]
    [clojure.test :refer :all]
    [reitit.core :as r]
    ))


(def router )



(deftest construct-fulcro-segments-test
  (let [out (fr/construct-fulcro-segments {:template    "/calendar"
                                           :data        {:segment ["calendar" (fn [_] (-> (js/Date.) .toISOString))] :name :calendar}
                                           :result      nil
                                           :path-params {}
                                           :path        "/calendar"
                                           })

        out2 (fr/construct-fulcro-segments
          (r/match-by-path router "/test")) ]
    (println "out: " out)
    (println "out2: " out2)
    ))
