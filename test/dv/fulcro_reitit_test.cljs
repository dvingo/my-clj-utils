(ns dv.fulcro-reitit-test
  (:require
    [dv.fulcro-reitit :as fr]
    [dv.fulcro-reitit2 :as fr2]
    [clojure.test :refer :all]
    [reitit.core :as r]
    [clojure.spec.alpha :as s]))


(def router)



(deftest construct-fulcro-segments-test
  (let [out  (fr/construct-fulcro-segments {:template    "/calendar"
                                            :data        {:segment ["calendar" (fn [_] (-> (js/Date.) .toISOString))] :name :calendar}
                                            :result      nil
                                            :path-params {}
                                            :path        "/calendar"
                                            })

        out2 (fr/construct-fulcro-segments
               (r/match-by-path router "/test"))]
    (println "out: " out)
    (println "out2: " out2)
    ))

(deftest test-init-router-state
  (let [routes
            [["/" {:name :root :segment ["tasks"]}]
             ["/tasks" {:name :tasks :segment ["tasks"]}]
             ["/calendar" {:segment ["calendar"]}
              ["" {:name :calendar :segment [(fn [_] "202020")]}]
              ["/:date" {:name :calendar-date :segment [:date]}]]
             ["/signup" {:name :signup :segment ["signup"]}]]
        out (fr2/init-router-state {} routes)]
    (s/valid? ::fr2/route-state out)
    #_{:dv.fulcro-reitit2/router {:router {; :reitit-router           #object[reitit.core.t_reitit$core133229],
                                         :routes-by-name          {:root          {:name :root, :segment ["tasks"], :path "/"},
                                                                   :tasks         {:name :tasks, :segment ["tasks"], :path "/tasks"},
                                                                   :calendar      {:segment ["calendar"],
                                                                                   :name    :calendar,
                                                                                   :path    "/calendar"},
                                                                   :calendar-date {:segment ["calendar" :date],
                                                                                   :name    :calendar-date,
                                                                                   :path    "/calendar/:date"},
                                                                   :signup        {:name :signup, :segment ["signup"], :path "/signup"}},
                                         :current-fulcro-route    [],
                                         :redirect-loop-count     0,
                                         :max-redirect-loop-count 10}}}
    ))
