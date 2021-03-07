(ns dv.fulcro-reitit-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is testing]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as c :refer [defsc]]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
    [com.fulcrologic.guardrails.core :refer [>defn => | ? >def]]
    [dv.fulcro-reitit :as fr]
    [reitit.core :as r]))

(deftest construct-fulcro-segments-test
  (let [out (fr/construct-fulcro-segments {:template    "/calendar"
                                           :data        {:segment ["calendar" (fn [_] (-> (js/Date.) .toISOString (.split "T") first))]
                                                         :name    :calendar}
                                           :result      nil
                                           :path-params {}
                                           :path        "/calendar"})]
    (is (= ["calendar" (-> (js/Date.) .toISOString (.split "T") first)] out))))

(deftest test-init-router-state
  (let [routes
            [["/" {:name :root :segment ["tasks"]}]
             ["/tasks" {:name :tasks :segment ["tasks"]}]
             ["/calendar" {:segment ["calendar"]}
              ["" {:name :calendar :segment [(fn [_] "202020")]}]
              ["/:date" {:name :calendar-date :segment [:date]}]]
             ["/signup" {:name :signup :segment ["signup"]}]]
        out (fr/initial-router-state routes)]
    (is (s/valid? ::fr/router-state out))))

(comment

  (vec (concat
         ["/top" {:name :top}]
         [["/nested" {:name :nested}]]))
  ["/top" {:name :top}]
  [["/nested" {:name :nested}]]

  ;; works
  (concat-sub-routes
    ["/:task-id/edit" {:name :edit-task :segment ["edit" :task-id]}]
    [[["/nested/another" {:name :nested :segment ["nested"]}]
      ["/:task-id" {:name :view-task :segment ["view" :task-id]}]]])
  )

(defsc ViewTask [this props]
  {:route-segment ["view" :task-id]
   :query         []
   ::fr/route     ["/:task-id" {:name :view-task :segment ["view" :task-id]}]})

(def ui-view-task (c/factory ViewTask))

(defsc Nested [this props]
  {:query         []
   :route-segment ["nested"]
   ::fr/route     ["/nested/another" {:name :nested :segment ["nested"]}]})

(dr/defrouter Nested2 [_ _] {:router-targets [Nested]})

(defsc EditTask [_ _]
  {:route-segment ["edit" :task-id]
   :query         [{:nested (c/get-query Nested2)}]
   ::fr/route     ["/:task-id/edit" {:name :edit-task :segment ["edit" :task-id]}]})

(dr/defrouter TaskRouter [_ _] {:router-targets [EditTask ViewTask]})

(defsc Target2 [_ _]
  {::fr/route     ["/signup" {:name :signup :segment ["signup"]}]
   :query         []
   :route-segment ["target2"]})

(defsc Target1 [_ _]
  {::fr/route     [^:alias ["/" {:name :root :segment ["tasks"]}]
                   ["/tasks" {:name :tasks :segment ["tasks"]}]]
   :route-segment ["tasks"]
   :query         [{:task-router (c/get-query TaskRouter)}]})

(dr/defrouter TopRouter [_ _] {:router-targets [Target1 Target2]})

(comment
  (gather-recursive TopRouter)
  ;;
  (def r (r/router (gather-recursive TopRouter)))
  (r/routes r)
  (r/match-by-path r "/tasks/nested/another")
  )

(deftest test-gather-recursive
  (let [out (fr/gather-recursive TopRouter)]
    ;(println "LAST one out: " out)
    (is (=
          [["/" {:name :root, :segment ["tasks"]}]
           ["/tasks"
            {:segment ["tasks"]}
            ["" {:name :tasks}]
            ["/:task-id/edit"
             {:segment ["edit" :task-id]}
             ["" {:name :edit-task}]
             ["/nested/another" {:name :nested, :segment ["nested"]}]]
            ["/:task-id" {:name :view-task, :segment ["view" :task-id]}]]
           ["/signup" {:name :signup, :segment ["signup"]}]]
          out))))

;; todo test when there are no reitit routes on a target fulcro router
;; edge case you need to handle
;;

(comment
  (dr/get-targets TopRouter)
  (println "HI")
  (fr/gather-recursive TopRouter)
  (test-gather-recursive))
