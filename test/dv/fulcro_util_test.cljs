(ns dv.fulcro-util-test
  (:require
    [cljs.pprint :refer [pprint]]
    [dv.fulcro-util :as fu]
    [clojure.test :refer [deftest is]]))

(def expected
  {:habit/id            #uuid "e777425f-e28e-45b0-9ccf-f007b603d02f",
   :habit/starts-on     "UnknownTransitType: 2020-08-06",
   :habit/criteria-num  2,
   :habit/description   "Right then",
   :habit/active?       true,
   :habit/repeats-every "UnknownTransitType: P1D",
   :habit/tasks
                        [{:task/subtasks     [],
                          :task/duration     "UnknownTransitType: PT1M",
                          :task/id           #uuid "1f2400de-af54-42e1-852d-48287d69f0d4",
                          :task/description  "chagnge",
                          :task/scheduled-at "UnknownTransitType: [object Object]",
                          :db/updated-at     #inst "2020-08-06T20:31:24.755-00:00",
                          :db/created-at     #inst "2020-08-06T19:47:07.686-00:00"}
                         {:task/subtasks     [],
                          :task/duration     "UnknownTransitType: PT1M",
                          :task/id           #uuid "8cf7f2eb-b095-4e36-a9a4-5f424cccc48c",
                          :task/description  "Drink 1 liter of water",
                          :task/scheduled-at "UnknownTransitType: [object Object]",
                          :db/updated-at     #inst "2020-08-06T19:47:07.696-00:00",
                          :db/created-at     #inst "2020-08-06T19:47:07.696-00:00"}],
   :habit/criteria      :min-of})

(deftest elide-client-only-values-test
  (let [out
        (fu/elide-client-only-values
          {:habit/id                          #uuid "e777425f-e28e-45b0-9ccf-f007b603d02f",
           :fulcro.client.primitives/computed {:some-key "value"}
           :habit/starts-on                   "UnknownTransitType: 2020-08-06",
           :habit/criteria-num                2,
           :habit/description                 "Right then",
           :habit/active?                     true,
           :habit/repeats-every               "UnknownTransitType: P1D",
           :habit/tasks
                                              [{:task/subtasks                [],
                                                :ui/has-duration?             false,
                                                :ui/has-scheduled-at?         false,
                                                :ui/show-delete-confirmation? false,
                                                :task/duration                "UnknownTransitType: PT1M",
                                                :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                              {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                      [:task/id #uuid "1f2400de-af54-42e1-852d-48287d69f0d4"],
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                      #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                        :task/description
                                                                                                                                        :task/scheduled-at},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/complete?
                                                                                                                                      #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                        :task/description
                                                                                                                                        :task/scheduled-at},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                      {:ui/has-duration?     false,
                                                                                                                                       :ui/has-scheduled-at? false,
                                                                                                                                       :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                       :task/description     "chagnge",
                                                                                                                                       :task/scheduled-at    "UnknownTransitType: [object Object]"}},
                                                :ui/machine-state             :state/not-submitting,
                                                :task/id                      #uuid "1f2400de-af54-42e1-852d-48287d69f0d4",
                                                :ui/subtask-form
                                                                              {:task/id              #uuid "6bde87a7-f6ef-4920-8828-404345282259",
                                                                               :task/description     "",
                                                                               :task/duration        "UnknownTransitType: PT1M",
                                                                               :task/scheduled-at    "UnknownTransitType: [object Object]",
                                                                               :ui/show-form-debug?  false,
                                                                               :ui/has-duration?     false,
                                                                               :ui/has-scheduled-at? false,
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                                                     {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                                             [:task/id #uuid "6bde87a7-f6ef-4920-8828-404345282259"],
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                                             #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                                               :task/description
                                                                                                                                                               :task/scheduled-at},
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                                             {:ui/has-duration?     false,
                                                                                                                                                              :ui/has-scheduled-at? false,
                                                                                                                                                              :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                                              :task/description     "",
                                                                                                                                                              :task/scheduled-at    "UnknownTransitType: [object Object]"}}},
                                                :ui/show-form-debug?          false,
                                                :task/description             "chagnge",
                                                :task/scheduled-at            "UnknownTransitType: [object Object]",
                                                :db/updated-at                #inst "2020-08-06T20:31:24.755-00:00",
                                                :db/created-at                #inst "2020-08-06T19:47:07.686-00:00"}
                                               {:task/subtasks                [],
                                                :ui/has-duration?             false,
                                                :ui/has-scheduled-at?         false,
                                                :ui/show-delete-confirmation? false,
                                                :task/duration                "UnknownTransitType: PT1M",
                                                :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                              {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                      [:task/id #uuid "8cf7f2eb-b095-4e36-a9a4-5f424cccc48c"],
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                      #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                        :task/description
                                                                                                                                        :task/scheduled-at},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/complete?
                                                                                                                                      #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                        :task/description
                                                                                                                                        :task/scheduled-at},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                      {:ui/has-duration?     false,
                                                                                                                                       :ui/has-scheduled-at? false,
                                                                                                                                       :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                       :task/description     "Drink 1 liter of water",
                                                                                                                                       :task/scheduled-at    "UnknownTransitType: [object Object]"}},
                                                :ui/machine-state             :state/not-submitting,
                                                :task/id                      #uuid "8cf7f2eb-b095-4e36-a9a4-5f424cccc48c",
                                                :ui/subtask-form
                                                                              {:task/id              #uuid "a8bdce00-bce1-44bf-b749-602db3b03a3a",
                                                                               :task/description     "",
                                                                               :task/duration        "UnknownTransitType: PT1M",
                                                                               :task/scheduled-at    "UnknownTransitType: [object Object]",
                                                                               :ui/show-form-debug?  false,
                                                                               :ui/has-duration?     false,
                                                                               :ui/has-scheduled-at? false,
                                                                               :com.fulcrologic.fulcro.algorithms.form-state/config
                                                                                                     {:com.fulcrologic.fulcro.algorithms.form-state/id
                                                                                                                                                             [:task/id #uuid "a8bdce00-bce1-44bf-b749-602db3b03a3a"],
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/fields
                                                                                                                                                             #{:ui/has-duration? :ui/has-scheduled-at? :task/duration
                                                                                                                                                               :task/description
                                                                                                                                                               :task/scheduled-at},
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/subforms {},
                                                                                                      :com.fulcrologic.fulcro.algorithms.form-state/pristine-state
                                                                                                                                                             {:ui/has-duration?     false,
                                                                                                                                                              :ui/has-scheduled-at? false,
                                                                                                                                                              :task/duration        "UnknownTransitType: PT1M",
                                                                                                                                                              :task/description     "",
                                                                                                                                                              :task/scheduled-at    "UnknownTransitType: [object Object]"}}},
                                                :ui/show-form-debug?          false,
                                                :task/description             "Drink 1 liter of water",
                                                :task/scheduled-at            "UnknownTransitType: [object Object]",
                                                :db/updated-at                #inst "2020-08-06T19:47:07.696-00:00",
                                                :db/created-at                #inst "2020-08-06T19:47:07.696-00:00"}],
           :habit/criteria                    :min-of})
        ]
    ;(println out)
    ;(prn out)
    (pprint out)
    (is (= expected out))
    ;(is false "i guess it worked.")
    out
    ))

(comment (elide-client-only-values-test))
