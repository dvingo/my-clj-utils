(ns dv.fulcro-util
  (:refer-clojure :exclude [uuid ident?])
  (:require
    ["react" :as react]
    [cljs.core.async :refer [<! chan put! go go-loop]]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as c :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom :refer [div ul li p h3 button]]
    [com.fulcrologic.fulcro.dom.events :as e]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.networking.mock-server-remote :refer [mock-http-server]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.semantic-ui.modules.transition.ui-transition :refer [ui-transition]]
    [dv.cljs-emotion-reagent :refer [defstyled]]
    [edn-query-language.core :as eql]
    [goog.events :as events]
    [goog.object :as gobj]
    [reitit.frontend.easy :as rfe]
    [taoensso.timbre :as log])
  (:require-macros [dv.fulcro-util]))

(defn error [& msg]
  (js/Error. (apply str msg)))

;; some thoughts:
;; I'm thinking of a floating widget on every component that you opt into that you click to toggle
;; the helpers - pprint props in a floating div etc.

;; websocket network remote
;; etc

(defn parse-int [int-str] (js/parseInt int-str 10))

(s/def ::str-or-num (s/or :s string? :n number?))

(>defn to-int
  [str-num]
  [::str-or-num => (s/or :empty #{""} :n number?)]
  (if (string? str-num)
    (let [v (str/replace str-num #"[^-0-9]" "")]
      (cond-> v
        (not= v "")
        parse-int))
    str-num))
(comment (to-int "9sss")
  (to-int "-10") (to-int "-0"))

;; Form helpers str -> data type

(>defn pos-or-empty
  [i]
  [string? => ::str-or-num]
  (if (empty? i)
    i
    (Math/max 0 (to-int i))))

(>defn min-or-empty
  [min i]
  [number? string? => ::str-or-num]
  (if (empty? i)
    i
    (Math/max min (to-int i))))

(>defn clamp-or-empty
  "Ret if invalid empty str, else int clamped at min - max"
  [min max num-str]
  [number? number? string? => ::str-or-num]
  (cond
    (empty? num-str) num-str
    (or (nil? min) (nil? max)) (to-int num-str)
    :else (Math/min max (Math/max min (to-int num-str)))))

(defn listen
  "goog.events.listen to channel
   (listen js/global (.-MOUSEMOVE EventType))
   https://google.github.io/closure-library/api/goog.events.EventType.html
  "
  ([el type]
   (let [out (chan)]
     (events/listen el type
       (fn [e] (put! out e)))
     out))
  ([el type tx]
   (let [out (chan 1 tx)]
     (events/listen el type
       (fn [e] (put! out e)))
     out)))

(defn remove-id* [state table-key id]
  (update state table-key #(dissoc % id)))

(defn remove-ids*
  "Given a top level fulcro db table key and a collection of ids, dissoc's those ids from the db."
  [state table-key ids]
  (reduce #(remove-id* %1 table-key %2) state ids))

(defn nan? [v] (js/Number.isNaN v))

(defn valid-inst? [v]
  (and (inst? v)
    (not (nan? (.valueOf v)))))

;; you probably want to use logic like:
;; https://github.com/dvingo/cljs-emotion/blob/master/src/main/dv/cljs_emotion_reagent.cljc#L126
(defn react-factory [el]
  (fn
    ([] (react/createElement el))
    ([props & children]
     (if (seq children)
       (apply react/createElement el (clj->js props) (apply array children))
       (if (react/isValidElement props)
         (react/createElement el nil props)
         (react/createElement el (clj->js props)))))))

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(comment (re-matches email-regex "HI@HI.com"))

(defn ^boolean valid-email? [email]
  (boolean (re-matches email-regex email)))

(comment (valid-email? "1230HI@HI.com"))

(defn ^boolean valid-password? [password] (> (count password) 7))

(defn ui-input [props] (react/createElement "input" (clj->js props)))

(defsc Input
  [this props]
  {}
  (react/createElement "input" (clj->js props)))

(def ui-input2 (c/factory Input))

(defsc AutoFocusInput [this props]
  {:initLocalState    (fn [this props]
                        (gobj/set this "inputRef" (react/createRef)))
   :componentDidMount (fn [this]
                        (when (:autofocus? (c/props this))
                          ;; Needed when using inside a portal otherwise the input will not focus
                          (js/setTimeout
                            #(-> this
                               (gobj/getValueByKeys "inputRef" "current")
                               (.focus)))))}
  (ui-input
    (merge
      (dissoc props :autofocus?)
      {:ref (gobj/get this "inputRef")})))

(def ui-auto-focus-input (c/factory AutoFocusInput))

(defn field
  [{:keys [label checked? valid? error-message inline-err?]
    :or   {inline-err? false}
    :as   props}]
  (let [input-props   (-> props
                        (assoc :name label)
                        (dissoc :checked? :label :valid? :error-message :inline-err?))
        show-err-msg? (and checked? (not valid?))]
    (div :.ui.field
      (dom/label {:htmlFor label} label)

      (cond->> (ui-auto-focus-input input-props)
        inline-err? (div :.ui.input.field {:classes [(when show-err-msg? "error")]}))

      (when inline-err?
        (dom/div :.ui.upward.pointing.red.basic.label
          {:style {:top "-19px"} :classes [(when (not show-err-msg?) "hidden")]} error-message))

      (when-not inline-err?
        (dom/div :.ui.error.message {:classes [(when valid? "hidden")]}
          error-message)))))

(defn mark-fields-complete*
  "
  - state map
  - ident of component
  - seq of fields to mark complete"
  [s ref fs]
  (reduce (fn [acc v] (fs/mark-complete* acc ref v)) s fs))

(defn reset-form* [s ident]
  (-> s
    (fs/pristine->entity* ident)
    (fs/clear-complete* ident)))

(defn reset-form! [this]
  (c/transact! this [(fs/reset-form!) (fs/clear-complete!)]))

(defn reset-form!! [this]
  (c/transact!! this [(fs/reset-form!) (fs/clear-complete!)]))

(defn mark-complete!
  [this field]
  (c/transact!! this [(fs/mark-complete! {:field field})]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server error and messages tracking mutation helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def submit-states #{:not-submitting :submitting :success :failed})

(defmutation set-submit
  [{:keys [val msg]}]
  (action [{:keys [state ref]}]
    (log/info "Setting submit state to: " val)
    (log/info "ref path: " (conj ref :ui/submit-state))
    (swap! state
      (fn [s]
        (-> s
          (assoc-in (conj ref :ui/submit-state) val)
          (assoc-in (conj ref :server/message) msg))))))

(s/def ::opt-map (s/* (s/cat :k keyword? :v any?)))

(>defn ui-textfield
  "
  (fu/ui-textfield this
  \"Description\"
        :habit/description
        props
        :tabIndex 1
        :error-message \"Give the habit a description.\")
  "
  [this label field-kw props & {:as opts}]
  [some? string? keyword? map? ::opt-map => some?]
  (let [value     (field-kw props)
        checked?  (fs/checked? props field-kw)
        opts      (or opts {})
        on-change (or (:onChange opts) identity)
        opts      (dissoc opts :onChange)
        cls       (c/react-type this)
        props     (merge
                    {:label         label
                     :inline-err?   true
                     :type          "text"
                     :value         (or (str value) "")
                     :checked?      checked?
                     :valid?        (not (empty? (str value)))
                     :error-message "Please enter a value"
                     :onBlur        (fn [e]
                                      (let [form-fields (or (fs/get-form-fields cls) #{})
                                            v           (str/trim (e/target-value e))]
                                        (m/set-string!! this field-kw :value v)
                                        (when (form-fields field-kw)
                                          (mark-complete! this field-kw))))
                     :autoComplete  "off"
                     :onChange      #(do
                                       (on-change %)
                                       (m/set-string!! this field-kw :event %))}
                    opts)]
    (field props)))

;; Defsc so you get shouldComponentUpdate perf gains when props don't change.

(defsc TextField
  [this
   {:keys [label value field-checked? error-message]
    :or   {label          ""
           value          ""
           field-checked? false
           error-message  "Please enter a value"}}
   {:keys [on-change on-blur valid? on-key-down]
    :or   {on-change   identity
           on-key-down identity}
    :as   computed-props}]
  (let [opts (dissoc computed-props :on-key-down :on-change :on-blur :valid?)]
    (field
      (merge
        {:label         label
         :inline-err?   true
         :type          "text"
         :value         (or (str value) "")
         :checked?      field-checked?
         :valid?        (not (empty? (str value)))
         :error-message error-message
         :onBlur        on-blur
         :autoComplete  "off"
         :onKeyDown     on-key-down
         :onChange      on-change}
        opts))))

(def ui-textfield2 (c/computed-factory TextField))

(defn ui-number-field
  [this label kw props & {:keys [min max] :as opts}]
  (ui-input
    (merge
      {:type     "number"
       :label    label
       :value    (str (kw props))
       :onBlur   (fn [] (mark-complete! this kw))
       :onChange #(m/set-value!! this kw (clamp-or-empty min max (e/target-value %)))}
      (or opts {}))))

;; TODO use stateful design to track a timeout for a text field that will mark-complete the field
;; after 2 seconds or so. Need to clear this after you complete the field (create a new entity for example)
;; you don't want the now-fresh form to be marked complete triggering validation errors.

(defn make-txt-field []
  (let [timer (volatile! nil)]
    (fn [this label field-kw val]
      (field [this label field-kw val]))))

(>defn ui-password2
  [this field-kw val & {:as opts}]
  [some? keyword? string? ::opt-map => some?]
  (field (merge {:label         "Password"
                 :type          "password"
                 :value         (or val "")
                 :valid?        (valid-password? val)
                 :error-message "Password must be at least 8 characters."
                 :onBlur        #(mark-complete! this field-kw)
                 :autoComplete  "off"
                 :onChange      #(m/set-string!! this field-kw :event %)}
           (or opts {}))))

(defn ui-password-field [value on-change & {:as opts}]
  (field (merge {:label        "Password"
                 :type         "password"
                 :value        value
                 :autoComplete "off"
                 :onChange     on-change}
           (or opts {}))))

(defn ui-verify-password [this f pw pw-ver on-blur & {:as opts}]
  (field
    (merge {:label         "Repeat Password"
            :type          "password"
            :value         (or pw-ver "")
            :autoComplete  "off"
            :valid?        (= pw pw-ver)
            :error-message "Passwords do not match."
            :onBlur        #(on-blur f)
            :onChange      (fn [e]
                             (when (= pw (e/target-value e))
                               (mark-complete! this f))
                             (m/set-string!! this f :event e))}
      (or opts {}))))

(defn ui-email [this f val on-blur & {:as opts}]
  (field
    (merge
      {:label         "Email"
       :type          "email"
       :value         (or val "")
       :valid?        (valid-email? val)
       :error-message "Must be an email address"
       :autoComplete  "off"
       :onBlur        #(on-blur f)
       :onChange      #(m/set-string!! this f :event %)}
      (or opts {}))))

(defn ui-primary-btn [text on-click]
  (dom/button :.ui.primary.button {:onClick on-click} text))

(defn ui-button
  ([props on-click text]
   (dom/button :.ui.button
     (merge
       props
       {:onClick on-click}) text))
  ([on-click text]
   (dom/button :.ui.button #js {:onClick on-click} text)))

(defn stop-propagation
  ([] (fn [e] (.stopPropagation e)))
  ([f] (fn [e] (.stopPropagation e) (f e))))

(defn prevent-default
  ([] (fn [e] (.preventDefault e)))
  ([f] (fn [e] (.preventDefault e) (f e))))

(defn prevent-default-and-stop
  ([] (fn [e] (doto e .preventDefault .stopPropagation)))
  ([f] (fn [e] (doto e .preventDefault .stopPropagation) (f e))))


(defn on-server? []
  (not (exists? js/window)))

(defn map->vec
  "Useful for passing a map to a fn that takes key val &args"
  [m]
  (vec (mapcat identity m)))

(comment
  (map->vec {:a 5 :b 6})
  (map->vec nil))

(defn notification [{:keys [ui/submit-state ui/server-message] :as props}]
  (let [[success? failed?] (map #{submit-state} [:state/success :state/failed])
        submit-done? (boolean (or success? failed?))]
    (c/fragment
      (ui-transition {:visible submit-done? :animation "scale" :duration 500}
        (dom/div :.ui.floating.message {:classes [(when success? "positive")
                                                  (when failed? "negative")]}
          (dom/div :.content
            (case submit-state :state/failed server-message
                               :state/success "Successfuly saved"
                               "")))))))
(defn get-props-keys [c]
  (->> (c/get-query c)
    (eql/query->ast)
    :children
    (map :key)))

(defn table [header-cells rows]
  (dom/table :.ui.celled.table.striped.yellow {:key "table"}
    (dom/thead
      (dom/tr nil (map #(dom/th #js{:key (str %)} %) header-cells)))
    (dom/tbody
      (map (fn [[k v]]
             (dom/tr #js{:key (str k)}
               (dom/td #js{:key "key"} (str k))
               (dom/td #js{:key "val"} (pr-str v))))
        rows))))

(defn props-data-debug
  "show table of fields for a component with value ."
  [com show?]
  (when show?
    (let [props (c/props com)]
      (when (some? props)
        (let [ident     (c/get-ident com props)
              prop-keys (get-props-keys com)
              rows      (map #(vector % (get props %)) prop-keys)]
          (dom/div nil
            (dom/h2 {:key "first"} "ident: " (pr-str ident))
            (table ["prop" "value"] rows)))))))

(defn form-debug
  "show table of form fields for a component with value and validation state.
  validator component instance"
  [validator com show?]
  (let [props      (c/props com)
        {:keys [form-fields]} (c/component-options com)
        form-ident (c/get-ident com)]
    (when show?
      (c/fragment
        (dom/h4 :.ui.violet.message (str "Form fields for " (pr-str form-ident) ":"))
        (dom/table :.ui.celled.table.striped.yellow
          (dom/thead
            (dom/tr (dom/th "field") (dom/th "value") (dom/th "valid state")))
          (dom/tbody
            (dom/tr {:key "all"} (dom/td "Entire form") (dom/td "n/a") (dom/td (pr-str (validator props))))
            (map #(dom/tr {:key (str %)}
                    (dom/td (str %))
                    (dom/td (pr-str (props %)))
                    (dom/td (pr-str (validator props %))))
              form-fields)))))))

(defn dirty-fields
  "Returns the actual map of dirty fields for the current component."
  [this]
  (let [props (c/props this)
        ident (c/ident this props)]
    (get
      (fs/dirty-fields props false)
      ident)))

(defmutation entity->pristine!* [_]
  (action [{:keys [ref state]}]
    (swap! state #(fs/entity->pristine* % ref))))

(defn entity->pristine! [this]
  (c/transact! this [(entity->pristine!*)]))

(defmutation pristine->entity!* [_]
  (action [{:keys [ref state]}]
    (swap! state #(fs/pristine->entity* % ref))))

(defn pristine->entity! [this]
  (c/transact! this [(pristine->entity!*)]))

(defn field-valid? [validator props field]
  (let [v (validator props field)]
    (contains? #{:unchecked :valid} v)))

(defn validator-state [this validator]
  (assert (c/component-instance? this))
  (let [cls               (c/react-type this)
        props             (c/props this)
        fields            (fs/get-form-fields cls)
        valid-state       (validator props)
        dirty?            (fs/dirty? props)
        all-fields-valid? (every? #(= :valid (validator props %)) fields)
        disabled?         (or (= :invalid valid-state)
                            (and (= :unchecked valid-state) (not all-fields-valid?)))]
    {:checked?  (fs/checked? props)
     :dirty?    dirty?
     :valid?    (= :valid valid-state)
     :disabled? disabled?}))

(defn get-server-mutation-err
  [result-or-env]
  (let [result       (or (some-> result-or-env ::sm/event-data ::sm/mutation-result) result-or-env)
        body         (:body result)
        mutation-sym (-> body keys first)]
    (let [error (-> body mutation-sym :server/message)]
      (if (nil? error)
        "There was an error sending your request."
        error))))

(defn get-server-mutation-err2
  [result-or-env]
  (let [result       (or (some-> result-or-env ::sm/event-data ::sm/mutation-result) result-or-env)
        body         (:body result)
        mutation-sym (-> body keys first)]
    (-> body mutation-sym :server/message)))

(defn map-table
  "show table of form fields for a component with value and validation state.
  validator component instance"
  ([m show?] (map-table "Map of data" m show?))
  ([label m show?]
   (when show?
     (c/fragment
       (dom/h4 :.ui.violet.message label)
       (dom/table :.ui.celled.table.striped.yellow
         (dom/thead
           (dom/tr (dom/th "field") (dom/th "value")))
         (dom/tbody
           (map (fn [[k v]]
                  (dom/tr {:key (str k)}
                    (dom/td (str k))
                    (dom/td (pr-str v))))
             m)))))))

(defstyled hover-hand :div {":hover" {:cursor "pointer"}})

(>defn uuid
  "Without args gives random UUID.
   With args, builds UUID based on input (useful in tests)."
  ([] [=> uuid?] (random-uuid))
  ([s] [string? => uuid?] (cljs.core/uuid s)))

(defn deep-merge [x y]
  (cond
    (and (map? x) (map? y)) (merge-with deep-merge x y)
    (and (map? x) (nil? y)) x
    (and (map? y) (nil? x)) y
    :else y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pathom remote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn local-remote
  [parser env]
  (let [transmit!
        (:transmit! (mock-http-server
                      {:parser
                       (fn [eql]
                         (go
                           (let [out (<! (parser env eql))]
                             (log/trace "Parser output: " out)
                             out)))}))]
    {:transmit! (fn [this send-node] (transmit! this send-node))}))

(defn has-field?
  "Returns true if a value is not nil and not :com.fulcrologic.fulcro.algorithms.merge/not-found"
  [v]
  (and
    (some? v)
    (not= ::merge/not-found v)))

(defn registry-key->class
  [kw-or-sym]
  (if-let [cls (c/registry-key->class kw-or-sym)]
    cls
    (throw (js/Error. (str "Class component: " kw-or-sym
                        " is not in the registry, you probably need to require the namespace it lives in.")))))

(defn kw-namespace [k] (and (keyword? k) (namespace k)))

(defn client-only-value? [k]
  ;(println "checking key: " k)
  (let [ns       (some-> k kw-namespace)
        ident-ns (when (eql/ident? k) (some-> (first k) kw-namespace))
        v        (or
                   (and
                     (string? ns)
                     (or
                       (= "ui" ns)
                       (str/starts-with? ns "fulcro.")
                       (str/starts-with? ns "com.fulcrologic.fulcro.")))
                   (and
                     (string? ident-ns)
                     (or
                       (= "ui" ident-ns)
                       (str/starts-with? ns "fulcro.")
                       (str/starts-with? ident-ns "com.fulcrologic.fulcro."))))
        ]
    ;(println "ret: " v)
    v))

;(defn elide-client-only-values
;  "Takes a map and recursively removes and keys that should not be sent over the network."
;  [m]
;  (reduce
;    (fn [acc [k v]]
;      (if-not (client-only-value? k)
;        (cond
;          (map? v) (assoc acc k (elide-client-only-values v))
;          (vector? v) (assoc acc k (mapv elide-client-only-values v))
;          :else (assoc acc k v))
;        acc))
;    {} m))


(defn elide-client-only-values*
  "Takes a map, removes keys that should not be sent over the network."
  [m]
  (reduce
    (fn [acc [k v]]
      (if (client-only-value? k) acc (assoc acc k v)))
    {} m))

(defn elide-client-only-values
  [m]
  (cond
    (vector? m)
    (mapv elide-client-only-values m)

    (map? m)
    (walk/postwalk
      (fn [x] (if (map? x) (elide-client-only-values* x) x))
      m)
    :else m))

;; testing elide-values

(comment
  (elide-client-only-values
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
  )
