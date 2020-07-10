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

(defn conj-vec [entity fkw val]
  (update entity fkw #(conj (or % []) val)))

(defn conj-set [entity fkw val]
  (update entity fkw #(conj (or (set %) #{}) val)))

(s/def ::str-or-num (s/or :s string? :n number?))

(defn parse-int [int-str] (js/parseInt int-str 10))

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
  (log/info "clamp: called: " num-str)
  (cond
    (empty? num-str) num-str
    (or (nil? min) (nil? max)) (to-int num-str)
    :else (Math/min max (Math/max min (to-int num-str)))))

(defn listen
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

(defn remove-ids* [state table-key ids]
  (reduce #(remove-id* %1 table-key %2) state ids))

(defn nan? [v] (js/Number.isNaN v))

(defn valid-inst? [v]
  (and (inst? v)
    (not (nan? (.valueOf v)))))

(defn react-factory [el]
  (fn
    ([] (react/createElement el #js{}))
    ([props & children]
     ;(log/info "in react factory props:" props )
     (if (seq children)
       (apply react/createElement el (clj->js props) (apply array children))
       (react/createElement el (clj->js props))))))

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(comment (re-matches email-regex "HI@HI.com"))

(defn ^boolean valid-email? [email]
  (boolean (re-matches email-regex email)))

(comment (valid-email? "1230HI@HI.com"))

(defn ^boolean valid-password? [password] (> (count password) 7))

(defn ui-input [props] (react/createElement "input" (clj->js props)))

(defsc AutoFocusInput [this props]
  {:initLocalState    (fn [this props]
                        (gobj/set this "inputRef" (react/createRef)))
   :componentDidMount (fn [this]
                        (when (:autofocus? (c/props this))
                          (-> this
                            (gobj/getValueByKeys "inputRef" "current")
                            (.focus))))}
  (ui-input
    (merge
      (dissoc props :autofocus?)
      {:ref (gobj/get this "inputRef")})))

(def ui-auto-focus-input (c/factory AutoFocusInput))

(defn field [{:keys [label checked? valid? error-message inline-err?]
              :or   {inline-err? false}
              :as   props}]
  (let [input-props   (-> props
                        (assoc :name label)
                        (dissoc :checked? :label :valid? :error-message :inline-err?))
        show-err-msg? (and checked? (not valid?))]
    (div :.ui.field
      (dom/label {:htmlFor label} label)

      (if inline-err?
        (div :.ui.input.field {:classes [(when show-err-msg? "error")]}
          (ui-auto-focus-input input-props))
        (ui-auto-focus-input input-props))

      (when inline-err?
        (dom/div :.ui.upward.pointing.red.basic.label
          {:style {:top "-19px"} :classes [(when (not show-err-msg?) "hidden")]} error-message))

      (when-not inline-err? (dom/div :.ui.error.message {:classes [(when valid? "hidden")]}
                              error-message)))))

(defn mark-fields-complete* [s ref fs]
  (reduce (fn [acc v] (fs/mark-complete* acc ref v)) s fs))

(defn reset-form* [s ident]
  (-> s
    (fs/pristine->entity* ident)
    (fs/clear-complete* ident)))

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

(defn on-server? []
  (not (exists? js/window)))


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

(defn field-valid? [validator props field]
  (let [v (validator props field)]
    (contains? #{:unchecked :valid} v)))

(defn validator-state [validator props]
  (let [valid-state (validator props)]
    {:checked?  (fs/checked? props)
     :disabled? (#{:invalid :unchecked} valid-state)}))

(defn get-server-mutation-err
  [result-or-env]
  (let [result       (or (some-> result-or-env ::sm/event-data ::sm/mutation-result) result-or-env)
        body         (:body result)
        mutation-sym (-> body keys first)]
    (let [error (-> body mutation-sym :server/message)]
      (if (nil? error)
        "There was an error sending your request."
        error))))

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

(defn id? [id]
  (or (keyword? id) (uuid? id)))

;; [prop val]
(s/def ::ident (s/tuple qualified-keyword? id?))
(s/def ::coll-of-idents (s/coll-of ::ident :kind vector?))
(defn coll-of-idents? [v] (s/valid? ::coll-of-idents v))

(defn ref?
  ([v] (s/valid? ::ident v))
  ([kw-id v]
   (and (s/valid? ::ident v)
     (= (first v) kw-id))))

(defn ident? [v] (s/valid? ::ident v))

(s/def ::prop-path (s/tuple qualified-keyword? id? qualified-keyword?))

(defn prop-path? [v]
  (s/valid? ::prop-path v))

(defn ref->id
  "ident [:prop id] => id"
  [v] (cond-> v (s/valid? ::ident v) second))

(>defn ->ident
  "Given a kw that is the id prop, and a map or id, return ident."
  ([kw]
   [keyword? => fn?]
   (fn [m] (->ident kw m)))
  ([kw v]
   [keyword? (s/or :id id? :m map?) => ::ident]
   [kw (if (map? v) (kw v) v)]))

(defn ident->id
  "ident [:prop id] => id"
  [v] (cond-> v (s/valid? ::ident v) second))

(defn server-error [msg]
  {:server/message msg
   :server/error?  true})

(>defn uuid
  "Without args gives random UUID.
   With args, builds UUID based on input (useful in tests)."
  ([] [=> uuid?] (random-uuid))
  ([s] [any? => any?] (cljs.core/uuid s)))

(defn deep-merge [x y]
  (cond
    (and (map? x) (map? y)) (merge-with deep-merge x y)
    (and (map? x) (nil? y)) (merge x y)
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
