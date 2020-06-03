;; state machine
;; debug helpers

;; I'm thinking of a floating widget on every component that you opt into that you click to toggle
;; the helpers - pprint props in a floating div etc.

;; client-side pathom
;; client-side pathom indexeddb
;; websocket network remote
;; etc
(ns dv.fulcro-util
  (:refer-clojure :exclude [uuid])
  (:require
    ["react" :as react]
    [cljs.core.async :refer [put! chan]]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.form-state :as fs]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom :refer [div ul li p h3 button]]
    [com.fulcrologic.fulcro.dom.events :as e]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.ui-state-machines :as sm :refer [defstatemachine]]
    [com.fulcrologic.guardrails.core :refer [>defn => ?]]
    [com.fulcrologic.semantic-ui.modules.transition.ui-transition :refer [ui-transition]]
    [dv.cljs-emotion :refer [defstyled]]
    [edn-query-language.core :as eql]
    [goog.events :as events]
    [goog.object :as gobj]
    [reitit.frontend.easy :as rfe]
    [space.matterandvoid.client.router :as r]
    [taoensso.timbre :as log]
    [tick.alpha.api :as t])
  (:require-macros [dv.fulcro-util]))

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

(defn str->tick-days-period
  [s]
  (let [i (pos-or-empty s)
        i (cond (string? i) ""
                (zero? i) 1
                :elsee i)]
    (cond-> i (number? i)
      (t/new-period :days))))

(>defn uuid
  ([] [=> uuid?] (random-uuid))
  ([s] [any? => any?] (cljs.core/uuid s)))

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

(defn id? [id]
  (or (keyword? id) (uuid? id)))

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
                        (when (:autofocus? (comp/props this))
                          (-> this
                            (gobj/getValueByKeys "inputRef" "current")
                            (.focus))))}
  (ui-input
    (merge
      (dissoc props :autofocus?)
      {:ref (gobj/get this "inputRef")})))

(def ui-auto-focus-input (comp/factory AutoFocusInput))

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
  (comp/transact!! this [(fs/mark-complete! {:field field})]))

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
  (let [value    (field-kw props)
        checked? (fs/checked? props field-kw)
        props    (merge
                   {:label         label
                    :inline-err?   true
                    :type          "text"
                    :value         (or (str value) "")
                    :checked?      checked?
                    :valid?        (not (empty? (str value)))
                    :error-message "Please enter a value"
                    :onBlur        (fn [e]
                                     (let [v (str/trim (e/target-value e))]
                                       (m/set-string!! this field-kw :value v)
                                       (mark-complete! this field-kw)))
                    :autoComplete  "off"
                    :onChange      #(m/set-string!! this field-kw :event %)}
                   (or opts {}))]
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

(defn link
  ([target current-tab]
   (link target current-tab {}))
  ([target current-tab opts]
   (dom/a :.item
     (merge
       {:classes [(when (= target current-tab) "active")]
        :key     (str target)
        :href    (if (on-server?)
                   (name target)
                   (r/route-href target))}
       opts)
     (str/capitalize (name target)))))

(defn notification [{:keys [ui/submit-state ui/server-message] :as props}]
  (let [[success? failed?] (map #{submit-state} [:state/success :state/failed])
        submit-done? (boolean (or success? failed?))]
    (comp/fragment
      (ui-transition {:visible submit-done? :animation "scale" :duration 500}
        (dom/div :.ui.floating.message {:classes [(when success? "positive")
                                                  (when failed? "negative")]}
          (dom/div :.content
            (case submit-state :state/failed server-message
                               :state/success "Successfuly saved"
                               "")))))))
(defn get-props-keys [c]
  (->> (comp/get-query c)
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
    (let [props (comp/props com)]
      (when (some? props)
        (let [ident     (comp/get-ident com props)
              prop-keys (get-props-keys com)
              rows      (map #(vector % (get props %)) prop-keys)]
          (dom/div nil
            (dom/h2 {:key "first"} "ident: " (pr-str ident))
            (table ["prop" "value"] rows)))))))

(defn form-debug
  "show table of form fields for a component with value and validation state.
  validator component instance"
  [validator com show?]
  (let [props      (comp/props com)
        {:keys [form-fields]} (comp/component-options com)
        form-ident (comp/get-ident com)]
    (when show?
      (comp/fragment
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
  [m show?]
  (when show?
    (comp/fragment
      (dom/h4 :.ui.violet.message "Map of data")
      (dom/table :.ui.celled.table.striped.yellow
        (dom/thead
          (dom/tr (dom/th "field") (dom/th "value")))
        (dom/tbody
          (map (fn [[k v]]
                 (dom/tr {:key (str k)}
                   (dom/td (str k))
                   (dom/td (pr-str v))))
            m))))))

(defstyled ^{:styled/classname :full} hover-hand :div {":hover" {:cursor "pointer"}})