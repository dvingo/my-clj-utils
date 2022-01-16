(ns dv.fulcro-components
  (:require
    [com.fulcrologic.fulcro.components :as c]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.semantic-ui.modules.transition.ui-transition :refer [ui-transition]]))

(defn notification [{:keys [ui/submit-state ui/server-message]}]
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

