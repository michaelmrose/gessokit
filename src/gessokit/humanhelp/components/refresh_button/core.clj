(ns gessokit.humanhelp.components.refresh-button.core
  (:require
   [gesso.core :as g]
   [gessokit.humanhelp.components.refresh-button.attr :as attr]))

(defn refresh-button
  [{:keys [stale?]}]
  (g/button
   {:variant (if stale? :primary :outline)
    :text "Refresh"
    :attrs (attr/button-attrs stale?)}))
