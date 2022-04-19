(ns configurator.frontend.app
  (:require
   [re-frame.core :as re-frame]
   [reagent.dom :as rdom]
   [datascript.core :as d]
   ))

(defonce schema {:products  {:db/cardinality :db.cardinality/many}
                 })

(defonce conn (d/create-conn schema))

(d/transact! conn [
                   {:db/id -1
                    :name "LP JR"
                    :price "$999"
                    }
                   {:db/id -1
                    :name "Jupiter"
                    :price "$1299"
                    }
                   ])

(defn all-names []
  (d/q '[:find ?name
         :where
         [?c :name ?name]]
       @conn))



(defn main-view []
   [:div
    [:h2 "all products"
     [:ul
      (map (fn [x] [:li x]) (map first (all-names)))
      ]]])


(defn ^:dev/after-load mount-root []
  ;; (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main-view] root-el)))

(defn init []
  (println "Hello World")
  (mount-root)
  )
