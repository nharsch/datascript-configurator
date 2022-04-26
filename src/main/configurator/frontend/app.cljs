(ns configurator.frontend.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [cljs-http.client :as http]
   [reagent.dom :as rdom]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   ))

(defonce db-cache (atom {}))


(comment

 (swap! db-cache assoc :products [0 1])
 (reset! db-cache {})
 (:products @db-cache)
 @db-cache

 )

;; TODO:
;; - [] what is static-table-resolver
;; - [] resolve query by hitting API endpoint
;; - [] get child ids from children URLs
;; - [] get child attrs from child queries


;; for now let's just load all products in
(defn get-products []
  (go
    (let [response (<! (http/get
                        "https://dev.tempurpedic.com/api/products/"
                        {:with-credentials? false}
                        ))]
      (if (= (:status response) 200)
        (:body response)))))

(go
  (let [products (<! (get-products))]
    (swap! db-cache assoc :products products)))
 

;; pathom query/resolvers
(pco/defresolver all-products []
  {::pco/output [{:products
                 (vec (keys (first (:products @db-cache))))}]}  ;; TODO: define these fields?
  {:products
   (:products @db-cache)} ;; TODO: load in via API
  )



(pco/defresolver product-resolver [{:keys [id]}]
  {::pco/output (vec (keys (first (:products @db-cache))))}
  (->>
   (all-products)
   :products
   (filter (fn [p] (== id (:id p))))
   first
   ;; (select-keys [:id])
   ))

;; (def opt-env
;;   (pci/register
;;     [(pbir/static-table-resolver :user/id
;;        {1 {:user/email "user@example.com"}
;;         2 {:user/email "another@example.com"
;;            :user/name  "Sam"}})
;;      (pbir/constantly-resolver :all-users
;;        [{:user/id 1}
;;         {:user/id 2}])
;;      user-display-name]))

(comment

  (product-resolver {:id 7})
  (p.eql/process _
                 {:id 1}
                 [:slug])


  @db-cache
  )

(def resolvers [
                all-products
                ])
;; (def parser
;;   (p/parser {::p/env    {::p/reader     [p/map-reader
;;                                          pc/reader2
;;                                          pc/ident-reader
;;                                          pc/index-reader]
;;                          ::p/placeholder-prefixes #{">"}
;;                          }
;;              ::p/mutate pc/mutate
;;              ::p/plugins [(pc/connect-plugin {::pc/register resolvers})
;;                           p/error-handler-plugin
;;                           (p/post-process-parser-plugin p/elide-not-found)
;;                           ]}))



(defn main-view []
  [:div
   [:h1 "Hello World"]
   ;; [:ul (map (fn [p] [:li (:title p)]) (:products @db-cache))]
   ]
  )


(defn ^:dev/after-load mount-root []
  ;; (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main-view] root-el)))

(defn init []
  (mount-root)
  )
