(ns configurator.frontend.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [cljs-http.client :as http]
   [reagent.dom :as rdom]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   ))
(comment
  [com.wsscode.pathom3.cache :as p.cache]
  [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
  [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
  [com.wsscode.pathom3.connect.foreign :as pcf]
  [com.wsscode.pathom3.connect.indexes :as pci]
  [com.wsscode.pathom3.connect.operation :as pco]
  [com.wsscode.pathom3.connect.operation.transit :as pcot]
  [com.wsscode.pathom3.connect.planner :as pcp]
  [com.wsscode.pathom3.connect.runner :as pcr]
  [com.wsscode.pathom3.error :as p.error]
  [com.wsscode.pathom3.format.eql :as pf.eql]
  [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
  [com.wsscode.pathom3.interface.eql :as p.eql]
  [com.wsscode.pathom3.interface.smart-map :as psm]
  [com.wsscode.pathom3.path :as p.path]
  [com.wsscode.pathom3.plugin :as p.plugin])


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


(pco/defresolver product-resolver [{:keys [id products]}]
  {::pco/output (->
                  @db-cache
                  :products
                  first
                  keys
                  vec)}
  (->>
   products
   (filter (fn [p] (== id (:id p))))
   first
   ))

;; TODO formalize this?
(def product-keys
  (->
   @db-cache
   :products
   first
   keys
   vec))

(pco/defresolver slug-resolver [{:keys [slug products]}]
  {::pco/output product-keys}
  (->>
   products
   (filter (fn [p] (== slug (:slug p))))
   first
   ))

(product-resolver {:id 1
                   :products (:products @db-cache)})

(def env
  (pci/register
   [(pbir/constantly-resolver :products
                          (:products @db-cache) ;; TODO: load in via API
                          )
    product-resolver
    slug-resolver
    ]))


(comment
  (p.eql/process env [{:products [:slug]}])
  (p.eql/process env [{:products [:id]}])
  (p.eql/process env [{:products [:title]}])
  (p.eql/process env [{[:id 1] [:slug]}])
  (p.eql/process env [{[:id 7] [:title]}])
  (p.eql/process env [{[:id 7] product-keys}])
  (p.eql/process env [{[:slug "grandpillow"] [:id]}])
  )


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
