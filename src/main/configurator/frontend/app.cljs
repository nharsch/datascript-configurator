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
;; - [] convert default keywords to product/kw
;; - [] resolve query by hitting API endpoint
;; - [] get child ids from children URLs
;;  - [] look at async-resolver
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
(:products @db-cache)

;; pathom query/resolvers

(defn ns-key [nms key]
  (keyword (str nms "/" (name key))))
(= (ns-key "product" :id) :product/id)

(defn ns-key-vec [nms v]
  (vec (map (partial ns-key nms) v)))
;; (ns-key-vec "product" [:id :test])

(def orig-product-keys
  (->> @db-cache
       :products
       first
       keys
       vec))
(def output-product-keys (vec (map (partial ns-key "product") orig-product-keys)))
(def kmap (zipmap orig-product-keys output-product-keys))


(defn ns-keys-in-map [nms map]
  (let [oldkeys (vec (keys map))
        newkeys (ns-key-vec nms oldkeys)]
    (clojure.set/rename-keys map (zipmap oldkeys newkeys))))
(ns-keys-in-map "product" (first (:products @db-cache)))

;; (clojure.set/rename-keys (first (:products @db-cache)) kmap)


(pco/defresolver product-resolver [{:keys [product/id products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== id (:product/id p))))
   first
   ))

(product-resolver {:product/id 1
                   :products [{:product/id 1}]})

(pco/defresolver slug-resolver [{:keys [product/slug products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== slug (:product/slug p))))
   first
   ))


(map #((clojure.set/rename-keys % kmap)) (:products @db-cache))

(def env
  (pci/register
   ; TODO lookup API directly
   [(pbir/constantly-resolver :products
                              (->> @db-cache
                                   :products
                                   (map (partial ns-keys-in-map "product"))
                                   vec))
    product-resolver
    slug-resolver]
   ))


(comment
  (p.eql/process env [{:products [:product/slug]}])
  (p.eql/process env [{:products [:product/id]}])
  (p.eql/process env [{:products [:product/title]}])
  (p.eql/process env [{[:product/id 1] [:product/slug]}])
  (p.eql/process env [{[:product/id 7] [:product/title]}])
  (p.eql/process env [{[:product/id 7] output-product-keys}])
  (p.eql/process env [{[:product/slug "grandpillow"] [:product/id]}])
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
  (mount-root))
