(ns configurator.frontend.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [cljs-http.client :as http]
   [reagent.dom :as rdom]
   [promesa.core :as p]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
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

(defn json-get [url]
  (p/let [resp (js/fetch url)
          json (.json resp)]
    (js->clj json :keywordize-keys true)))


(defn ns-key [nms key]
  (keyword (str nms "/" (name key))))
(= (ns-key "product" :id) :product/id)

(defn ns-key-vec [nms v]
  (vec (map (partial ns-key nms) v)))

(defn ns-keys-in-map [nms map]
  (let [oldkeys (vec (keys map))
        newkeys (ns-key-vec nms oldkeys)]
    (clojure.set/rename-keys map (zipmap oldkeys newkeys))))

;; consider using spec to verify
(def orig-product-keys
   [:children
    :slug
    :parent
    :bundles
    :_cache_built
    :category_names
    :title
    :categories
    :id
    :url
    :product_class
    :date_created
    :_cache_expires
    :skus
    :_cache_now
    :date_updated])
(def output-product-keys
  [:product/children
    :product/slug
    :product/parent
    :product/bundles
    :product/category_names
    :product/title
    :product/categories
    :product/id
    :product/url
    :product/product_class
    :product/date_created
    :product/skus
    :product/date_updated])
(def product-kmap
     {:children :product/children,
      :slug :product/slug,
      :parent :product/parent,
      :bundles :product/bundles,
      :category_names :product/category_names,
      :title :product/title,
      :categories :product/categories,
      :id :product/id,
      :url :product/url,
      :product_class :product/product_class,
      :date_created :product/date_created,
      :skus :product/skus,
      :date_updated :product/date_updated})






(pco/defresolver all-products-resolver []
  {::pco/output [{:products [output-product-keys]}]}
  (p/->>
   (json-get "https://dev.tempurpedic.com/api/products/")
   (map #(clojure.set/rename-keys % product-kmap))
   vec
   (hash-map :products)))


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


(def env
  (pci/register
   [all-products-resolver
    product-resolver
    slug-resolver]
   ))

(defn pres [p]
  (p/let [res p]
    (println res)))


(comment
  (pres (p.a.eql/process env [:products]))
  (pres (p.a.eql/process env [{:products [:product/slug]}]))
  (pres (p.a.eql/process env [{:products [:product/id]}]))
  (pres (p.a.eql/process env [{:products [:product/title]}]))
  (pres (p.a.eql/process env [{[:product/id 1] [:product/slug]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/title]}]))
  (pres (p.a.eql/process env [{[:product/id 7] output-product-keys}]))
  (pres (p.a.eql/process env [{[:product/slug "grandpillow"] [:product/id]}]))
  )

(pres (p.a.eql/process env [{[:product/slug "grandpillow"] [:product/id]}]))


(defn main-view []
  [:div
   [:h1 "Hello World"]
   [:ul (map #(vec [:li (:product/title %)])
             (:products (p.eql/process env [{:products [:product/title]}])))
    ]
   ]
  )


(defn ^:dev/after-load mount-root []
  ;; (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main-view] root-el)))

(defn init []
  (mount-root))
