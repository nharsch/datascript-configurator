(ns configurator.frontend.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [cljs-http.client :as http]
   [promesa.core :as p]
   [com.fulcrologic.fulcro.algorithms.tx-processing :as txn]
   [com.fulcrologic.fulcro.application :as app]
   [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
   [com.fulcrologic.fulcro.dom :as dom]
   [com.fulcrologic.fulcro.data-fetch :as df]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
   [edn-query-language.core :as eql]
   ))


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
(comment
  (= (ns-key "product" :id) :product/id))

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

(def category-keys
  ;; TODO
  {})






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


(pco/defresolver slug-resolver [{:keys [product/slug products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== slug (:product/slug p))))
   first
   ))

;; TODO: could be better
(defn extract-id-from-url [urlstring]
  (let [url (new js/URL urlstring)]
    (-> url
         (.-pathname)
         (.split "/")
         reverse
         second)))
(extract-id-from-url "https://dev.tempurpedic.com/api/categories/1/")

(pco/defresolver prod->cats [{:keys [product/id product/categories]}]
  {::pco/output [{:product/categories  [:category/id]}]}
  (println "prod->cats")
  (vec (map extract-id-from-url categories)))


(def env
  (pci/register
   [all-products-resolver
    product-resolver
    slug-resolver
    prod->cats
    ]
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
  (pres (p.a.eql/process env [{[:product/id 1] [:product/categories]}]))
  )

(def pathom (p.a.eql/boundary-interface env))



(defn pathom-remote [request]
  {:transmit! (fn transmit! [_ {::txn/keys [ast result-handler]}]
                (let [ok-handler    (fn [result]
                                      (try
                                        (result-handler (assoc result :status-code 200))
                                        (catch :default e
                                          (js/console.error e "Result handler for remote failed with an exception."))))
                      error-handler (fn [error-result]
                                      (try
                                        (result-handler (assoc error-result :status-code 500))
                                        (catch :default e
                                          (js/console.error e "Error handler for remote failed with an exception."))))
                      key           (-> ast :children first :key)
                      entity        (some-> ast :children first :query meta :pathom/entity)
                      ident-ent     {key (conj entity key)}]
                  (-> (p/let [res (request
                                    (cond-> {:pathom/ast ast}
                                      entity (assoc :pathom/entity ident-ent)))]
                        (ok-handler {:transaction (eql/ast->query ast)
                                     :body        res}))
                      (p/catch (fn [e]
                                 (js/console.error "Pathom Remote Error" e)
                                 (error-handler {:error e}))))))})

;; ---- FULCRO -----
(defonce app (app/fulcro-app
              {
               :remotes {:remote (pathom-remote pathom)}
               }))


(defsc ProductTile [this {:product/keys [id title slug] :as props}]
  {:query [:product/id :product/title :product/slug]
   :ident (fn [] [:product/id (:product/id props)])
   :initial-state (fn [{:keys [id title slug] :as params}]
                    {:product/id id
                     :product/title title
                     :product/slug slug})}
  (dom/li
   (dom/ul
    (dom/li (str "title: " title))
    (dom/li (str "slug: " slug)))))

(def ui-product-tile (comp/factory ProductTile))

(defsc ProductList [this {:keys [products]}]
  (dom/div
   (dom/h1 "Products")
   (println products)
   (dom/ul
    (map ui-product-tile products))))
(def ui-product-list (comp/factory ProductList))

(defsc Root [this {:keys [products]}]
  {:query [{:products (comp/get-query ProductTile)}]
   :initial-state (fn [params] {:products [(comp/get-initial-state ProductTile {:product/id 0 :product/title "test" :product/slug "slug"})
                                           (comp/get-initial-state ProductTile {:product/id 1 :product/title "test1" :product/slug "slug1"})]})}
  (dom/div {:className "a" :id "id"}
           (dom/p "Hello")
           (ui-product-list {:products  products})))


(defn ^:export init []
  (app/mount! app Root "app")
  (df/load! app :products Root)
  (println "Loaded app"))

(defn ^:export refresh
  "Shadow hot reload support"
  []
  (app/mount! app Root "app")
  (comp/refresh-dynamic-queries! app)
  (println "Hot reload"))
