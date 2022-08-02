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

(def category-input-keys
  [:description
   :children
   :slug
   :parent
   :products
   :name
   :id
   :url
   :image
   :depth])
(def category-output-keys
  [:category/description
   :category/children
   :category/slug
   :category/parent
   :category/products
   :category/name
   :category/id
   :category/url
   :category/image
   :category/depth])
(def cat-kmap (zipmap category-input-keys category-output-keys))


(pco/defresolver all-products-resolver []
  {::pco/output [{:all-products {:products  [output-product-keys]}}]}
  (p/->>
   (json-get "https://dev.tempurpedic.com/api/products/")
   (map #(clojure.set/rename-keys % product-kmap))
   vec
   (hash-map :products)
   (hash-map :all-products)))

(pco/defresolver all-category-resolver []
  {::pco/output [:all-categories {:categories [output-product-keys]}]}
  (p/->>
   (json-get "https://dev.tempurpedic.com/api/categories/")
   (map #(clojure.set/rename-keys % cat-kmap))
   vec
   (hash-map :categories)))


(pco/defresolver product-resolver [{:keys [product/id products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== id (:product/id p))))
   first
   ))

(pco/defresolver category-resolver [{:keys [category/id categories]}]
  {::pco/output category-output-keys}
  (->>
   categories
   (filter (fn [p] (== id (:category/id p))))
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

(pco/defresolver caturl->id
  [{:keys [product/categories]}]
  ;; (println "prod->cats")
  {:product/category-ids
   (->> categories
        (map extract-id-from-url)
        (map (partial hash-map :category/id))
        vec)})

(def env
  (pci/register
   [
    all-products-resolver
    product-resolver
    slug-resolver
    all-category-resolver
    category-resolver
    caturl->id
    ]))

(defn pres [p]
  (p/let [res p]
    (println res)))
;; test queries
(comment
  (pres (p.a.eql/process env [:all-products]))
  (pres (p.a.eql/process env [:categories]))
  (pres (p.a.eql/process env [{:all-products [:product/id]}]))
  (pres (p.a.eql/process env [{:categories [:category/id]}]))
  (pres (p.a.eql/process env [{:all-products [:product/slug]}]))
  (pres (p.a.eql/process env [{:all-products [:product/title]}]))
  (pres (p.a.eql/process env [{[:product/id 1] [:product/title :product/slug]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/categories :product/category-ids]}]))
  (pres (p.a.eql/process env [{[:product/id 7] output-product-keys}]))
  (pres (p.a.eql/process env [{[:category/id 2] category-output-keys}]))
  (pres (p.a.eql/process env [{[:product/slug "grandpillow"] [:product/id]}]))
  (pres (p.a.eql/process env [:all-products {:products [:product/id :product/title :product/slug]}]))
)





;; ---- FULCRO -----
(defsc ProductTile [this {:product/keys [id title slug]}]
  {:query [:product/id :product/title :product/slug]
   ;; :ident (fn [] [:product/id (:product/id props)])
   :initial-state (fn [{:keys [id title slug]}]
                    {:product/id id
                     :product/title title
                     :product/slug slug})

   }
  (dom/li {:key id}
          (dom/div
           (dom/p (str "title: " title))
           (dom/p (str "slug: " slug)))))

(def ui-product-tile
  (comp/factory ProductTile
                ;; {:keyfn :product/id}
                ))

(defsc ProductList [this {:keys [products]}]
  {:query [{:products (comp/get-query ProductTile)}]
   :initial-state (fn [{:keys []}]
                    {:products [(comp/get-initial-state ProductTile {:id 0 :title "test title" :slug "test-slug"})
                                (comp/get-initial-state ProductTile {:id 1 :title "test title 1" :slug "test-slug-1"})
                                ]})
   }
  (println "prods" products)
  (dom/div
   (dom/h1 "Products")
   (dom/ul
    (map ui-product-tile products))))
(def ui-product-list (comp/factory ProductList))


;; (defsc CategoryItem [this {:category/keys [id name] :as props}]
;;   {:query [:category/id :category/name]
;;    :ident (fn [] [:category/id (:category/id props)])}
;;   (dom/li
;;    (dom/ul
;;     (dom/li (str "name: " name)))))
;; (def ui-category-item (comp/factory CategoryItem {:keyfn :category/id}))

;; (defsc CategoryChooser [this {:keys [categories]}]
;;   (dom/div
;;    (dom/h1 "Categories")
;;    (dom/ul
;;     (map ui-category-item categories))))
;; (def ui-category-chooser (comp/factory CategoryChooser))

;; (defsc Root [this {:keys [products]}]
;;   {:query []
;;    :initial-state (fn [{:keys [app]}]
;;                     {:products [{:product/id 0
;;                                  :product/title "Test Product"
;;                                  :product/slug "test-product"}]})}
;;   (dom/div {:className "a" :id "id"}
;;            (dom/p "Hello")
;;            (ui-product-list {:products products})))

(comment
  (pres (p.a.eql/process env [:all-products (comp/get-query ProductList)]))
  )

(def pathom (p.a.eql/boundary-interface env))

(defn pathom-remote [request]
  ;; (PRINTLN "pathom-remote called" request)
  {:transmit! (fn transmit! [_ {::txn/keys [ast result-handler]}]
                ;; (println "transmit called" ast result-handler)
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
                  ;; (println key "key" entity "entity" ident-ent "ident-ent")
                  (-> (p/let [res (request
                                   (cond-> {:pathom/ast ast}
                                     entity (assoc :pathom/entity ident-ent)))]
                        (println "remote response" res)
                        (ok-handler {:transaction (eql/ast->query ast)
                                     :body        res}))
                      (p/catch (fn [e]
                                 (js/console.error "Pathom Remote Error" e)
                                 (error-handler {:error e}))))))})

(def app (app/fulcro-app {:remotes {:remote (pathom-remote pathom)}}))

(defn ^:export init []
  (app/mount! app ProductList "app")
  (df/load! app :all-products ProductList)
  (println "Loaded app"))

(defn ^:export refresh []
  (app/mount! app ProductList "app")
  (comp/refresh-dynamic-queries! app)
  (println "Hot reload"))

;; (refresh)
