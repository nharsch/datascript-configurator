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
;; - [] look at async-resolver
;; - [] get child attrs from child queries
(defn pres [p]
  (p/let [res p]
    (println res)))

(defn json-get [url]
  (p/let [json (.then (js/fetch url)
                    #(.json %))]
    (js->clj json :keywordize-keys true)))

;; (.then (json-get "https://dev.tempurpedic.com/api/products") prn)



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

;; TODO: consider using spec to verify
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
   :product/category-urls
   :product/id
   :product/url
   :product/product_class
   :product/date_created
   :product/skus
   :product/date_updated])

;; add product/ to key to avoid confusion with category
(def product-kmap
     {:children :product/children,
      :slug :product/slug,
      :parent :product/parent,
      :bundles :product/bundles,
      :category_names :product/category_names,
      :title :product/title,
      :categories :product/category-urls,
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


(pco/defresolver all-products-resolver [env _]
  {::pco/output [{:products  [output-product-keys]}]}
  (p/->>
   (json-get "https://dev.tempurpedic.com/api/products/")
   (map #(clojure.set/rename-keys % product-kmap))
   (#(if-some [product-ids (get (pco/params env) :product/ids)]
      (filter
       (fn [p] (contains? (set product-ids) (:product/id p))) %)
      %))
   vec
   (hash-map :products)
   ))

(filter  #(contains? #{1 2 3} %) [3 4 5])

(pco/defresolver all-category-resolver []
  {::pco/output [{:categories [category-output-keys]}]}
  (p/->>
   (json-get "https://dev.tempurpedic.com/api/categories/")
   (map #(clojure.set/rename-keys % cat-kmap))
   vec
   (hash-map :categories)))


(pco/defresolver product-id-resolver [{:keys [product/id products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== id (:product/id p))))
   first
   ))




(pco/defresolver category-id-resolver [{:keys [category/id categories]}]
  {::pco/output category-output-keys}
  (->>
   categories
   (filter (fn [p] (== id (:category/id p))))
   first
   ))

(pco/defresolver product-slug-resolver [{:keys [product/slug products]}]
  {::pco/output output-product-keys}
  (->>
   products
   (filter (fn [p] (== slug (:product/slug p))))
   first
   ))

(defn extract-id-from-url [urlstring]
  (let [url (new js/URL urlstring)]
    (-> url
         (.-pathname)
         (.split "/")
         reverse
         second
         int)))

(pco/defresolver caturls->ids
  [{:keys [product/category-urls categories]}]
  {:product/categories
   (->> category-urls
        (map extract-id-from-url)
        (map (partial hash-map :category/id))
        vec)})



(def env
  (pci/register
   [
    all-products-resolver
    product-id-resolver
    product-slug-resolver
    all-category-resolver
    category-id-resolver
    caturls->ids
    ]))


;; test queries
(comment
  (pres (p.a.eql/process env [:products]))
  ;; TODO: get parameterized ids to work
  (pres (p.a.eql/process env ['(:products {:product/ids [1 4 13]})]))
  (pres (p.a.eql/process env [:categories]))
  (pres (p.a.eql/process env [{:products [:product/id]}]))
  (pres (p.a.eql/process env [{:categories [:category/id]}]))
  (pres (p.a.eql/process env [{:products [:product/slug]}]))
  (pres (p.a.eql/process env [{:products [:product/title]}]))
  (pres (p.a.eql/process env [{[:product/id 1] [:product/title :product/slug]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/category-urls]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/category-ids]}]))
  (pres (p.a.eql/process env [{[:category/id 28] [:category/name]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/slug {:product/categories [:category/id :category/name]}]}]))
  (pres (p.a.eql/process env [{[:product/id 7] [:product/categories]}]))
  (pres (p.a.eql/process env [{[:product/id 7] output-product-keys}]))
  (pres (p.a.eql/process env [{[:product/id 7] output-product-keys}]))
  (pres (p.a.eql/process env [{[:category/id 2] category-output-keys}]))
  (pres (p.a.eql/process env [{[:product/slug "grandpillow"] [:product/id]}]))
  (pres (p.a.eql/process env [{[:product/slug "tempur-adjustable-support-pillow-bundle"] [:product/id]}]))
  (pres (p.a.eql/process env [:products {:products [:product/id :product/title :product/slug]}]))
  )





;; ---- FULCRO -----
(defsc ProductTile [this {:product/keys [id title slug] :as props}]
  {:query [:product/id :product/title :product/slug]
   :ident (fn [] [:product/id (:product/id props)])
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
  (comp/factory ProductTile))

(defsc ProductList [this {:keys [products]}]
  {:query [{:products (comp/get-query ProductTile)}]
   :initial-state (fn [{:keys []}]
                    {:products [(comp/get-initial-state ProductTile {:id 0 :title "test title" :slug "test-slug"})
                                (comp/get-initial-state ProductTile {:id 1 :title "test title 1" :slug "test-slug-1"})
                                ]})
   }
  (dom/div
   (dom/h1 "Products")
   (dom/ul
    (map ui-product-tile (:products (first products))))))
(def ui-product-list (comp/factory ProductList))

;; Category components
(defsc CategoryItem [this {:category/keys [id name] :as props}]
  {:query [:category/id :category/name]
   :ident (fn [] [:category/id (:category/id props)])
   :initial-state (fn [{:keys [id name]}]
                    {:category/id id
                     :category/name name})}
  (dom/li
   (dom/div
    (dom/p (str "name: " name)))))
(def ui-category-item (comp/factory CategoryItem {:keyfn :category/id}))

(defsc CategoryList [this {:keys [categories]}]
  {:query [{:categories (comp/get-query CategoryItem)}]
   :initial-state (fn [{:keys []}]
                      {:categories [comp/get-initial-state CategoryItem {:id 0 :name "Test Category"}]})}
  (dom/div
   (dom/h1 "Categories")
   (dom/ul
    (map ui-category-item (:categories (first categories))))))
(def ui-category-list (comp/factory CategoryList))



;; (defsc CategoryChooser [this {:keys [categories]}]
;;   (dom/div
;;    (dom/h1 "Categories")
;;    (dom/ul
;;     (map ui-category-item categories))))
;; (def ui-category-chooser (comp/factory CategoryChooser))

(defsc Root [this {:keys [products categories]}]
  {:query [{:products (comp/get-query ProductList)}
           {:categories (comp/get-query CategoryList)}]
   :initial-state (fn [{:keys [app]}]
                    {:products [{:products [{:product/id 0
                                             :product/title "Test Product"
                                             :product/slug "test-product"}]}]
                     :categories [{:categories  [{:category/id 0
                                                  :category/name "Test Category"}]}]
                     })}
  (dom/div {:className "root" :id "id"}
           (dom/p "Hello")
           (ui-product-list {:products products})
           (ui-category-list {:categories categories})))

(comment
  (pres (p.a.eql/process env [:all-products (comp/get-query ProductList)]))
  )

(def pathom-interface (p.a.eql/boundary-interface env))


;; Connect to PATHOM
(defn pathom-remote [request]
  ;; (PRINTLN "pathom-remote called" request)
  {:transmit! (fn transmit! [_ {::txn/keys [ast result-handler]}]
                ;; (println "transmit called" ast)
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
                        ;; (println "remote response" res)
                        (ok-handler {:transaction (eql/ast->query ast)
                                     :body        res}))
                      (p/catch (fn [e]
                                 (js/console.error "Pathom Remote Error" e)
                                 (error-handler {:error e}))))))})

(def app (app/fulcro-app {:remotes {:remote (pathom-remote pathom-interface)}}))

(defn ^:export init []
  (app/mount! app Root "app")
  (df/load! app :products ProductList)
  ;; (df/load! app :products ProductList {:params {:pathom-context {:product/ids [1 4 13]}}})
  (df/load! app :categories CategoryList)
  (println "Loaded app"))

(defn ^:export refresh []
  (app/mount! app Root "app")
  (comp/refresh-dynamic-queries! app)
  (println "Hot reload"))
