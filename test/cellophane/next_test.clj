(ns cellophane.next-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.next :as cellophane :refer [defui ui]]
            [cellophane.dom :as dom]
            [cellophane.protocols :as p]
            [om.next.protocols :as om-p])
  (:import [cellophane.next Indexer]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1}))

(def simple-component-factory (cellophane/factory SimpleComponent))

(deftest test-get-prop
  (is (= (#'cellophane/get-prop (simple-component-factory) :cellophaneclj$depth) 0))
  (binding [cellophane/*shared* :fake]
    (is (= (#'cellophane/get-prop (simple-component-factory) :cellophaneclj$shared) :fake))))

(defui ComponentWithStatics
  static cellophane/Ident
  (ident [this props]
    [:by-id 42])
  static cellophane/IQuery
  (query [this]
    [:foo]))

(deftest test-defui
  (testing "defui definition works"
    (is SimpleComponent)
    (is (fn? SimpleComponent)))
  (testing "defui implements Lifecycle protocols"
    (let [c (SimpleComponent nil nil nil nil)]
      (is (cellophane/component? c))
      (is (= (.initLocalState c) {:foo 1}))))
  (let [c (ComponentWithStatics nil nil nil nil)]
    (testing "defui implements statics"
      (is (= (.query c) [:foo]))
      (is (= (.ident c {}) [:by-id 42])))
    (testing "allow defui not to implement lifecycle render"
      (is (cellophane/component? c))
      (is (not (cellophane/renderable? c)))))
  (is (fn? (-> SimpleComponent meta :component)))
  (testing "`ui` macro"
    (is (= (cellophane/get-query
             (ui
               static cellophane/IQuery
               (query [this] [:foo])))
           [:foo]))))

(deftest test-component?-predicate
  (let [simple-c-factory (cellophane/factory SimpleComponent)
        c (simple-c-factory)]
    (is (cellophane/component? c))
    (is (not (cellophane/component? simple-c-factory)))
    (is (not (cellophane/component? SimpleComponent)))))

(defui ReactKeysChild
  static cellophane/IQuery
  (query [this]
    [:name])
  Object
  (render [this]
    (let [p (cellophane/props this)]
      (dom/div nil (:name p)))))

(def react-keys-child-factory (cellophane/factory ReactKeysChild))

(defui ReactKeysParent
  static cellophane/IQuery
  (query [this]
    [{:children (cellophane/get-query ReactKeysChild)}])
  Object
  (render [this]
    (let [children (:children (cellophane/props this))]
      (dom/div nil
        (map react-keys-child-factory children)))))

(def react-keys-state
  {:children [{:name "John"} {:name "Mary"}]})

(defn react-keys-read
  [{:keys [state]} _ _]
  {:value (:children @state)})

(deftest test-react-bridging
  (testing "factory, state, props, children"
    (let [c (simple-component-factory)]
      (is (satisfies? p/IReactComponent c))
      (is (= (cellophane/get-state c) {:foo 1}))
      (is (= (cellophane/get-state c :foo) 1))
      (is (= (cellophane/get-state c [:foo]) 1))
      (is (= (cellophane/get-rendered-state c :foo) 1))
      (is (= (cellophane/get-rendered-state c [:foo]) 1))
      (is (= (cellophane/get-state c) (cellophane/get-rendered-state c)))
      (cellophane/set-state! c {:bar 1})
      (is (= (cellophane/get-state c) {:bar 1}))
      (cellophane/update-state! c #(update-in % [:bar] inc))
      (is (= (cellophane/get-state c) {:bar 2}))
      (is (= (cellophane/props c) nil))
      (is (= (cellophane/props (simple-component-factory {:foo 1})) {:foo 1}))
      (is (= (cellophane/children (simple-component-factory nil "some text"))
            ["some text"]))))
  (testing "react keys"
    (let [c (simple-component-factory {:react-key "foo"})
          r (cellophane/reconciler {:state (atom react-keys-state)
                                    :parser (cellophane/parser {:read react-keys-read})})
          rks-c (cellophane/add-root! r ReactKeysParent nil)]
      (is (= (cellophane/react-key c) "foo"))
      (is (= (-> (p/-render rks-c) :children first :react-key)
             "cellophane$next_test$ReactKeysChild_[:children 0]"))))
  (testing "react type"
    (let [c (simple-component-factory)]
      (is (thrown? AssertionError (cellophane/react-type 42)))
      (is (thrown? AssertionError (cellophane/react-type nil)))
      (is (= (cellophane/react-type c) SimpleComponent)))))

(deftest test-factory-validator
  (let [f (cellophane/factory SimpleComponent {:validator (fn [props] (some? props))})]
    (is (thrown? AssertionError (f nil)))))

(deftest test-computed-props
  (is (= (cellophane/get-computed (cellophane/computed {} {:a 1}))
         {:a 1}))
  (is (= (cellophane/get-computed (cellophane/computed {:some :prop} {:a 1}))
         {:a 1}))
  (is (= (cellophane/computed {:some :prop} {:a 1})
         {:cellophane.next/computed {:a 1} :some :prop})))

(defui ComponentWithQPs
  static cellophane/IQueryParams
  (params [this]
    {:a 1})
  static cellophane/IQuery
  (query [this]
    '[:foo (:bar {:a ?a})]))

(defui ComponentWithQuery
  static cellophane/IQuery
  (query [this]
    [:foo :bar]))

(defui SubqueryChild
  static cellophane/IQuery
  (query [this]
    [:foo :bar])
  Object
  (render [this]
    (dom/div nil "I'm a child")))

(def subquery-child-factory (cellophane/factory SubqueryChild))

(defui SubqueryParent
  static cellophane/IQuery
  (query [this]
    [{:children (cellophane/get-query SubqueryChild)}])
  Object
  (render [this]
    (dom/div nil
      (subquery-child-factory {:ref :child-1})
      (subquery-child-factory {:ref :child-2}))))

(deftest test-queries
  (let [cqps-factory (cellophane/factory ComponentWithQPs)
        cq-factory (cellophane/factory ComponentWithQuery)
        cqps (cqps-factory)
        cq (cq-factory)]
    (testing "iquery?"
      (is (cellophane/iquery? ComponentWithQPs))
      (is (cellophane/iquery? cqps))
      (is (not (cellophane/iquery? SimpleComponent))))
    (testing "get-query"
      (is (= (cellophane/get-query ComponentWithQPs)
             '[:foo (:bar {:a 1})]))
      (is (= (meta (cellophane/get-query ComponentWithQPs))
             {:component ComponentWithQPs}))
      #_(is (= (cellophane/get-query cqps)
             '[:foo (:bar {:a 1})]))
      #_(is (= (meta (cellophane/get-query cqps))
             {:component ComponentWithQPs}))
      (is (= (cellophane/get-query ComponentWithQuery)
             [:foo :bar]))
      #_(is (= (cellophane/get-query cq)
             [:foo :bar]))
      (is (= (cellophane/get-unbound-query cq)
             [:foo :bar]))
      (is (= (cellophane/get-unbound-query cqps)
             '[:foo (:bar {:a ?a})]))
      (is (= (cellophane/params ComponentWithQPs)
             {:a 1}))
      (is (nil? (cellophane/params SimpleComponent)))
      (is (= (cellophane/get-params cqps)
             {:a 1}))))
  (testing "subquery"
    (let [factory (cellophane/factory SubqueryParent)
          c (factory)]
      (is (= (cellophane/subquery c :child-1 SubqueryChild)
             [:foo :bar]))
      (p/-render c)
      (is (#'cellophane/mounted? c))
      (is (cellophane/react-ref c :child-1))
      (is (#'cellophane/mounted? (cellophane/react-ref c :child-1)))
      #_(is (= (cellophane/subquery c :child-1 SubqueryChild)
             [:foo :bar])))))

(deftest test-focus-query
  (is (= (cellophane/focus-query [:foo/bar] [])
         [:foo/bar]))
  (is (= (cellophane/focus-query
           [:foo/bar {:baz/woz [:goz/noz]}]
           [:baz/woz])
         [{:baz/woz [:goz/noz]}]))
  (is (= (cellophane/focus-query
           [:foo/bar :baz/woz]
           [:baz/woz])
        [:baz/woz]))
  (is (= (cellophane/focus-query
           [:foo/bar {:baz/woz [:goz/noz {:bop/wop [:nop/sop]} :cuz/wuz]}]
           [:baz/woz :bop/wop])
        [{:baz/woz [{:bop/wop [:nop/sop]}]}]))
  (is (= (cellophane/focus-query
           '[{:tree [:id {:counter [:value]} {:children ...}]}]
           [:tree :children :counter])
        [{:tree [{:children [{:counter [:value]}]}]}]))
  (is (= (cellophane/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/foo])
         '[{:tree {:tree/foo [{:children {:tree/foo [:id :node/type :foo/value {:children ...}]}}]}}]))
  (is (= (cellophane/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/bar])
         '[{:tree
            {:tree/foo [{:children {:tree/bar [:id :node/type {:counter [:value]}
                                               :bar/value {:children ...}]}}]}}]))
  (is (= (cellophane/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/bar :counter])
         '[{:tree
            {:tree/foo [{:children {:tree/bar [{:counter [:value]}]}}]}}])))

(deftest test-focus->path
  (is (= (#'cellophane/focus->path [{:baz/woz [{:bop/wop [:nop/sop]}]}])
         [:baz/woz :bop/wop]))
  (is (= (#'cellophane/focus->path [:app/title {:counters/list [:db/id :counter/count]}])
         []))
  (is (= (#'cellophane/focus->path [{:todos/list [{[:todo/by-id 0] [:id :title]}]}])
         [:todos/list [:todo/by-id 0]]))
  (is (= (#'cellophane/focus->path [{:todos/list [{'[:current-todo _] [:id :title]}]}])
         [:todos/list '[:current-todo _]]))
  (is (= (#'cellophane/focus->path [{:people/list [{[:person/by-id 0] [{:person/name [:name/first :name/last]}]}]}])
         [:people/list [:person/by-id 0] :person/name])))

(deftest test-temp-id-equality
  (let [uuid (java.util.UUID/randomUUID)
        id0  (cellophane/tempid uuid)
        id1  (cellophane/tempid uuid)]
    (is (= (cellophane/tempid? id0)))
    (is (= id0 id1))
    (is (= (hash id0) (hash id1)))))

(defui ComponentWithIdent
  static cellophane/Ident
  (ident [this {:keys [id]}]
    [:item/by-id id]))

(deftest test-get-ident
  (let [factory (cellophane/factory ComponentWithStatics)
        c (factory {})]
    (is (= (cellophane/get-ident c) [:by-id 42])))
  (let [factory (cellophane/factory ComponentWithIdent)
        c (factory {:id 3})]
    (is (= (cellophane/get-ident c) [:item/by-id 3]))))

(defui ReactRefsChild
  Object
  (render [this]
    (dom/div nil "some text")))

(def child-factory (cellophane/factory ReactRefsChild))

(defui ReactRefsParent
  Object
  (render [this]
    (dom/div nil
      (child-factory {:ref "foo"}))))

(deftest test-react-refs
  (let [factory (cellophane/factory ReactRefsParent)
        c (factory)]
    (p/-render c)
    (is (= (cellophane/react-type
             (cellophane/react-ref c "foo"))
           ReactRefsChild))))

(deftest test-dom-node
  (let [factory (cellophane/factory ReactRefsParent)
        c (factory)
        sb (StringBuilder.)]
    (p/-render c)
    (is (instance? cellophane.dom.Element (dom/node c)))
    (is (instance? cellophane.dom.Element (dom/node c "foo")))
    (dom/render-element! (dom/node c "foo") (volatile! 1) sb)
    (is (= (str sb)
           "<div data-reactroot=\"\" data-reactid=\"1\">some text</div>"))))

(defui ClassPathChild
  static cellophane/IQuery
  (query [this]
    [:foo :bar])
  Object
  (render [this]
    (dom/div nil "stuff")))

(def class-path-child-factory (cellophane/factory ClassPathChild))

(defui ClassPathParent
  static cellophane/IQuery
  (query [this]
    [{:child (cellophane/get-query ClassPathChild)}])
  Object
  (render [this]
    (class-path-child-factory {:ref "child"})))

(deftest test-class-path
  (let [factory (cellophane/factory ClassPathParent)
        c (factory)]
    (p/-render c)
    (is (= (cellophane/class-path (cellophane/react-ref c "child"))
           [ClassPathParent ClassPathChild]))))

(deftest test-reconciler
  (let [r (cellophane/reconciler {:state {:a 1}})]
    (is (cellophane/reconciler? r))
    (is (= @(cellophane/app-state r) {:a 1}))))

;; Indexer

(defui ^:once Component
  static cellophane/IQuery
  (query [this]
    '[:foo/bar :baz/woz]))

(defui ComponentList
  static cellophane/IQueryParams
  (params [this]
    {:component (cellophane/get-query Component)})
  static cellophane/IQuery
  (query [this]
    '[{:components/list ?component} :app/title]))

(defui ComponentA
  static cellophane/IQuery
  (query [this]
    '[:foo]))

(defui ComponentB
  static cellophane/IQuery
  (query [this]
    '[:bar]))

(defui RootComponent
  static cellophane/IQueryParams
  (params [this]
    {:component (cellophane/get-query ComponentA)})
  static cellophane/IQuery
  (query [this]
    '[{:components/list ?component}]))

(defui IdxrChild
  static cellophane/IQuery
  (query [_]
    [:name]))

(defui IdxrRoot
  static cellophane/IQuery
  (query [_]
    [{:root [{:child (cellophane/get-query IdxrChild)}]}]))

(defui OM-595-Component
  static cellophane/IQuery
  (query [this]
    '[{:item [:id :title {:next ...}]}]))

(defui IdxrNode
  static cellophane/IQuery
  (query [this]
    '[:node-value {:children ...}]))

(defui IdxrTree
  static cellophane/IQuery
  (query [this]
    [{:tree (cellophane/get-query IdxrNode)}]))

(defui IdxrLinkProp
  static cellophane/IQuery
  (query [this]
    '[:foo [:current-user _]]))

(defui IdxrLinkJoin
  static cellophane/IQuery
  (query [this]
    '[:foo {[:current-user _] [:name :email]}]))

(defui IdxrIdentProp
  static cellophane/IQuery
  (query [this]
    '[:foo [:users/by-id 2]]))

(defui IdxrIdentJoin
  static cellophane/IQuery
  (query [this]
    '[:foo {[:users/by-id 2] [:id :name :email]}]))

(defui IdxrLinkItem
  static cellophane/IQuery
  (query [this]
    [:b]))

(defui IdxrLinkRoot
  static cellophane/IQuery
  (query [this]
    [{[:a '_] (cellophane/get-query IdxrLinkItem)}]))

(defui IdxrParamsComponent
  static cellophane/IQueryParams
  (params [_]
    {:foo ""})
  static cellophane/IQuery
  (query [_]
    '[(:some/key {:foo ?foo})]))

(deftest test-indexer
  (testing "prop->classes"
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr ComponentList)]
      (is (= (set (keys (:prop->classes idxs)))
            #{:app/title :components/list :foo/bar :baz/woz})))
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr IdxrParamsComponent)]
      (is (= (set (keys (:prop->classes idxs)))
            #{:some/key}))))
  (testing "simple recursion indexing"
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr IdxrTree)
          cps (keys (:class-path->query idxs))]
      (is (= (count cps) 2))
      (is (not (nil? (some #{[IdxrTree IdxrNode]} cps))))))
  (testing "OM-595: recursion queries without own component"
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr OM-595-Component)
          cps (keys (:class-path->query idxs))]
      (is (= (count cps) 1))
      (is (= (first cps) [OM-595-Component]))))
  (testing "OM-612: regression introduced by OM-595"
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr IdxrRoot)
          cps (keys (:class-path->query idxs))]
      (is (not (nil? (some #{[IdxrRoot IdxrChild]} cps))))))
  (testing "OM-620: link & indent indexing"
    (let [idxr (cellophane/indexer)]
      (are [class res] (= (->> class
                            (om-p/index-root idxr)
                            :prop->classes keys set)
                         res)
        IdxrLinkProp #{:foo :current-user}
        IdxrLinkJoin #{:foo :current-user :name :email}
        IdxrIdentProp #{:foo [:users/by-id 2]}
        IdxrIdentJoin #{:foo [:users/by-id 2] :id :name :email})))
  (testing "OM-639: index-root fails on links"
    (let [idxr (cellophane/indexer)
          idxs (om-p/index-root idxr IdxrLinkRoot)]
      (is (contains? idxs :prop->classes)))))

(deftest test-reindex-instances
  (let [r (cellophane/reconciler
            {:state (atom nil)
             :parser (cellophane/parser {:read (fn [_ _ _] {})})})
        idxr (get-in r [:config :indexer])
        ;; simulate mounting
        _ (om-p/add-root! r RootComponent nil nil)
        _ (om-p/index-component! idxr (RootComponent nil nil #js {:cellophaneclj$reconciler r} nil))
        indexes @(:indexes idxr)
        classes (-> indexes :class->components keys)
        cps (-> indexes :class-path->query keys)
        c (first (get-in indexes [:class->components RootComponent]))]
    (is (= (first classes) RootComponent))
    (is (not (nil? (some #{[RootComponent ComponentA]} cps))))
    ;; will reindex
    (cellophane/set-query! c {:params {:component (cellophane/get-query ComponentB)}})
    (let [indexes @(:indexes idxr)
          cps (-> indexes :class-path->query keys)]
      (is (not (nil? (some #{[RootComponent ComponentB]} cps)))))))

(deftest test-reconciler-has-indexer
  (let [r (cellophane/reconciler
            {:state (atom nil)})]
    (is (instance? Indexer (get-in r [:config :indexer])))))

(def data
  {:list/one [{:name "John" :points 0 :friend {:name "Bob"}}
              {:name "Mary" :points 0 :foo :bar}
              {:name "Bob" :points 0 :friend {:name "John"}}]
   :list/two [{:name "Gwen" :points 0 :friends [{:name "Jeff"}]}
              {:name "Mary" :points 0 :baz :woz}
              {:name "Jeff" :points 0 :friends [{:name "Gwen"}]}]})

(defui Person
  static cellophane/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static cellophane/IQuery
  (query [this]
    [:name :points
     {:friend (cellophane/tag [:name] Person)}
     {:friends (cellophane/tag [:name] Person)}
     :foo :baz])
  Object
  (render [this]))

(defui ListView
  Object
  (render [this]))

(defui RootView
  static cellophane/IQuery
  (query [this]
    (let [subquery (cellophane/get-query Person)]
      [{:list/one subquery} {:list/two subquery}]))
  Object
  (render [this]))

(deftest test-tree->db
  (let [norm (cellophane/tree->db RootView data)
        refs (meta norm)
        p0   (get-in refs [:person/by-name "Mary"])]
    (is (= 3 (count (get norm :list/one))))
    (is (= {:name "John" :points 0 :friend [:person/by-name "Bob"]}
           (get-in refs [:person/by-name "John"])))
    (is (= 3 (count (get norm :list/two))))
    (is (contains? p0 :foo))
    (is (contains? p0 :baz))))

(deftest test-incremental-tree->db
  (let [p0   (cellophane/tree->db Person
               {:name "Susan" :points 5 :friend {:name "Mary"}})
        refs (meta p0)]
    (is (= {:name "Susan" :points 5 :friend [:person/by-name "Mary"]}
           p0))
    (is (= refs {:person/by-name {"Mary" {:name "Mary"}}}))))

(deftest test-db->tree-ident-chain
  (testing "simple joins"
    (let [data {:a [:b 1]
                :b {1 [:c 2]}
                :c {2 {:foo "bar"}}}
          query [{:a [:foo]}]]
      (is (= (cellophane/db->tree query data data)
            {:a {:foo "bar"}}))))
  (testing "simple unions"
    (let [data {:a [:b 1]
                :b {1 [:c 2]}
                :c {2 {:foo "foo" :bar "bar"}}}]
      (is (= (cellophane/db->tree [{:a {:b [:x] :c [:foo]}}] data data)
             {:a {:foo "foo"}}))))
  (testing "unions with idents in queries"
    (let [data {:y {"bla" [:z "meh"]}
                :z {"meh" {:name "Bar" :age 25}}}]
      (is (= (cellophane/db->tree [{[:y "bla"] {:z [:name]}}] data data)
             {[:y "bla"] {:name "Bar"}})))))

;; Remote test

(defn remote-read
  [{:keys [state query]} k _]
  (assert (= k :some/list))
  (let [st @state]
    (if (contains? st k)
      {:value (cellophane/db->tree query (get st k) st)}
      {:remote true})))

(defui RemotePerson
  static cellophane/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static cellophane/IQuery
  (query [this]
    [:name :age])
  Object
  (render [this]
    (let [{:keys [name age] :as props} (cellophane/props this)]
      (dom/li nil
        (dom/label nil (str name ", age: " age))))))

(def remote-person (cellophane/factory RemotePerson {:keyfn :name}))

(defui RemoteList
  static cellophane/IQuery
  (query [this]
    [{:some/list (cellophane/get-query RemotePerson)}])
  Object
  (render [this]
    (let [{:keys [some/list]} (cellophane/props this)]
      (apply dom/ul nil
        (map remote-person list)))))

(defn remote-send [{:keys [remote]} cb]
  ;; simulate calling the server parser
  (cb {:some/list [{:name "John" :age 30} {:name "Mary" :age 25}]} remote))

(def remote-reconciler
  (cellophane/reconciler {:state (atom {})
                          :normalize true
                          :parser (cellophane/parser {:read remote-read})
                          :send remote-send}))

(deftest test-remote-send
  (let [c (cellophane/add-root! remote-reconciler RemoteList nil)]
    (is (some? c))
    (is (not (empty? @remote-reconciler)))))

(defui MigratePerson
  static cellophane/Ident
  (ident [this {:keys [db/id]}]
    [:person/by-id id])
  static cellophane/IQuery
  (query [this]
    [:db/id :person/name]))

(defui MigratePeople
  static cellophane/IQuery
  (query [this]
    [{:people (cellophane/get-query MigratePerson)}]))

(defn migrate-read
  [{:keys [state query]} k _]
  (let [st @state]
    {:value (cellophane/db->tree query (get st k) st)}))

(deftest test-migrate
  (let [tmpid (cellophane/tempid (java.util.UUID/randomUUID))
        r (cellophane/reconciler {:state (atom {:people [[:person/by-id tmpid]]
                                                :person/by-id  {tmpid {:db/id tmpid
                                                                       :person/name "Joe"}}})
                                  :normalize true
                                  :id-key :db/id
                                  :parser (cellophane/parser {:read migrate-read})})]
    (cellophane/add-root! r MigratePeople nil)
    (is (cellophane/tempid? (-> @r :person/by-id ffirst)))
    (cellophane/merge! r
      {'some/action! {:tempids {[:person/by-id tmpid] [:person/by-id 42]}}}
      (cellophane/get-query MigratePeople))
    (is (not (cellophane/tempid? (-> @r :person/by-id ffirst))))
    (is (= (-> @r :person/by-id ffirst) 42))))

(defui InstrumentChild
  Object
  (render [this]
    (dom/p nil "I'm a child")))

(def instrument-child (cellophane/factory InstrumentChild {:instrument? true}))

(defui InstrumentRoot
  Object
  (render [this]
    (dom/div nil
      (instrument-child))))

(deftest test-instrument
  (let [cnt (atom 0)
        r (cellophane/reconciler
            {:state (atom {:root 1})
             :instrument (fn [{:keys [class props children factory]}]
                           (swap! cnt inc)
                           (apply factory props children))})]
    (dom/render-to-str (cellophane/add-root! r InstrumentRoot nil))
    (is (= @cnt 2))))

(defmulti transact-read cellophane/dispatch)
(defmulti transact-mutate cellophane/dispatch)

(defmethod transact-read :app/count
  [{:keys [state]} k _]
  {:value (get @state k)})

(defmethod transact-mutate 'this/throws
  [_ _ _]
  {:action #(throw (Exception.))})

(defmethod transact-mutate 'app/inc!
  [{:keys [state]} _ _]
  {:action #(swap! state update-in [:app/count] inc)})

(deftest test-transact!
  (let [r (cellophane/reconciler {:state (atom {:app/count 0})
                          :parser (cellophane/parser {:read transact-read
                                                      :mutate transact-mutate})})
        t-ret (cellophane/transact! r '[(app/inc!)])
        t-err (cellophane/transact! r '[(this/throws)])]
    (is (= (:app/count @r) 1))
    (is (some? t-ret))
    (is (= t-ret {'app/inc! {:result {:app/count 1}}}))
    (is (= (cellophane/transact! r '[(app/inc!) :app/count])
          {'app/inc! {:result {:app/count 2}}
           :app/count 2}))
    (is (some? t-err))
    (is (contains? t-err 'this/throws))
    (is (contains? (get t-err 'this/throws) :om.next/error))
    (is (not (contains? (get t-err 'this/throws) :result)))
    (is (= (get (cellophane/transact! r '[(this/throws) :app/count]) :app/count) 2))))

;; Bugs

(deftest test-om-604
  (let [data {:items [[:by-id 1]]
              :by-id {1 {:id 1
                         :stuff [{:name "something"}]}}}]
    (is (= (cellophane/db->tree [{:items [:id {:stuff [:name]}]}] data data)
           {:items [{:id 1, :stuff [{:name "something"}]}]})))
  (let [data {:todos/list [{:id 42
                            :author {:first-name "John"
                                     :last-name "Smith"}
                            :states [:done :archived]}
                           {:id 43
                            :author {:first-name "Mary"
                                     :last-name "Brown"}
                            :states [:done :archived]}]}]
    (is (= (cellophane/db->tree [{:todos/list [:id {:author [:first-name]}]}] data data)
           {:todos/list [{:id 42 :author {:first-name "John"}}
                         {:id 43 :author {:first-name "Mary"}}]})))
  (let [data {:items [[:by-id 1]]
              :by-id {1 {:id 1
                         :stuff [{:name {:first "John" :last "Smith"}}]}}}]
    (is (= (cellophane/db->tree [{:items [:id {:stuff [{:name [:first]}]}]}] data data)
          {:items [{:id 1, :stuff [{:name {:first "John"}}]}]}))
    (is (= (cellophane/db->tree [{:items [:id {:stuff ['({:name [:first]} {:param 1})]}]}] data data)
          {:items [{:id 1, :stuff [{:name {:first "John"}}]}]}))))

(deftest test-om-637
  (let [data {:some/list [[:item/by-id 1]]
              :item/by-id {1 {:id 1
                              :name "foo"}}}]
    (is (= (cellophane/db->tree '[{:some/list [*]}] data data)
           {:some/list [{:id 1, :name "foo"}]})))
  (is (= (cellophane/db->tree '[[:my-route _]] {:my-route [:some-route]} {:my-route [:some-route]})
         {:my-route [:some-route]}))
  (is (= (cellophane/db->tree '[[:my-route _]] {:my-route [:some-route :other-key]} {:my-route [:some-route :other-key]})
         {:my-route [:some-route :other-key]}))
  (let [data {:todos/list [[:todo/by-id 42]]
              :todo/by-id {42 {:id 42
                               :states [:archived]}}}]
    (is (= (cellophane/db->tree [{:todos/list [:id :states]}] data data)
           {:todos/list [{:id 42, :states [:archived]}]})))
  (let [data {:todos/list [[:todo/by-id 42]]
              :todo/by-id {42 {:id 42
                               :states [:done :archived]}}
              :current-users [{:id 0 :name "John"} {:id 1 :name "Mary"}]}]
    (is (= (cellophane/db->tree [{:todos/list [:id :states '[:current-users _]]}] data data)
           {:todos/list [{:id 42, :states [:done :archived]
                          :current-users [{:id 0 :name "John"}
                                          {:id 1, :name "Mary"}]}]})))
  (let [data {:todos/list [{:id 42 :title "do stuff" :states [:done :archived]}
                           {:id 43 :title "buy milk" :states [:done :archived]}]}]
    (is (= (cellophane/db->tree [{:todos/list [:id :title]}] data data)
           {:todos/list [{:id 42 :title "do stuff"}
                         {:id 43 :title "buy milk"}]}))))

(deftest test-om-727
  (let [data {:item [:bar 0]
              :bar    {0 {:id 0 :next [:bar 1]}
                       1 {:id 1 :next [:bar 2]}
                       2 {:id 2}}}]
    (is (= (cellophane/db->tree [{:item {:foo [:id] :bar [:id {:next '...}]}}] data data)
           {:item {:id 0, :next {:id 1, :next {:id 2}}}}))))

(deftest test-om-732
  (let [state {:curr-view [:main :view]
               :main {:view {:curr-item [[:sub-item/by-id 2]]}}
               :sub-item/by-id {2 {:foo :baz :sub-items [[:sub-item/by-id 4]]}
                                4 {:foo :bar}}}]
    (is (= (cellophane/db->tree [{:curr-view
                          {:main [{:curr-item [:foo {:sub-items '...}]}]}}] state state)
           {:curr-view {:curr-item [{:foo :baz :sub-items [{:foo :bar}]}]}}))))

(defui ^:private Private
  static cellophane/IQuery
  (query [this]
    [:foo]))

(defui ^:private ^:once PrivateOnce
  static cellophane/IQuery
  (query [this]
    [:foo]))

(deftest test-om-739
  (is (true? (-> #'Private meta :private)))
  (is (true? (-> #'PrivateOnce meta :private))))
