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
        [{:tree [{:children [{:counter [:value]}]}]}])))

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

