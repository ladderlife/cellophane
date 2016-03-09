(ns cellophane.next-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.dom :as dom]
            [cellophane.protocols :as p]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1}))

(def simple-component-factory (cellophane/factory SimpleComponent))

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
    (is (class? SimpleComponent)))
  (testing "defui implements Lifecycle protocols"
    (let [c (->SimpleComponent nil nil nil nil)]
      (is (cellophane/component? c))
      (is (= (.initLocalState c) {:foo 1}))))
  (let [c (->ComponentWithStatics nil nil nil nil)]
    (testing "defui implements statics"
      (is (= (.query c) [:foo]))
      (is (= (.ident c {}) [:by-id 42])))
    (testing "allow defui not to implement lifecycle render"
      (is (cellophane/component? c))
      (is (not (cellophane/renderable? c))))))

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

(deftest test-react-bridging
  (testing "factory, state, props, children"
    (let [c (simple-component-factory)]
      (is (satisfies? p/IReactComponent c))
      (is (= (cellophane/get-state c) {:foo 1}))
      (is (= (cellophane/get-state c :foo) 1))
      (is (= (cellophane/get-state c [:foo]) 1))
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
          rks-factory (cellophane/factory ReactKeysParent)
          rks-c (rks-factory react-keys-state)]
      (is (= (cellophane/react-key c) "foo"))
      (is (= (-> (p/-render rks-c)
               :children first :react-key) "cellophane$next_test$ReactKeysChild_[:children 0]"))))
  (testing "react type"
    (let [c (simple-component-factory)]
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
      (is (= (cellophane/get-query cqps)
             '[:foo (:bar {:a 1})]))
      (is (= (cellophane/get-query ComponentWithQuery)
             [:foo :bar]))
      (is (= (cellophane/get-query cq)
             [:foo :bar]))
      (is (= (cellophane/get-unbound-query cq)
             [:foo :bar]))
      (is (= (cellophane/get-unbound-query cqps)
             '[:foo (:bar {:a ?a})]))
      (is (= (cellophane/get-params cqps)
             {:a 1})))))

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
    (is (= (cellophane/react-type
             (cellophane/react-ref c "foo"))
           ReactRefsChild))))
