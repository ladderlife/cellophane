(ns cellophane.next-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.protocols :as p]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1}))

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
    (let [c (->SimpleComponent nil nil nil)]
      (is (cellophane/component? c))
      (is (= (.initLocalState c) {:foo 1}))))
  (let [c (->ComponentWithStatics nil nil nil)]
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

(deftest test-factory
  (let [simple-component-factory (cellophane/factory SimpleComponent)
        c (simple-component-factory)]
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

(defui ComponentWithQuery
  static cellophane/IQuery
  (query [this]
    [:foo :bar]))

(deftest test-queries
  (testing "iquery?"
    (let [cq-factory (cellophane/factory ComponentWithQuery)
          c (cq-factory)]
      (is (cellophane/iquery? ComponentWithQuery))
      (is (not (cellophane/iquery? SimpleComponent))))))
