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
    (is SimpleComponent))
  (testing "defui implements Lifecycle protocols"
    (let [c (SimpleComponent)]
      (is (= (.initLocalState c) {:foo 1}))))
  (testing "defui implements statics"
    (let [c (ComponentWithStatics)]
      (is (= (.query c) [:foo]))
      (is (= (.ident c {}) [:by-id 42])))))

(deftest test-factory)

