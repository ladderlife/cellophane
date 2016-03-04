(ns cellophane.next-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.next :as om :refer [defui]]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1})
  (render [this]
    (dom/div nil "Hello World")))

(defui ComponentWithStatics
  static om/Ident
  (ident [this props]
    [:by-id 42])
  static om/IQuery
  (query [this]
    [:foo]))

(deftest test-defui
  (testing "defui definition works"
    (is SimpleComponent))
  (testing "defui implements Lifecycle protocols"
    (is (= (.initLocalState SimpleComponent) {:foo 1})))
  (testing "defui implements statics"
    (is (= (.query ComponentWithStatics) [:foo]))
    (is (= (.ident ComponentWithStatics {}) [:by-id 42]))))

(deftest test-factory)

