(ns cellophane.core-test
  (require [clojure.test :refer [deftest testing is are]]
           [cellophane.next :as om :refer [defui]]
           [cellophane.dom :as dom]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1})
  (render [this]
    (dom/div nil "Hello World")))

(deftest test-defui
  (testing "defui definition works"
    (is SimpleComponent))
  (testing "defui implements Lifecycle protocols"
    (is (= (.initLocalState SimpleComponent) {:foo 1}))))

