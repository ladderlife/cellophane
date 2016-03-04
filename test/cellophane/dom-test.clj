(ns cellophane.dom-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.dom :as dom]))

(defn test-tags [tags res-fn]
  `(are [element# res#] (= (dom/render-element {:tag element#}) res#)
     ~@(mapcat (fn [tag#] [tag# (res-fn tag#)]) tags)))

(defmacro test-container-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(dom/container-tag? % nil)))]
    (test-tags container-tags #(str "<" % ">" "</" % ">"))))

(defmacro test-void-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(not (dom/container-tag? % nil))))]
    ;; TODO: should we add mode for XHTML in which tags need to have a
    ;; closing slash? e.g. "<input/>" vs "<input>"
    (test-tags container-tags #(str "<" % ">"))))

(defn simple-component []
  (dom/div nil "Hello World"))

(deftest test-render-element
  (testing "render-element works with empty content in all tags"
    (test-container-tags)
    (test-void-tags))
  (testing "render-element renders simple function elements"
    (is (= (dom/render-element (simple-component)) "<div>Hello World</div>"))))
