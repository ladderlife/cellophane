(ns cellophane.dom-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.dom :as dom]))

(defn- remove-whitespace [s]
  (->> (str/split s #">\s+<")
    (filter #(not (str/blank? %)))
    (str/join "><")))

(defn test-tags [tags res-fn]
  `(are [element# res#] (= (dom/render-element {:tag element# :react-id [0]}) res#)
     ~@(mapcat (fn [tag#] [tag# (res-fn tag#)]) tags)))

(defmacro test-container-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(dom/container-tag? % nil)))]
    (test-tags container-tags #(str "<" % " data-reactid=\".0\">" "</" % ">"))))

(defmacro test-void-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(not (dom/container-tag? % nil))))]
    ;; TODO: should we add mode for XHTML in which tags need to have a
    ;; closing slash? e.g. "<input/>" vs "<input>"
    (test-tags container-tags #(str "<" % " data-reactid=\".0\">"))))

(defn simple-component []
  (dom/div nil "Hello World"))

(defn simple-nested-component []
  (dom/div nil
    (dom/h1 #js {:id "page-title"} "Title")))

(defn comp-nested-component []
  (dom/div nil
    (simple-component)
    (simple-nested-component)))

(deftest test-render-element
  (testing "render-element works with empty content in all tags"
    (test-container-tags)
    (test-void-tags))
  (testing "render-element renders simple function elements"
    (are [component res] (= (dom/render-element (dom/assign-react-ids component)) res)
      (simple-component) "<div data-reactid=\".0\">Hello World</div>"
      (simple-nested-component) (remove-whitespace
                                  "<div data-reactid=\".0\">
                                     <h1 data-reactid=\".0.0\" id=\"page-title\">Title</h1>
                                   </div>")
      (comp-nested-component) (remove-whitespace
                                "<div data-reactid=\".0\">
                                   <div data-reactid=\".0.0\">Hello World</div>
                                   <div data-reactid=\".0.1\">
                                     <h1 data-reactid=\".0.1.0\" id=\"page-title\">Title</h1>
                                   </div>
                                 </div>"))))

(defui SimpleComponent
  Object
  (render [this]
    (dom/div nil "Hello World")))

(defui Hello
  Object
  (render [this]
    (dom/p nil (-> this cellophane/props :text))))

(defui Children
  Object
  (render [this]
    (dom/div nil
      (map identity
        #js [(dom/div nil "Foo")
             (dom/div nil "Bar")
             (map identity
               #js [(dom/div nil "Bar")
                    (dom/div nil "Woz")])]))))

(deftest test-render-to-str
  (let [c (->SimpleComponent nil nil nil)]
    (is (= (dom/render-to-str c) "<div data-reactid=\".0\">Hello World</div>")))
  (let [hello (cellophane/factory Hello)]
    (is (= (dom/render-to-str (hello {:text "Hello, world!"}))
           "<p data-reactid=\".0\">Hello, world!</p>")))
  (let [children (cellophane/factory Children)]
    (is (= (dom/render-to-str (children))
          (remove-whitespace "<div data-reactid=\".0\">
                                <div data-reactid=\".0.0\">Foo</div>
                                <div data-reactid=\".0.1\">Bar</div>
                                <div data-reactid=\".0.2\">Bar</div>
                                <div data-reactid=\".0.3\">Woz</div>
                              </div>")))))

(def styles
  #js {:textAlign "center"
       :marginLeft "10px"})

(defui ComponentWithStyle
  Object
  (render [this]
    (dom/div #js {:style styles})))

(deftest test-format-styles
  (is (= (dom/format-styles (select-keys styles [:textAlign])) "text-align:center;"))
  (is (= (dom/format-styles styles) "text-align:center;margin-left:10px;"))
  (is (= (dom/format-styles {:zoom 1}) "zoom:1;"))
  (is (= (dom/format-styles {:zoom 1
                             :opacity 0.5
                             :width 100}) "zoom:1;opacity:0.5;width:100px;")))



(deftest test-render-component-with-style
  (let [ctor (cellophane/factory ComponentWithStyle)]
    (is (= (dom/render-to-str (ctor))
          "<div data-reactid=\".0\" style=\"text-align:center;margin-left:10px;\"></div>"))))

;; ===================================================================
;; Checksums, react-ids

(deftest test-checksums
  (are [markup chk] (= (dom/checksum markup) chk)
    "<div data-reactid=\".18h0b0ubv28\"></div>" 34999398))
