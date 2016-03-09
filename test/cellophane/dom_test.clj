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
  (let [c ((cellophane/factory SimpleComponent))]
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

(deftest test-format-react-attrs
  (are [map res] (= (dom/render-attr-map map) res)
    {:htmlFor "something"} " for=\"something\""
    {:className "foo"} " class=\"foo\""
    {:srcLang "en"} " srclang=\"en\""
    {:acceptCharset "ISO-8859-1"} " accept-charset=\"ISO-8859-1\""
    {:placeholder "Title"} " placeholder=\"Title\""))

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

;; Om animals tutorial

(def animals-state
  {:app/title "Animals"
   :animals/list
   [[1 "Ant"] [2 "Antelope"] [3 "Bird"] [4 "Cat"] [5 "Dog"]
    [6 "Lion"] [7 "Mouse"] [8 "Monkey"] [9 "Snake"] [10 "Zebra"]]})

(defui AnimalsList
  static cellophane/IQueryParams
  (params [this]
    {:start 0 :end 10})
  static cellophane/IQuery
  (query [this]
    '[:app/title (:animals/list {:start ?start :end ?end})])
  Object
  (render [this]
    (let [{:keys [app/title animals/list]} (cellophane/props this)]
      (dom/div nil
        (dom/h2 nil title)
        (apply dom/ul nil
          (map
            (fn [[i name]]
              (dom/li nil (str i ". " name)))
            list))))))

(deftest test-render-animals-tutorial
  (let [ctor (cellophane/factory AnimalsList)]
    (is (= (dom/render-to-str (ctor animals-state))
           (remove-whitespace "<div data-reactid=\".0\">
                                 <h2 data-reactid=\".0.0\">Animals</h2>
                                 <ul data-reactid=\".0.1\">
                                   <li data-reactid=\".0.1.0\">1. Ant</li>
                                   <li data-reactid=\".0.1.1\">2. Antelope</li>
                                   <li data-reactid=\".0.1.2\">3. Bird</li>
                                   <li data-reactid=\".0.1.3\">4. Cat</li>
                                   <li data-reactid=\".0.1.4\">5. Dog</li>
                                   <li data-reactid=\".0.1.5\">6. Lion</li>
                                   <li data-reactid=\".0.1.6\">7. Mouse</li>
                                   <li data-reactid=\".0.1.7\">8. Monkey</li>
                                   <li data-reactid=\".0.1.8\">9. Snake</li>
                                   <li data-reactid=\".0.1.9\">10. Zebra</li>
                                 </ul>
                               </div>")))))

;; Simple nested `defui`s

(defui SimpleNestedChild
  Object
  (render [this]
    (dom/div nil "child")))

(def simple-nested-child-factory (cellophane/factory SimpleNestedChild))

(defui SimpleNestedParent
  Object
  (render [this]
    (dom/div nil
      (simple-nested-child-factory))))

(deftest test-simple-nested-defuis
  (let [ctor (cellophane/factory SimpleNestedParent)]
    (is (= (dom/render-to-str (ctor))
           (remove-whitespace "<div data-reactid=\".0\">
                                 <div data-reactid=\".0.0\">child</div>
                               </div>")))))


;; Om Simple Recursive Tree

(declare simple-node)

(defui SimpleNode
  static cellophane/IQuery
  (query [this]
    '[:node-value {:children ...}])
  Object
  (render [this]
    (let [{:keys [node-value children]} (cellophane/props this)]
      (dom/li nil
        (dom/div nil (str "Node value:" node-value))
        (dom/ul nil
          (map simple-node children))))))

(def simple-node (cellophane/factory SimpleNode))

(defui SimpleTree
  static cellophane/IQuery
  (query [this]
    [{:tree (cellophane/get-query SimpleNode)}])
  Object
  (render [this]
    (let [{:keys [tree]} (cellophane/props this)]
      (dom/ul nil
        (simple-node tree)))))

(def simple-tree-data
  {:tree {:node-value 1
          :children [{:node-value 2
                      :children [{:node-value 3
                                  :children []}]}
                     {:node-value 4
                      :children []}]}})

(deftest test-render-simple-recursive-example
  (let [ctor (cellophane/factory SimpleTree)]
    (is (= (dom/render-to-str (ctor simple-tree-data))
          (remove-whitespace "<ul data-reactid=\".0\">
                                <li data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree]\">
                                  <div data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].0\">Node value:1</div>
                                  <ul data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1\">
                                    <li data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0]\">
                                      <div data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0].0\">Node value:2</div>
                                      <ul data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0].1\">
                                        <li data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0 =2children 0]\">
                                          <div data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0 =2children 0].0\">Node value:3</div>
                                          <ul data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 0 =2children 0].1\"></ul>
                                        </li>
                                      </ul>
                                    </li>
                                    <li data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 1]\">
                                      <div data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 1].0\">Node value:4</div>
                                      <ul data-reactid=\".0.$cellophane$dom_test$SimpleNode_[=2tree].1.$cellophane$dom_test$SimpleNode_[=2tree =2children 1].1\"></ul>
                                    </li>
                                  </ul>
                                </li>
                              </ul>")))))

(defn MultipleTextChildren []
  (dom/div nil
    "Some text"
    "More text"))

(defn ChildAndText []
  (dom/div nil
    (dom/p nil "A paragraph!")
    "More text"))

(deftest test-render-multiple-text-children
  (testing "rendering an element with multiple children converts text nodes to <span>"
    (are [comp res] (= (dom/render-element (dom/assign-react-ids (comp)))
                      (remove-whitespace res))
      MultipleTextChildren "<div data-reactid=\".0\">
                              <span data-reactid=\".0.0\">Some text</span>
                              <span data-reactid=\".0.1\">More text</span>
                            </div>"
      ChildAndText "<div data-reactid=\".0\">
                      <p data-reactid=\".0.0\">A paragraph!</p>
                      <span data-reactid=\".0.1\">More text</span>
                    </div>")))

;; ===================================================================
;; Checksums, react-ids

(deftest test-checksums
  (are [markup chk] (= (dom/checksum markup) chk)
    "<div data-reactid=\".18h0b0ubv28\"></div>" 34999398))
