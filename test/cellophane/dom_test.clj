(ns cellophane.dom-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.test-utils :refer [remove-whitespace]]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.dom :as dom]))

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
                                     <h1 id=\"page-title\" data-reactid=\".0.0\">Title</h1>
                                   </div>")
      (comp-nested-component) (remove-whitespace
                                "<div data-reactid=\".0\">
                                   <div data-reactid=\".0.0\">Hello World</div>
                                   <div data-reactid=\".0.1\">
                                     <h1 id=\"page-title\" data-reactid=\".0.1.0\">Title</h1>
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
    (is (= (dom/render-to-str c) "<div><div data-reactid=\".0\">Hello World</div></div>")))
  (let [hello (cellophane/factory Hello)]
    (is (= (dom/render-to-str (hello {:text "Hello, world!"}))
           "<div><p data-reactid=\".0\">Hello, world!</p></div>")))
  (let [children (cellophane/factory Children)]
    (is (= (dom/render-to-str (children))
          (remove-whitespace "<div>
                                <div data-reactid=\".0\">
                                  <div data-reactid=\".0.0\">Foo</div>
                                  <div data-reactid=\".0.1\">Bar</div>
                                  <div data-reactid=\".0.2\">Bar</div>
                                  <div data-reactid=\".0.3\">Woz</div>
                                </div>
                              </div>")))))

(deftest test-format-react-attrs
  (are [map res] (= (dom/render-attr-map map) res)
    {:htmlFor "something"} " for=\"something\""
    {:className "foo"} " class=\"foo\""
    {:srcLang "en"} " srclang=\"en\""
    {:acceptCharset "ISO-8859-1"} " accept-charset=\"ISO-8859-1\""
    {:placeholder "Title"} " placeholder=\"Title\""))

(deftest test-ref-is-elided-in-props
  (is (= (dom/render-element
           (dom/assign-react-ids
             (dom/div #js {:ref "someDiv"})))
         "<div data-reactid=\".0\"></div>")))

(deftest test-attrs-rendered-in-declaration-order
  (are [element res] (= (dom/render-element (dom/assign-react-ids element)) res)
    (dom/input {:type "text"
                :placeholder "some text"
                :id "stuff"})
    "<input type=\"text\" placeholder=\"some text\" id=\"stuff\" data-reactid=\".0\">"

    (dom/input {:id "stuff"
                :placeholder "some text"
                :type "text"})
    "<input id=\"stuff\" placeholder=\"some text\" type=\"text\" data-reactid=\".0\">"

    (dom/input {:placeholder "some text"
                :id "stuff"
                :type "text"})
    "<input placeholder=\"some text\" id=\"stuff\" type=\"text\" data-reactid=\".0\">"))

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

(deftest test-empty-styles-not-rendered
  (is (= (dom/render-element (dom/assign-react-ids (dom/div {:style {}})))
         "<div data-reactid=\".0\"></div>")))

(deftest test-render-component-with-style
  (let [ctor (cellophane/factory ComponentWithStyle)]
    (is (= (dom/render-to-str (ctor))
          "<div><div style=\"text-align:center;margin-left:10px;\" data-reactid=\".0\"></div></div>"))))

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
           (remove-whitespace "<div>
                                 <div data-reactid=\".0\">
                                   <div data-reactid=\".0.0\">child</div>
                                 </div>
                               </div>")))))


;; Om Simple Recursive Tree
(def simple-tree-data
  {:tree {:node-value 1
          :children [{:node-value 2
                      :children [{:node-value 3
                                  :children []}]}
                     {:node-value 4
                      :children []}]}})

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

(defmulti simple-tree-read cellophane/dispatch)

(defmethod simple-tree-read :node-value
  [{:keys [data] :as env} _ _]
  {:value (:node-value data)})

(defmethod simple-tree-read :children
  [{:keys [data parser query] :as env} _ _]
  {:value (let [f #(parser (assoc env :data %) query)]
            (into [] (map f (:children data))))})

(defmethod simple-tree-read :tree
  [{:keys [state parser query] :as env} k _]
  (let [st @state]
    {:value (parser (assoc env :data (:tree st)) query)}))

(def simple-tree-reconciler
  (cellophane/reconciler
    {:state     (atom simple-tree-data)
     :normalize false
     :parser    (cellophane/parser {:read simple-tree-read})}))

(deftest test-render-simple-recursive-example
  (let [c (cellophane/add-root! simple-tree-reconciler SimpleTree nil)]
    (is (= (dom/render-to-str c)
          (remove-whitespace
            "<div>
               <ul data-reactid=\".0\">
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
               </ul>
             </div>")))))

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

;; Shared test

(defui Home
  static cellophane/IQuery
  (query [this] [:counter])

  Object
  (render [this]
    (let [shared (cellophane/shared this)
          props  (cellophane/props this)]
      (dom/div nil
        (dom/h3 nil (str "Props: " props))
        (dom/h3 nil (str "Shared: " shared))
        (dom/button
          #js {:onClick #(cellophane/transact! this '[(my/test) :counter])}
          "Increment!")))))

(def app-state (atom {:counter 0}))

(defn read
  [env key params]
  (let [{:keys [state]} env]
    {:value (get @state key)}))

(defn mutate
  [env key params]
  (let [{:keys [state]} env]
    {:value  {:keys [:counter]}
     :action #(swap! state update-in [:counter] inc)}))

(def reconciler
  (cellophane/reconciler
    {:state     app-state
     :parser    (cellophane/parser {:read read :mutate mutate})
     :shared    {}
     :shared-fn (fn [root-props]
                  root-props)}))

(deftest test-shared
  (let [c (cellophane/add-root! reconciler Home nil)]
    (is (= (dom/render-to-str c)
           (remove-whitespace "<div>
                                 <div data-reactid=\".0\">
                                   <h3 data-reactid=\".0.0\">Props: {:counter 0}</h3>
                                   <h3 data-reactid=\".0.1\">Shared: {:counter 0}</h3>
                                   <button data-reactid=\".0.2\">Increment!</button>
                                 </div>
                               </div>")))
    (is (= (cellophane/force-root-render! reconciler) nil))))

(deftest test-render-to-str-elements
  (are [elem res] (= (dom/render-to-str elem) res)
    (dom/div nil "foo") "<div><div data-reactid=\".0\">foo</div></div>"))
