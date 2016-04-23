(ns cellophane.dom-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.test-utils :refer [remove-whitespace]]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.dom :as dom]))

(defn test-tags [tags res-fn]
  `(are [element# res#] (= (dom/render-element {:tag element# :react-id 1}) res#)
     ~@(mapcat (fn [tag#] [tag# (res-fn tag#)]) tags)))

(defmacro test-container-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(dom/container-tag? % nil)))]
    (test-tags container-tags #(str "<" % " data-reactid=\"1\">" "</" % ">"))))

(defmacro test-void-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(not (dom/container-tag? % nil))))]
    ;; TODO: should we add mode for XHTML in which tags need to have a
    ;; closing slash? e.g. "<input/>" vs "<input>"
    (test-tags container-tags #(str "<" % " data-reactid=\"1\">"))))

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
      (simple-component) "<div data-reactroot=\"\" data-reactid=\"1\">Hello World</div>"
      (simple-nested-component) (remove-whitespace
                                  "<div data-reactroot=\"\" data-reactid=\"1\">
                                     <h1 id=\"page-title\" data-reactid=\"2\">Title</h1>
                                   </div>")
      (comp-nested-component) (remove-whitespace
                                "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <div data-reactid=\"2\">Hello World</div>
                                   <div data-reactid=\"3\">
                                     <h1 id=\"page-title\" data-reactid=\"4\">Title</h1>
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
    (is (= (#'dom/render-to-str* c) "<div data-reactroot=\"\" data-reactid=\"1\">Hello World</div>")))
  (let [hello (cellophane/factory Hello)]
    (is (= (#'dom/render-to-str* (hello {:text "Hello, world!"}))
           "<p data-reactroot=\"\" data-reactid=\"1\">Hello, world!</p>")))
  (let [children (cellophane/factory Children)]
    (is (= (#'dom/render-to-str* (children))
          (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                  <div data-reactid=\"2\">Foo</div>
                                  <div data-reactid=\"3\">Bar</div>
                                  <div data-reactid=\"4\">Bar</div>
                                  <div data-reactid=\"5\">Woz</div>
                              </div>")))))

(deftest test-format-react-attrs
  (are [map res] (= (dom/render-attr-map map) res)
    {:htmlFor "something"} " for=\"something\""
    {:className "foo"} " class=\"foo\""
    {:srcLang "en"} " srclang=\"en\""
    {:acceptCharset "ISO-8859-1"} " accept-charset=\"ISO-8859-1\""
    {:placeholder "Title"} " placeholder=\"Title\""
    ;; svg xlink:stuff
    {:xlinkActuate "foo"} " xlink:actuate=\"foo\""))

(deftest test-ref-is-elided-in-props
  (is (= (dom/render-element
           (dom/assign-react-ids
             (dom/div #js {:ref "someDiv"})))
         "<div data-reactroot=\"\" data-reactid=\"1\"></div>")))

(deftest test-attrs-rendered-in-declaration-order
  (are [element res] (= (dom/render-element (dom/assign-react-ids element)) res)
    (dom/input {:type "text"
                :placeholder "some text"
                :id "stuff"})
    "<input type=\"text\" placeholder=\"some text\" id=\"stuff\" data-reactroot=\"\" data-reactid=\"1\">"

    (dom/input {:id "stuff"
                :placeholder "some text"
                :type "text"})
    "<input id=\"stuff\" placeholder=\"some text\" type=\"text\" data-reactroot=\"\" data-reactid=\"1\">"

    (dom/input {:placeholder "some text"
                :id "stuff"
                :type "text"})
    "<input placeholder=\"some text\" id=\"stuff\" type=\"text\" data-reactroot=\"\" data-reactid=\"1\">"))

(deftest test-only-supported-attrs-rendered
  (are [element markup] (= (#'dom/render-to-str* element) (remove-whitespace markup))
    (dom/div #js {:not-supported "foo"}) "<div data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:className "stuff" :class "other"}) "<div class=\"stuff\" data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:media :stuff}) "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))

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
         "<div data-reactroot=\"\" data-reactid=\"1\"></div>")))

(deftest test-render-component-with-style
  (let [ctor (cellophane/factory ComponentWithStyle)]
    (is (= (#'dom/render-to-str* (ctor))
          "<div style=\"text-align:center;margin-left:10px;\" data-reactroot=\"\" data-reactid=\"1\"></div>"))))

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
    (is (= (#'dom/render-to-str* (ctor))
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <div data-reactid=\"2\">child</div>
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
    (is (= (#'dom/render-to-str* c)
          (remove-whitespace
            "<ul data-reactroot=\"\" data-reactid=\"1\">
                 <li data-reactid=\"2\">
                   <div data-reactid=\"3\">Node value:1</div>
                   <ul data-reactid=\"4\">
                     <li data-reactid=\"5\">
                       <div data-reactid=\"6\">Node value:2</div>
                       <ul data-reactid=\"7\">
                         <li data-reactid=\"8\">
                           <div data-reactid=\"9\">Node value:3</div>
                           <ul data-reactid=\"10\"></ul>
                         </li>
                       </ul>
                     </li>
                     <li data-reactid=\"11\">
                       <div data-reactid=\"12\">Node value:4</div>
                       <ul data-reactid=\"13\"></ul>
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
      MultipleTextChildren "<div data-reactroot=\"\" data-reactid=\"1\">
                              <!-- react-text: 2 -->Some text<!-- /react-text -->
                              <!-- react-text: 3 -->More text<!-- /react-text -->
                            </div>"
      ChildAndText "<div data-reactroot=\"\" data-reactid=\"1\">
                      <p data-reactid=\"2\">A paragraph!</p>
                      <!-- react-text: 3 -->More text<!-- /react-text -->
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
    (is (= (#'dom/render-to-str* c)
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <h3 data-reactid=\"2\">Props: {:counter 0}</h3>
                                   <h3 data-reactid=\"3\">Shared: {:counter 0}</h3>
                                   <button data-reactid=\"4\">Increment!</button>
                               </div>")))))

(deftest test-render-to-str-elements
  (are [elem res] (= (#'dom/render-to-str* elem) res)
    (dom/div nil "foo") "<div data-reactroot=\"\" data-reactid=\"1\">foo</div>"))

(deftest react-key-in-elements
  (is (= (:react-key (dom/div {:key "foo"})) "foo"))
  (is (= (:attrs (dom/div {:key "foo"})) {}))
  (is (= (:react-key (dom/div nil)) nil))
  (is (= (#'dom/render-to-str* (dom/div {:key "foo"}))
        "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))
  (is (= (#'dom/render-to-str* (dom/div nil (dom/div #js {:key "foo"})))
        "<div data-reactroot=\"\" data-reactid=\"1\"><div data-reactid=\"2\"></div></div>")))

(deftest test-non-string-attributes
  (is (= (#'dom/render-to-str* (dom/div {:className 3}))
        "<div class=\"3\" data-reactroot=\"\" data-reactid=\"1\"></div>")))

(defui NilChild
  Object
  (render [this]
    nil))

(def nil-child-factory (cellophane/factory NilChild))

(defui NilParent
  Object
  (render [this]
    (dom/div nil
      "foo"
      (nil-child-factory))))

(deftest test-nil-children
  (is (= (#'dom/render-to-str* (nil-child-factory))
         "<noscript data-reactid=\".0\"></noscript>"))
  (is (= (#'dom/render-to-str* ((cellophane/factory NilParent)))
        (remove-whitespace "<div data-reactid=\".0\"><span data-reactid=\".0.0\">foo</span>
                                <noscript data-reactid=\".0.1\"></noscript>
                            </div>"))))

(defui CLPHN-3-Component-1
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil (str "a: " (cellophane/get-state this :a)))))

(defui CLPHN-3-Component-2
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (componentDidMount [this]
    (cellophane/update-state! this update-in [:a] * 10))
  (render [this]
    (dom/div nil (str "a: " (cellophane/get-state this :a)))))

(defui CLPHN-3-Child
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (componentDidMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil (str "child a: " (cellophane/get-state this :a)))))

(def clphn3-child (cellophane/factory CLPHN-3-Child))

(defui CLPHN-3-Parent
  Object
  (initLocalState [this]
    {:a 2})
  (componentWillMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (componentDidMount [this]
    (cellophane/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil
      (clphn3-child)
      (str "parent a: " (cellophane/get-state this :a)))))

(deftest test-clphn-3
  (let [c1 ((cellophane/factory CLPHN-3-Component-1))
        c2 ((cellophane/factory CLPHN-3-Component-2))
        c3 ((cellophane/factory CLPHN-3-Parent))]
    (is (= (#'dom/render-to-str* c1)
           "<div data-reactroot=\"\" data-reactid=\"1\">a: 2</div>"))
    (is (= (#'dom/render-to-str* c2)
           "<div data-reactroot=\"\" data-reactid=\"1\">a: 20</div>"))
    (is (= (#'dom/render-to-str* c3)
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                 <div data-reactid=\"2\">child a: 3</div>
                                 <!-- react-text: 3 -->parent a: 4<!-- /react-text -->
                               </div>")))))

(defui SomeChild
  Object
  (render [this]
    (dom/div nil "foo")))

(def some-child (cellophane/factory SomeChild))

(defui SomeParent
  Object
  (render [this]
    (some-child)))

(deftest test-om-644
  (is (= (#'dom/render-to-str* ((cellophane/factory SomeParent)))
         "<div data-reactroot=\"\" data-reactid=\"1\">foo</div>")))

(defui NilChildrenComp
  Object
  (render [this]
    (dom/div #js {}
      nil)))

(deftest test-nil-children
  (is (= (#'dom/render-to-str* ((cellophane/factory NilChildrenComp)))
         "<div data-reactroot=\"\" data-reactid=\"1\"></div>")))

;; React 15

(defui React15Comp
  Object
  (render [this]
    (dom/div nil
      (dom/div nil
        "nested"
        (dom/div nil "other")))))

(deftest react-15-render
  (is (= (dom/render-to-str ((cellophane/factory React15Comp)))
        (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"1635398171\">
                              <div data-reactid=\"2\">
                                <!-- react-text: 3 -->nested<!-- /react-text -->
                                <div data-reactid=\"4\">other</div>
                              </div>
                            </div>")))
  (is (= (dom/render-to-str
           (dom/div nil
             (dom/div nil
               (dom/div nil "3"))
             (dom/div nil
               (dom/div nil "5"))))
        (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"-1239993276\">
                              <div data-reactid=\"2\">
                                <div data-reactid=\"3\">3</div>
                              </div>
                              <div data-reactid=\"4\">
                                <div data-reactid=\"5\">5</div>
                              </div>
                            </div>"))))
