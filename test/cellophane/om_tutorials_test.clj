(ns cellophane.om-tutorials-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.test-utils :refer [remove-whitespace]]
            [cellophane.next :as cellophane :refer [defui]]
            [cellophane.dom :as dom]))

;; =============================================================================
;; Quick Start

(def animals-app-state
  (atom
    {:app/title "Animals"
     :animals/list
     [[1 "Ant"] [2 "Antelope"] [3 "Bird"] [4 "Cat"] [5 "Dog"]
      [6 "Lion"] [7 "Mouse"] [8 "Monkey"] [9 "Snake"] [10 "Zebra"]]}))

(defmulti animals-read (fn [env key params] key))

(defmethod animals-read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defmethod animals-read :animals/list
  [{:keys [state] :as env} key {:keys [start end]}]
  {:value (subvec (:animals/list @state) start end)})

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

(def animals-reconciler
  (cellophane/reconciler
    {:state animals-app-state
     :parser (cellophane/parser {:read animals-read})}))

(deftest test-render-animals-tutorial
  (let [result-markup (remove-whitespace
                        "<div data-reactid=\".0\">
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
                         </div>")]
    (testing "render with factory"
      (let [ctor (cellophane/factory AnimalsList)]
        (is (= (dom/render-to-str (ctor @animals-app-state)) result-markup))))
    (testing "render with reconciler & add-root!"
      (let [c (cellophane/add-root! animals-reconciler AnimalsList nil)
            markup-str (dom/render-to-str c)]
        (is (= (class (cellophane/app-root animals-reconciler)) AnimalsList))
        (is (= markup-str result-markup))))))

;; Om Links tutorial

(def links-init-data
  {:current-user {:email "bob.smith@gmail.com"}
   :items [{:id 0 :title "Foo"}
           {:id 1 :title "Bar"}
           {:id 2 :title "Baz"}]})

(defmulti links-read cellophane/dispatch)

(defmethod links-read :items
  [{:keys [query state]} k _]
  (let [st @state]
    {:value (cellophane/db->tree query (get st k) st)}))

(defui LinksItem
  static cellophane/Ident
  (ident [_ {:keys [id]}]
    [:item/by-id id])
  static cellophane/IQuery
  (query [_]
    '[:id :title [:current-user _]])
  Object
  (render [this]
    (let [{:keys [title current-user]} (cellophane/props this)]
      (dom/li nil
        (dom/div nil title)
        (dom/div nil (:email current-user))))))

(def links-item (cellophane/factory LinksItem))

(defui LinksSomeList
  static cellophane/IQuery
  (query [_]
    [{:items (cellophane/get-query LinksItem)}])
  Object
  (render [this]
    (dom/div nil
      (dom/h2 nil "A List!")
      (dom/ul nil
        (map links-item (-> this cellophane/props :items))))))

(def links-reconciler
  (cellophane/reconciler
    {:state links-init-data
     :parser (cellophane/parser {:read links-read})}))

(deftest test-render-links-tutorial
  (let [c (cellophane/add-root! links-reconciler LinksSomeList nil)]
    (is (= (dom/render-to-str c)
          (remove-whitespace
            "<div data-reactid=\".0\">
               <h2 data-reactid=\".0.0\">A List!</h2>
               <ul data-reactid=\".0.1\">
                 <li data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 0]\">
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 0].0\">Foo</div>
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 0].1\">bob.smith@gmail.com</div>
                 </li>
                 <li data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 1]\">
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 1].0\">Bar</div>
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 1].1\">bob.smith@gmail.com</div>
                 </li>
                 <li data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 2]\">
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 2].0\">Baz</div>
                   <div data-reactid=\".0.1.$cellophane$om_tutorials_test$LinksItem_[=2items 2].1\">bob.smith@gmail.com</div>
                 </li>
               </ul>
             </div>")))))

