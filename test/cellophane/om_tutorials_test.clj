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

;; =============================================================================
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

;; =============================================================================
;; Componentes, Identity & Normalization

(def cian-init-data
  {:list/one [{:name "John" :points 0}
              {:name "Mary" :points 0}
              {:name "Bob"  :points 0}]
   :list/two [{:name "Mary" :points 0 :age 27}
              {:name "Gwen" :points 0}
              {:name "Jeff" :points 0}]})

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti cian-read cellophane/dispatch)

(defn get-people [state key]
  (let [st @state]
    (into [] (map #(get-in st %)) (get st key))))

(defmethod cian-read :list/one
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmethod cian-read :list/two
  [{:keys [state] :as env} key params]
  {:value (get-people state key)})

(defmulti cian-mutate cellophane/dispatch)

(defmethod cian-mutate 'points/increment
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
       [:person/by-name name :points]
       inc))})

(defmethod cian-mutate 'points/decrement
  [{:keys [state]} _ {:keys [name]}]
  {:action
   (fn []
     (swap! state update-in
       [:person/by-name name :points]
       #(let [n (dec %)] (if (neg? n) 0 n))))})

;; -----------------------------------------------------------------------------
;; Components

(defui Person
  static cellophane/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static cellophane/IQuery
  (query [this]
    '[:name :points :age])
  Object
  (render [this]
    (println "Render Person" (-> this cellophane/props :name))
    (let [{:keys [points name foo] :as props} (cellophane/props this)]
      (dom/li nil
        (dom/label nil (str name ", points: " points))
        (dom/button
          #js {:onClick
               (fn [e]
                 (cellophane/transact! this
                   `[(points/increment ~props)]))}
          "+")
        (dom/button
          #js {:onClick
               (fn [e]
                 (cellophane/transact! this
                   `[(points/decrement ~props)]))}
          "-")))))

(def person (cellophane/factory Person {:keyfn :name}))

(defui ListView
  Object
  (render [this]
    ;(println "Render ListView" (-> this cellophane/path first))
    (let [list (cellophane/props this)]
      (apply dom/ul nil
        (map person list)))))

(def list-view (cellophane/factory ListView))

(defui RootView
  static cellophane/IQuery
  (query [this]
    (let [subquery (cellophane/get-query Person)]
      `[{:list/one ~subquery} {:list/two ~subquery}]))
  Object
  (render [this]
    (println "Render RootView")
    (let [{:keys [list/one list/two]} (cellophane/props this)]
      (apply dom/div nil
        [(dom/h2 nil "List A")
         (list-view one)
         (dom/h2 nil "List B")
         (list-view two)]))))

(def cian-reconciler
  (cellophane/reconciler
    {:state  cian-init-data
     :parser (cellophane/parser {:read cian-read :mutate cian-mutate})}))

(deftest test-cian-tutorial
  (let [c (cellophane/add-root! cian-reconciler RootView nil)]
    (is (= (dom/render-to-str c)
           (remove-whitespace "<div data-reactid=\".0\">
                                 <h2 data-reactid=\".0.0\">List A</h2>
                                 <ul data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one]\">
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$John\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$John.0\">John, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$John.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$John.2\">-</button>
                                   </li>
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Mary\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Mary.0\">Mary, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Mary.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Mary.2\">-</button>
                                   </li>
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Bob\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Bob.0\">Bob, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Bob.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/one].$Bob.2\">-</button>
                                   </li>
                                 </ul>
                                 <h2 data-reactid=\".0.2\">List B</h2>
                                 <ul data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two]\">
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Mary\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Mary.0\">Mary, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Mary.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Mary.2\">-</button>
                                   </li>
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Gwen\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Gwen.0\">Gwen, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Gwen.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Gwen.2\">-</button>
                                   </li>
                                   <li data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Jeff\">
                                     <label data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Jeff.0\">Jeff, points: 0</label>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Jeff.1\">+</button>
                                     <button data-reactid=\".0.$cellophane$om_tutorials_test$ListView_[=2list/two].$Jeff.2\">-</button>
                                   </li>
                                 </ul>
                               </div>")))))


