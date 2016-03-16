# cellophane [![Circle CI](https://circleci.com/gh/ladderlife/cellophane.svg?style=svg)](https://circleci.com/gh/ladderlife/cellophane)

Server-side rendering for Om Next components

## Installation

Add Cellophane to your dependencies:



## Guide

### Getting started

Cellophane provides a Clojure implementation of Om Next, which is just enough to get components to render server-side into an HTML string that can be picked up by React on the browser.

If you want to take full advantage of server-side rendering, you need to:

1. Port your component code to `.cljc`
2. Port your client parser code to `.cljc`
3. Require Cellophane in the `:clj` branches of your namespaces, as shown below.
4. Use a different `send` function in the Cellophane reconciler. [Here's](https://github.com/ladderlife/cellophane/blob/18f39/fullstack_example/src/shared/todomvc/todomvc.cljc#L78-L84) an example for projects using the Om Next parser server-side.


```clojure
(ns my-project.core
  (:require #?@(:cljs [[om.next :as om :refer-macros [defui]]
                       [om.dom :as dom]]
                :clj  [[cellophane.next :as om :refer [defui]]
                       [cellophane.dom :as dom]])))
```

### Rendering components server-side

Server-side rendering your components is very similar to what one would do in Om Next.

#### Simple case (no reconciler)

The simplest case is to feed `dom/render-to-str` a component instance created with a factory. Here's an example:

```clojure
(ns my-project.core
  (:require [cellophane.next :as om :refer [defui]]
            [cellophane.dom :as dom]))

(defui SimpleComponent
  Object
  (render [this]
    (dom/div nil "Hello, world!")))

(def simple-factory (om/factory SimpleComponent))

(dom/render-to-str (simple-factory))
;; => "<div><div data-reactid=\".0\">Hello, world!</div></div>"

```

#### Full-blown case

The full-blown case is shown below, and includes parser and reconciler code. The following code is directly taken from the Om Next Quick Start.

A full stack TodoMVC example with server-side rendering can be found in [fullstack_example](./fullstack_example).

```clojure
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

;; Server-side rendering:
(def component (cellophane/add-root! animals-reconciler AnimalsList nil))

(dom/render-to-str component)

;; => "<div>
;;      <div data-reactid=\".0\">
;;        <h2 data-reactid=\".0.0\">Animals</h2>
;;        <ul data-reactid=\".0.1\">
;;          <li data-reactid=\".0.1.0\">1. Ant</li>
;;          <li data-reactid=\".0.1.1\">2. Antelope</li>
;;          <li data-reactid=\".0.1.2\">3. Bird</li>
;;          <li data-reactid=\".0.1.3\">4. Cat</li>
;;          <li data-reactid=\".0.1.4\">5. Dog</li>
;;          <li data-reactid=\".0.1.5\">6. Lion</li>
;;          <li data-reactid=\".0.1.6\">7. Mouse</li>
;;          <li data-reactid=\".0.1.7\">8. Monkey</li>
;;          <li data-reactid=\".0.1.8\">9. Snake</li>
;;          <li data-reactid=\".0.1.9\">10. Zebra</li>
;;        </ul>
;;      </div>
;;    </div>"


```


## Limitations

Because Cellophane's `defui` generates Clojure records (which under the hood are Java classes), `:require`ing other namespaces is not enough to use those components. Using `:import` is also required, as demonstrated below:

```clojure
(ns my-ns
  (:require [other.ns :as other])
  (:import [other.ns Component]))
```


## Copyright & License

Copyright © 2016 Ladder Financial, Inc.

Distributed under the Eclipse Public License (see [LICENSE](./LICENSE)).

Contains adapted code from the following projects:

- [Om](https://github.com/omcljs/om)
- [Foam](https://github.com/arohner/foam)
