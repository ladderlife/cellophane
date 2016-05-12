# cellophane [![Circle CI](https://circleci.com/gh/ladderlife/cellophane.svg?style=svg)](https://circleci.com/gh/ladderlife/cellophane)

Server-side rendering for Om Next components

## Contents

- [Installation](#installation)
- [Guide](#guide)
  - [Getting started](#getting-started)
  - [Rendering components server-side](#rendering-components-server-side)
    - [Simple case (no reconciler)](#simple-case-no-reconciler)
    - [Full-blown case](#full-blown-case)
- [Limitations](#limitations)
- [Copyright & License](#copyright--license)


## Installation

Leiningen dependency information:

[![Clojars Project](https://clojars.org/com.ladderlife/cellophane/latest-version.svg)](https://clojars.org/com.ladderlife/cellophane)


Maven dependency information:

```xml
<dependency>
  <groupId>com.ladderlife</groupId>
  <artifactId>cellophane</artifactId>
  <version>0.1.0</version>
</dependency>
```

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
;; => "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"1632637923\">Hello, world!</div>"

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

;; => "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"-1712681713\">
;;       <h2 data-reactid=\"2\">Animals</h2>
;;       <ul data-reactid=\"3\">
;;         <li data-reactid=\"4\">1. Ant</li>
;;         <li data-reactid=\"5\">2. Antelope</li>
;;         <li data-reactid=\"6\">3. Bird</li>
;;         <li data-reactid=\"7\">4. Cat</li>
;;         <li data-reactid=\"8\">5. Dog</li>
;;         <li data-reactid=\"9\">6. Lion</li>
;;         <li data-reactid=\"10\">7. Mouse</li>
;;         <li data-reactid=\"11\">8. Monkey</li>
;;         <li data-reactid=\"12\">9. Snake</li>
;;         <li data-reactid=\"13\">10. Zebra</li>
;;       </ul>
;;     </div>"

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
