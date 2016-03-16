# cellophane [![Circle CI](https://circleci.com/gh/ladderlife/cellophane.svg?style=svg)](https://circleci.com/gh/ladderlife/cellophane)

Server-side rendering for Om Next components


## Usage

Cellophane provides a Clojure implementation of Om Next, which is just enough to get components to render server-side into an HTML string that can be picked up by React on the browser.

If you want to take full advantage of server-side rendering, you need to:

1. Port your component code to `.cljc`
2. Port your client parser code to `.cljc`
3. Require Cellophane in the `:clj` branches of your namespaces, as shown below.
4. Use a different `send` function in the Cellophane reconciler. [Here's](https://github.com/ladderlife/cellophane/blob/18f39/fullstack_example/src/shared/todomvc/todomvc.cljc#L78-L84) an example for projects using the Om Next parser server-side.


```clojure
(:require #?@(:cljs [[om.next :as om :refer-macros [defui]]
                     [om.dom :as dom]]
              :clj  [[cellophane.next :as om :refer [defui]]
                     [cellophane.dom :as dom]]))
```

There's a full stack TodoMVC example with server-side rendering in [fullstack_example](./fullstack_example).

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
