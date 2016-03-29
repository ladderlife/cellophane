(ns cellophane.protocols)

(defprotocol IReactDOMElement
  (-render-to-string [this] "renders a DOM node to string."))

(defprotocol IReactChildren
  (-children [this] "returns the element's children"))

(defprotocol IReactComponent
  (-render [this] "must return a valid ReactDOMElement."))

(defprotocol IReactLifecycle
  (shouldComponentUpdate [this next-props next-state])
  (initLocalState [this])
  (componentWillReceiveProps [this next-props])
  (componentWillUpdate [this next-props next-state])
  (componentDidUpdate [this prev-props prev-state])
  (componentWillMount [this])
  (componentDidMount [this])
  (componentWillUnmount [this])
  (render [this]))

(defprotocol IIndexer
  (indexes [this])
  (index-root [this x])
  (index-component! [this component])
  (drop-component! [this component])
  (ref-for [this component])
  (key->components [this k]))

(defprotocol IReconciler
  (basis-t [this])
  (add-root! [reconciler root-class target options])
  (remove-root! [reconciler target])
  (schedule-render! [reconciler])
  (schedule-sends! [reconciler])
  (queue! [reconciler ks])
  (queue-sends! [reconciler sends])
  (reindex! [reconciler])
  (reconcile! [reconciler])
  (send! [reconciler]))
