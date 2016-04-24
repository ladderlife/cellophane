(ns cellophane.protocols)

(defprotocol IReactDOMElement
  (^String -render-to-string [this sb] "renders a DOM node to string."))

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
