(ns cellophane.protocols)

(defprotocol IReactDOMElement
  (-children [this] "returns the element's children")
  (-render-to-string [this] "renders a DOM node to string."))

(defprotocol IReactComponent
  (-render [this] "must return a valid ReactDOMElement."))

(defprotocol IReactLifecycle
  (initLocalState [this])
  (componentWillReceiveProps [this next-props])
  (componentWillUpdate [this next-props next-state])
  (componentDidUpdate [this prev-props prev-state])
  (componentWillMount [this])
  (componentDidMount [this])
  (componentWillUnmount [this])
  (render [this]))
