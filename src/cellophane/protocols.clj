(ns cellophane.protocols)

(defprotocol ReactDOMElement
  (-children [this] "returns the element's children")
  (-render-to-string [this] "renders a DOM node to string."))

(defprotocol ReactComponent
  (-render [this] "must return a valid ReactDOMElement."))

(defprotocol ReactLifecycle
  (initLocalState [this])
  (componentWillReceiveProps [this next-props])
  (componentWillUpdate [this next-props next-state])
  (componentDidUpdate [this prev-props prev-state])
  (componentWillMount [this])
  (componentDidMount [this])
  (componentWillUnmount [this])
  (render [this]))
