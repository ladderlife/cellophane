(ns cellophane.dom
  (:refer-clojure :exclude [map meta time use])
  (:require [clojure.string :as str]
            [cellophane.protocols :as p]
            [clojure.core.reducers :as r]
            [cellophane.checksums :as chk]))

;; ===================================================================
;; DOM render

(def tags
  '[a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    details
    dfn
    dialog
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    picture
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr

    ;; svg
    circle
    clipPath
    ellipse
    g
    line
    mask
    path
    pattern
    polyline
    rect
    svg
    text
    defs
    linearGradient
    polygon
    radialGradient
    stop
    tspan
    use

    ;; hand-generated by Om
    input
    textarea
    option
    select])

(def supported-attrs
  #{;; HTML
    "accept" "acceptCharset" "accessKey" "action" "allowFullScreen" "allowTransparency" "alt"
    "async" "autoComplete" "autoFocus" "autoPlay" "capture" "cellPadding" "cellSpacing" "challenge"
    "charSet" "checked" "classID" "className" "colSpan" "cols" "content" "contentEditable"
    "contextMenu" "controls" "coords" "crossOrigin" "data" "dateTime" "default" "defer" "dir"
    "disabled" "download" "draggable" "encType" "form" "formAction" "formEncType" "formMethod"
    "formNoValidate" "formTarget" "frameBorder" "headers" "height" "hidden" "high" "href" "hrefLang"
    "htmlFor" "httpEquiv" "icon" "id" "inputMode" "integrity" "is" "keyParams" "keyType" "kind" "label"
    "lang" "list" "loop" "low" "manifest" "marginHeight" "marginWidth" "max" "maxLength" "media"
    "mediaGroup" "method" "min" "minLength" "multiple" "muted" "name" "noValidate" "nonce" "open"
    "optimum" "pattern" "placeholder" "poster" "preload" "radioGroup" "readOnly" "rel" "required"
    "reversed" "role" "rowSpan" "rows" "sandbox" "scope" "scoped" "scrolling" "seamless" "selected"
    "shape" "size" "sizes" "span" "spellCheck" "src" "srcDoc" "srcLang" "srcSet" "start" "step" "style" "summary"
    "tabIndex" "target" "title" "type" "useMap" "value" "width" "wmode" "wrap"
    ;; RDF
    "about" "datatype" "inlist" "prefix" "property" "resource" "typeof" "vocab"
    ;; SVG
    "clipPath" "cx" "cy" "d" "dx" "dy" "fill" "fillOpacity" "fontFamily"
    "fontSize" "fx" "fy" "gradientTransform" "gradientUnits" "markerEnd"
    "markerMid" "markerStart" "offset" "opacity" "patternContentUnits"
    "patternUnits" "points" "preserveAspectRatio" "r" "rx" "ry" "spreadMethod"
    "stopColor" "stopOpacity" "stroke" "strokeDasharray" "strokeLinecap"
    "strokeOpacity" "strokeWidth" "textAnchor" "transform" "version"
    "viewBox" "x1" "x2" "x" "xlinkActuate" "xlinkArcrole" "xlinkHref" "xlinkRole"
    "xlinkShow" "xlinkTitle" "xlinkType" "xmlBase" "xmlLang" "xmlSpace" "y1" "y2" "y"

    ;; Special case
    "data-reactid" "data-reactroot"})

(def no-suffix
  #{"animationIterationCount" "boxFlex" "boxFlexGroup" "boxOrdinalGroup"
    "columnCount" "fillOpacity" "flex" "flexGrow" "flexPositive" "flexShrink"
    "flexNegative" "flexOrder" "fontWeight" "lineClamp" "lineHeight" "opacity"
    "order" "orphans" "stopOpacity" "strokeDashoffset" "strokeOpacity"
    "strokeWidth" "tabSize" "widows" "zIndex" "zoom"})

(def lower-case-attrs
  #{"accessKey" "allowFullScreen" "allowTransparency" "autoComplete"
    "autoFocus" "autoPlay" "contentEditable" "contextMenu" "crossOrigin"
    "cellPadding" "cellSpacing" "charSet" "classID" "colSpan" "dateTime"
    "encType" "formAction" "formEncType" "formMethod" "formNoValidate"
    "formTarget" "frameBorder" "hrefLang" "inputMode" "keyParams"
    "keyType" "marginHeight" "marginWidth" "maxLength" "mediaGroup"
    "minLength" "noValidate" "radioGroup" "readOnly" "rowSpan"
    "spellCheck" "srcDoc" "srcLang" "srcSet" "tabIndex" "useMap" })

(def kebab-case-attrs
  #{"acceptCharset" "httpEquiv" "fillOpacity" "fontFamily" "fontSize"
    "markerEnd" "markerMid" "markerStart" "stopColor" "stopOpacity"
    "strokeDasharray" "strokeLinecap" "strokeOpacity" "strokeWidth"
    "textAnchor"})

(def colon-between-attrs
  #{"xlinkActuate" "xlinkArcrole" "xlinkHref" "xlinkRole"
    "xlinkShow" "xlinkTitle" "xlinkType" "xmlBase" "xmlLang" "xmlSpace"})

(declare render-element!)

(defn append!
  ([^StringBuilder sb s0] (.append sb s0))
  ([^StringBuilder sb s0 s1]
   (.append sb s0)
   (.append sb s1))
  ([^StringBuilder sb s0 s1 s2]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2))
  ([^StringBuilder sb s0 s1 s2 s3]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2)
   (.append sb s3))
  ([^StringBuilder sb s0 s1 s2 s3 s4]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2)
   (.append sb s3)
   (.append sb s4))
  ([^StringBuilder sb s0 s1 s2 s3 s4 & rest]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2)
   (.append sb s3)
   (.append sb s4)
   (doseq [s rest]
     (.append sb s))))

(defrecord Element [tag attrs react-key children]
  p/IReactDOMElement
  (-render-to-string [this react-id sb]
    (render-element! this react-id sb))

  p/IReactChildren
  (-children [this] children))

(defrecord Text [s]
  p/IReactDOMElement
  (-render-to-string [this react-id sb]
    (assert (string? s))
    (append! sb s)))

(defrecord ReactText [text]
  p/IReactDOMElement
  (-render-to-string [this react-id sb]
    (assert (string? text))
    (append! sb "<!-- react-text: " @react-id " -->" text "<!-- /react-text -->")
    (vswap! react-id inc)))

(defrecord ReactEmpty []
  p/IReactDOMElement
  (-render-to-string [this react-id sb]
    (append! sb "<!-- react-empty: " @react-id " -->")
    (vswap! react-id inc)))

(defn text-node
  "HTML text node"
  [s]
  (map->Text {:s s}))

(defn react-text-node
  "HTML text node"
  [s]
  (map->ReactText {:text s}))

(defn- react-empty-node []
  (map->ReactEmpty {}))

(defn- render-component [c]
  (if (or (nil? c) (satisfies? p/IReactDOMElement c))
    c
    (recur (p/-render c))))

(defn element
  "Creates a dom node."
  [{:keys [tag attrs react-key children] :as elem}]
  (assert (name tag))
  (assert (or (nil? attrs) (map? attrs)) (format "elem %s attrs invalid" elem))
  (let [children (flatten children)
        child-node-count (count children)
        reduce-fn (if (> child-node-count 1)
                    r/reduce
                    reduce)
        children (reduce-fn
                   (fn [res c]
                     (let [c' (cond
                                (satisfies? p/IReactDOMElement c) c

                                (satisfies? p/IReactComponent c)
                                (let [rendered (if-let [element (render-component c)]
                                                 element
                                                 (react-empty-node))]
                                  (assoc rendered :react-key
                                    (some-> (p/-props c) :cellophaneclj$reactKey)))

                                (or (string? c) (number? c))
                                (let [c (cond-> c (number? c) str)]
                                  (if (> child-node-count 1)
                                    (react-text-node c)
                                    (text-node c)))
                                (nil? c) nil
                                :else (do
                                        (println "invalid child element:" c (class c))
                                        (assert false)))]
                       (cond-> res
                         (some? c') (conj c'))))
                   [] children)]
    (map->Element {:tag (name tag)
                   :attrs attrs
                   :react-key react-key
                   :children children})))

(defn camel->other-case [^String sep]
  (fn ^String [^String s]
    (-> s
      (str/replace #"([A-Z])" (str sep "$1"))
      str/lower-case)))

(def camel->kebab-case
  (camel->other-case "-"))

(def camel->colon-between
  (camel->other-case ":"))

(defn coerce-attr-key ^String [^String k]
  (cond
    (contains? lower-case-attrs k) (str/lower-case k)
    (contains? kebab-case-attrs k) (camel->kebab-case k)
    ;; special cases
    (= k "className") "class"
    (= k "htmlFor") "for"
    (contains? colon-between-attrs k) (camel->colon-between k)
    :else k))

(defn escape-html ^String [^String s]
  (let [len (count s)]
    (loop [^StringBuilder sb nil
           i                 (int 0)]
      (if (< i len)
        (let [char (.charAt s i)
              repl (case char
                     \& "&amp;"
                     \< "&lt;"
                     \> "&gt;"
                     \" "&quot;"
                     \' "&#x27;"
                     nil)]
          (if (nil? repl)
            (if (nil? sb)
              (recur nil (inc i))
              (recur (doto sb
                       (.append char))
                     (inc i)))
            (if (nil? sb)
              (recur (doto (StringBuilder.)
                       (.append s 0 i)
                       (.append repl))
                     (inc i))
              (recur (doto sb
                       (.append repl))
                     (inc i)))))
        (if (nil? sb) s (str sb))))))

(defn render-xml-attribute! [sb name value]
  (let [name (coerce-attr-key (clojure.core/name name))]
    (append! sb " " name "=\""
      (cond-> value
        (string? value) escape-html) "\"")))

(defn normalize-styles! [sb styles]
  (letfn [(coerce-value [k v]
            (cond-> v
              (and (number? v)
                (not (contains? no-suffix k))
                (pos? v))
              (str "px")))]
    (run! (fn [[k v]]
            (let [k (name k)]
              (append! sb (camel->kebab-case k) ":" (coerce-value k v) ";")))
      styles)))

(defn render-styles! [sb styles]
  (when-not (empty? styles)
    (append! sb " style=\"")
    (normalize-styles! sb styles)
    (append! sb "\"")))

(defn render-attribute! [sb [key value]]
  (cond
    (or (fn? value)
        (not value))
    nil

    (= key :style)
    (render-styles! sb value)
    ;; TODO: not sure if we want to limit values to strings/numbers
    (and (or (contains? supported-attrs (name key))
             (.startsWith (name key) "data-"))
         (or (string? value) (number? value)))
    (if (true? value)
      (append! sb " " (name key))
      (render-xml-attribute! sb key value))

    :else nil))

;; some props assigned first in input and option. see:
;; https://github.com/facebook/react/blob/08a08/src/renderers/dom/client/wrappers/ReactDOMOption.js#L108
;; https://github.com/facebook/react/blob/08a08/src/renderers/dom/client/wrappers/ReactDOMInput.js#L58
(defn render-attr-map! [sb tag attrs]
  (letfn [(sorter [order]
            (fn [[k _]]
              (get order k (->> (vals order)
                             (apply max)
                             inc))))]
    (let [attrs (cond->> attrs
                  (= tag "input") (sort-by (sorter {:type 0 :step 1}))
                  (= tag "option") (sort-by (sorter {:selected 0})))]
      (run! (partial render-attribute! sb) attrs))))

(def ^{:doc "A list of elements that must be rendered without a closing tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link"
    "meta" "param" "source" "track" "wbr"})

(defn container-tag?
  "Returns true if the tag has content or is not a void tag. In non-HTML modes,
  all contentless tags are assumed to be void tags."
  [tag content]
  (or content
    (and (not (void-tags tag)))))

(defn render-element!
  "Render a tag vector as a HTML element string."
  [{:keys [tag attrs children]} react-id ^StringBuilder sb]
  (append! sb "<" tag)
  (render-attr-map! sb tag attrs)
  (let [react-id-val @react-id]
    (when (= react-id-val 1)
      (append! sb " data-reactroot=\"\""))
    (append! sb " data-reactid=\"" react-id-val "\"")
    (vswap! react-id inc))
  (if (container-tag? tag (seq children))
    (do
      (append! sb ">")
      (run! #(p/-render-to-string % react-id sb) children)
      (append! sb "</" tag ">"))
    (append! sb "/>")))

(defn gen-tag-fn [tag]
  `(defn ~tag [~'attrs & ~'children]
     (element {:tag (quote ~tag)
               :attrs (dissoc ~'attrs :ref :key)
               :react-key (:key ~'attrs)
               :children ~'children})))

(defmacro gen-all-tags []
  `(do
     ~@(clojure.core/map gen-tag-fn tags)))

(gen-all-tags)

(def key-escape-lookup
  {"=" "=0"
   ":" "=2"})

;; preserves testability without having to compute checksums
(defn- render-to-str* ^StringBuilder [x]
  {:pre [(or (satisfies? p/IReactComponent x)
             (satisfies? p/IReactDOMElement x))]}
  (let [element (if-let [element (cond-> x
                                   (satisfies? p/IReactComponent x) render-component)]
                  element
                  (react-empty-node))
        sb (StringBuilder.)]
    (p/-render-to-string element (volatile! 1) sb)
    sb))

(defn render-to-str ^String [x]
  (let [sb (render-to-str* x)]
    (chk/assign-react-checksum sb)
    (str sb)))

(defn node
  "Returns the dom node associated with a component's React ref."
  ([component]
   {:pre [(satisfies? p/IReactComponent component)]}
   (p/-render component))
  ([component name]
   {:pre [(satisfies? p/IReactComponent component)]}
   (some-> @(p/-refs component) (get name) p/-render)))
