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

(declare noscript render-element)

(defrecord Element [tag attrs react-id react-key children]
  p/IReactDOMElement
  (-render-to-string [this]
    (render-element this))

  p/IReactChildren
  (-children [this] children))

(defrecord Text [s]
  p/IReactDOMElement
  (-render-to-string [this]
    (assert (string? s))
    s))

(defrecord ReactText [text react-id]
  p/IReactDOMElement
  (-render-to-string [this]
    (assert (string? text))
    (str "<!-- react-text: " react-id " -->" text "<!-- /react-text -->")))

(defn text-node
  "HTML text node"
  [s]
  (map->Text {:s s}))

(defn react-text-node
  "HTML text node"
  [s]
  (map->ReactText {:text s}))

(defn- nil-element []
  (noscript nil))

(defn- render-component [c]
  (if (or (nil? c) (satisfies? p/IReactDOMElement c))
    c
    (recur (p/-render c))))

(defn element
  "Creates a dom node."
  [{:keys [tag attrs react-key children] :as elem}]
                                        ;{:post [(valid-element? %)]}
  (assert (name tag))
  (assert (or (nil? attrs) (map? attrs)) (format "elem %s attrs invalid" elem))
  (let [children (flatten children)
        child-node-count (count children)
        nproc (.. Runtime getRuntime availableProcessors)
        reduce-fn (if (> child-node-count (/ nproc 2))
                 r/reduce
                 reduce)
        children (reduce-fn
                   (fn [res c]
                     (let [c' (cond
                                (satisfies? p/IReactDOMElement c) c

                                (satisfies? p/IReactComponent c)
                                (let [rendered (if-let [element (render-component c)]
                                                 element
                                                 (nil-element))]
                                  (assoc rendered :react-key
                                    (some-> (:props c) :cellophaneclj$reactKey)))

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

(defn camel->other-case [sep]
  (fn [s]
    (->> (str/split s #"(?=[A-Z])")
      (clojure.core/map #(reduce str %))
      (clojure.core/map str/lower-case)
      (str/join sep))))

(def camel->kebab-case
  (camel->other-case "-"))

(def camel->colon-between
  (camel->other-case ":"))

(defn coerce-attr-key [k]
  (cond
    (contains? lower-case-attrs k) (str/lower-case k)
    (contains? kebab-case-attrs k) (camel->kebab-case k)
    ;; special cases
    (= k "className") "class"
    (= k "htmlFor") "for"
    (contains? colon-between-attrs k) (camel->colon-between k)
    :else k))

(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  {:pre [(string? text)]
   :post [(string? %)]}
  (.. ^String (clojure.core/name text)
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")))

(defn xml-attribute [name value]
  (let [name (coerce-attr-key (clojure.core/name name))]
    (str " " name "=\"" (escape-html value) "\"")))

(defn format-styles [styles]
  (letfn [(coerce-value [k v]
            (cond-> v
              (and (number? v)
                   (not (contains? no-suffix k))
                   (pos? v))
              (str "px")))]
    (reduce (fn [s [k v]]
              (let [k (name k)]
                (str s (camel->kebab-case k) ":" (coerce-value k v) ";")))
      "" styles)))

(defn render-attribute [[key value]]
  (cond
    (fn? value) ""
    (not value) ""
    (= key :style) (cond->> (format-styles value)
                      (not (empty? value)) (xml-attribute key))
    ;; TODO: not sure if we want to limit values to strings/numbers
    (and (contains? supported-attrs (name key))
         (or (string? value) (number? value)))
    (xml-attribute key (cond-> value
                         (or (keyword? value)
                             (number? value)) str))
    (true? value) (str " " (name key))
    :else ""))

(defn render-attr-map [attrs]
  (apply str
    (clojure.core/map render-attribute attrs)))

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

(defn collect-strs
  [{:keys [tag attrs react-id children] :as elem}]
  (let [attrs (cond-> attrs
                (some? react-id)
                (assoc :data-reactid react-id))
        container-tag? (container-tag? tag (seq children))]
    (loop [children (seq children)
           worklist ["<" tag (render-attr-map attrs) ">"]]
      (if children
        (let [child (first children)]
          (recur (next children)
            (into worklist
              (cond
                (instance? Text child) [(:s child)]

                (instance? ReactText child) [(p/-render-to-string child)]

                :else (collect-strs child)))))
        (cond-> worklist
          container-tag? (into ["</" tag ">"]))))))

(defn render-element
  "Render a tag vector as a HTML element string."
  [element]
  (apply str (collect-strs element)))

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


(defn assign-react-ids
  ([elem]
   (let [elem (assoc-in elem [:attrs :data-reactroot] "")]
     (assign-react-ids elem (atom 1))))
  ([elem id]
   (let [elem (assoc elem :react-id @id)]
     (when-not (instance? Text elem)
       (swap! id inc))
     (update-in elem [:children]
       (fn [children]
         (mapv
           (fn [child]
             (assign-react-ids child id))
           children))))))

(defn- render-to-str* [x]
  {:pre [(or (satisfies? p/IReactComponent x)
             (satisfies? p/IReactDOMElement x))]}
  (let [element (if-let [element (cond-> x
                                   (satisfies? p/IReactComponent x) render-component)]
                  element
                  (nil-element))
        element (assign-react-ids element)]
    (p/-render-to-string element)))

;; preserves testability without having to compute checksums
(defn render-to-str [x]
  (let [markup (render-to-str* x)]
    (chk/assign-react-checksum markup)))

(defn node
  "Returns the dom node associated with a component's React ref."
  ([component]
   {:pre [(satisfies? p/IReactComponent component)]}
   (p/-render component))
  ([component name]
   {:pre [(satisfies? p/IReactComponent component)]}
   (some-> @(:refs component) (get name) p/-render)))
