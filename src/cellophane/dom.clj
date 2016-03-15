(ns cellophane.dom
  (:refer-clojure :exclude [map meta time use])
  (:require [clojure.string :as str]
            [cellophane.protocols :as p])
  (:import [java.util.zip Adler32]))

;; ===================================================================
;; Checksums (data-react-checksum)

;; Not equal to React's optimized version of adler32. See
;; https://github.com/facebook/react/blob/3b96650/src/shared/utils/adler32.js
(defn checksum [markup]
  (let [chk (Adler32.)
        bytes (.getBytes markup)]
    (.update chk bytes 0 (count bytes))
    (.getValue chk)))


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
    options
    select])

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
  #{"acceptCharset" "httpEquiv"})

(defn camel->kebab-case [s]
  (->> (str/split s #"(?=[A-Z])")
    (clojure.core/map #(reduce str %))
    (clojure.core/map str/lower-case)
    (str/join "-")))

(defn coerce-attr-key [k]
  (cond
    (contains? lower-case-attrs k) (str/lower-case k)
    (contains? kebab-case-attrs k) (camel->kebab-case k)
    ;; special cases
    (= k "className") "class"
    (= k "htmlFor") "for"
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

(defn render-attribute [[name value]]
  (cond
    (true? value) (str " " (clojure.core/name name))
    (fn? value) ""
    (not value) ""
    (= name :style) (cond->> (format-styles value)
                      (not (empty? value)) (xml-attribute name))
    :else (xml-attribute name value)))

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

(defn react-id-str [react-id]
  (assert (vector? react-id))
  (str "." (str/join "." react-id)))

(defn render-element
  "Render an tag vector as a HTML element string."
  [{:keys [tag attrs react-id children]}]
  (assert react-id)
  (let [attrs (assoc attrs :data-reactid (react-id-str react-id))]
    (if (container-tag? tag (seq children))
      (str "<" tag (render-attr-map attrs) ">"
        (apply str (clojure.core/map p/-render-to-string children))
        "</" tag ">")
      (str "<" tag (render-attr-map attrs) ">"))))

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

(defn text-node
  "HTML text node"
  [s]
  (map->Text {:s s}))

(declare span)

(defn element
  "Creates a dom node."
  [{:keys [tag attrs children] :as elem}]
                                        ;{:post [(valid-element? %)]}
  (assert (name tag))
  (assert (or (nil? attrs) (map? attrs)) (format "elem %s attrs invalid" elem))
  (let [children (flatten children)
        child-node-count (count children)
        children (doall (->> (clojure.core/map
                               (fn [c]
                                 (cond
                                   (satisfies? p/IReactDOMElement c) c

                                   (satisfies? p/IReactComponent c)
                                   (assoc (p/-render c) :react-key
                                     (some-> (p/-props c) :cellophaneclj$reactKey))

                                   (or (string? c) (number? c))
                                   (let [c (cond-> c (number? c) str)]
                                     (if (> child-node-count 1)
                                       (span nil c)
                                       (text-node c)))
                                   (nil? c) nil
                                   :else (do
                                           (println "invalid child element:" c (class c))
                                           (assert false)))) children)
                          (filter identity)))]
    (map->Element {:tag (name tag)
                   :attrs attrs
                   :children children})))

(defn gen-tag-fn [tag]
  `(defn ~tag [~'attrs & ~'children]
     (element {:tag (quote ~tag)
               :attrs (dissoc ~'attrs :ref)
               :children ~'children})))

(defmacro gen-all-tags []
  `(do
     ~@(clojure.core/map gen-tag-fn tags)))

(gen-all-tags)

(def key-escape-lookup
  {"=" "=0"
   ":" "=2"})

;; https://github.com/facebook/react/blob/bef45/src/shared/utils/traverseAllChildren.js
(defn wrap-user-provided-key [key]
  (when key
    (str "$" (str/replace key #"[=:]" key-escape-lookup))))

(defn assign-react-ids
  ([elem]
   (let [id (or (wrap-user-provided-key (:react-key elem))
              0)]
     (assign-react-ids elem [id])))
  ([elem id]
   (assert (vector? id))
   (let [elem (assoc elem :react-id id)]
     (update-in elem [:children]
       (fn [children]
         (map-indexed (fn [i c]
                        (let [react-id (or (wrap-user-provided-key (:react-key c))
                                         i)]
                          (assign-react-ids c (conj id react-id)))) children))))))

(defn render-to-str [class]
  {:pre [(satisfies? p/IReactComponent class)]}
  (let [element (p/-render class)
        element (assign-react-ids element)]
    (p/-render-to-string element)))
