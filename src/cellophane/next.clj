(ns cellophane.next
  (:refer-clojure :exclude [var? force])
  (:require [cellophane.protocols :as p]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [om.tempid :as tempid]
            [om.transit :as transit]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true :private true} *parent* nil)

;; ===================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

(extend-type Object
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this] "Return the component's unbound query"))

(defprotocol ILocalState
  (-set-state! [this new-state] "Set the component's local state")
  (-get-state [this] "Get the component's local state")
  (-get-rendered-state [this] "Get the component's rendered local state")
  (-merge-pending-state! [this] "Get the component's pending local state"))

(defn- dispatch
  "Helper function for implementing static `query` and `params` multimethods.
   Dispatches on the (component) class"
  [class] class)

(defmulti class-query dispatch)

(defmulti class-params dispatch)
(defmethod class-params :default [_])

(defmulti class-ident dispatch)

;; ===================================================================
;; React bridging (defui, factory, props, state)

(defn renderable? [x]
  (and (satisfies? p/IReactComponent x)
    (try
      (boolean (p/render x))
      (catch AbstractMethodError e
        false))))

(defn component? [x]
  (if-not (nil? x)
    (satisfies? p/IReactComponent x)
    false))

(defn props [component]
  {:pre [(component? component)]}
  (:cellophaneclj$value (p/-props component)))

(defn computed
  "Add computed properties to props."
  [props computed-map]
  (when-not (nil? props)
    (if (vector? props)
      (cond-> props
        (not (empty? computed-map)) (vary-meta assoc :cellophane.next/computed computed-map))
      (cond-> props
        (not (empty? computed-map)) (assoc :cellophane.next/computed computed-map)))))

(defn get-computed
  "Return the computed properties on a component or its props."
  ([x]
   (get-computed x []))
  ([x k-or-ks]
   (when-not (nil? x)
     (let [props (cond-> x (component? x) props)
           ks    (into [:cellophane.next/computed]
                   (cond-> k-or-ks
                     (not (sequential? k-or-ks)) vector))]
       (if (vector? props)
         (-> props meta (get-in ks))
         (get-in props ks))))))

(defn children [component]
  {:pre [(component? component)]}
  (p/-children component))

(defn set-state!
  [component new-state]
  {:pre [(component? component)]}
  (if (satisfies? ILocalState component)
    (-set-state! component new-state)
    (reset! (:state component) new-state)))

(defn get-state
  ([component]
   (get-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (satisfies? ILocalState component)
               (-get-state component)
               @(:state component))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

(defn update-state!
  ([component f]
   (set-state! component (f (get-state component))))
  ([component f arg0]
   (set-state! component (f (get-state component) arg0)))
  ([component f arg0 arg1]
   (set-state! component (f (get-state component) arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-state! component (f (get-state component) arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3]
   (set-state! component (f (get-state component) arg0 arg1 arg2 arg3)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-state! component
     (apply f (get-state component) arg0 arg1 arg2 arg3 arg-rest))))

(defn get-rendered-state
  [component]
  {:pre [(component? component)]}
  (if (satisfies? ILocalState component)
    (-get-rendered-state component)
    (get-state component)))

(defn collect-statics [dt]
  (letfn [(split-on-static [forms]
            (split-with (complement '#{static}) forms))
          (split-on-symbol [forms]
            (split-with (complement symbol?) forms))]
    (loop [dt (seq dt) dt' [] statics {:fields {} :protocols []}]
      (if dt
        (let [[pre [_ sym & remaining :as post]] (split-on-static dt)
              dt' (into dt' pre)]
          (if (seq post)
            (cond
              (= sym 'field)
              (let [[field-info dt] (split-at 2 remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:fields] conj (vec field-info))))
              (symbol? sym)
              (let [[protocol-info dt] (split-on-symbol remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:protocols]
                    into (concat [sym] protocol-info))))
              :else (throw (IllegalArgumentException. "Malformed static")))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))

(def reshape-map
  {:reshape
   {'render
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#]
           (binding [;om.next/*reconciler* (om.next/get-reconciler this#)
                     ;om.next/*depth*      (inc (om.next/depth this#))
                     ;om.next/*shared*     (om.next/shared this#)
                     ;om.next/*instrument* (om.next/instrument this#)
                     cellophane.next/*parent* this#]
            ~@body))))}})

(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (reshapef x))
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))
          (add-object-protocol [dt]
            (if-not (some '#{Object} dt)
              (conj dt 'Object)
              dt))]
    (->> dt (map reshape*) vec add-object-protocol add-defaults)))

;; TODO: probably need to reshape dt to implement defaults
(defn defui* [name forms]
  (let [{:keys [dt statics]} (collect-statics forms)
        define-class-methods (when-not (empty? (:protocols statics))
                               `(do
                                  ~@(->> (partition 2 (:protocols statics))
                                      (filter (fn [[p _]]
                                                (some #{(clojure.core/name p)}
                                                  '#{"IQuery" "Ident" "IQueryParams"})))
                                      (map
                                        (fn [[_ impl]]
                                          (cons 'defmethod
                                            (cons (symbol (str "cellophane.next/class-" (first impl)))
                                              (cons name (rest impl)))))))))]
    `(do
       (defrecord ~name [~'state ~'refs props# children#]
         ;; TODO: non-lifecycle methods defined in the JS prototype
         cellophane.protocols/IReactLifecycle
         ~@(rest (reshape dt reshape-map))

         ~@(:protocols statics)

         cellophane.protocols/IReactChildren
         (~'-children [this#]
          children#)

         cellophane.protocols/IReactComponent
         (~'-props [this#]
          props#)
         (~'-render [this#]
          ;; TODO: circle back. where should this exception be caught?
          (try
            (p/render this#)
            (catch AbstractMethodError e#
              (println "abstrctmethoderror")))))

       ~define-class-methods)))

(defmacro defui [name & forms]
  (defui* name forms))

(defn- munge-component-name [x]
  (let [cl (reflect/typename (cond-> x (component? x) class))
        [ns-name cl-name] (str/split cl #"\.(?=[^.]*$)")]
    (munge
      (str (str/replace (str ns-name) "." "$") "$" cl-name))))

(defn- compute-react-key [cl props]
  (when-let [idx (-> props meta :om-path)]
    (str (munge-component-name cl) "_" idx)))

(defn- path-meta
  [x path]
  (let [x' (cond->> x
             (map? x) (into {} (map (fn [[k v]] [k (path-meta v (conj path k))])))
             (vector? x) (into [] (map-indexed #(path-meta %2 (conj path %1)))))]
    (cond-> x'
      (instance? clojure.lang.IObj x')
      (vary-meta assoc :om-path path))))

(declare iquery?)

(defn factory
  ([class]
   (factory class nil))
  ;; TODO: support validator
  ([class {:keys [validator keyfn] :as opts}]
   {:pre [(class? class)]}
   (fn self
     ([] (self nil))
     ([props & children]
      ;; setting path-meta here works because props are fed to factory
      ;; from the root
      (let [root? (nil? (meta props))
            props (cond-> props
                    (and (iquery? class) root?) (path-meta []))
            react-key (cond
                        (some? keyfn) (keyfn props)
                        (some? (:react-key props)) (:react-key props)
                        :else (when-not root?
                                (compute-react-key class props)))
            ctor (.getConstructor class
                   (into-array java.lang.Class
                     (take 4 (repeat java.lang.Object))))
            ref (:ref props)
            props {:cellophaneclj$reactRef ref
                   :cellophaneclj$reactKey react-key
                   :cellophaneclj$parent *parent*
                   :cellophaneclj$value (dissoc props :ref)}
            component (.newInstance ctor
                                    ;;   state      refs
                        (object-array [(atom nil) (atom nil) props children]))
            init-state (try
                         (.initLocalState component)
                         (catch AbstractMethodError _))]
        (when ref
          (assert (some? *parent*))
          (swap! (:refs *parent*) assoc ref component))
        (when init-state
          (reset! (:state component) init-state))
        component)))))

(defn- parent
  "Returns the parent Om component."
  [component]
  {:pre [(component? component)]}
  (-> component p/-props :cellophaneclj$parent))

(defn react-key [component]
  {:pre [(component? component)]}
  (get (p/-props component) :cellophaneclj$reactKey))

(defn react-type [component]
  {:pre [(component? component)]}
  (type component))

(defn react-ref [component name]
  {:pre [(component? component)]}
  (p/-render component)
  (some-> @(:refs component) (get name)))

#_(defn add-root!
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   ))

;; ===================================================================
;; Query implementations

(defn iquery? [x]
  (let [class (cond-> x (component? x) class)]
    (extends? IQuery class)))

(defn- var? [x]
  (and (symbol? x)
       (.startsWith (str x) "?")))

(defn- var->keyword [x]
  (keyword (.substring (str x) 1)))

(defn- replace-var [expr params]
  (if (var? expr)
    (get params (var->keyword expr) expr)
    expr))

(defn- bind-query [query params]
   (letfn [(replace-var [expr]
             (if (var? expr)
               (get params (var->keyword expr) expr)
               expr))]
     (walk/prewalk replace-var query)))

(defn- get-local-query-data [component]
  ;; TODO: change when we implement `set-query!`
  {:query  (query component)
   :params (params component)})

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (get-local-query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (get-local-query-data component) (params component)))

(defn- get-component-query [c]
  (let [qps (get-local-query-data c)
        q   (:query qps (query c))]
    (bind-query q (:params qps (params c)))))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (when (iquery? x)
    (if (component? x)
      (get-component-query x)
      (bind-query (class-query x) (class-params x)))))

(defn get-ident [c]
  {:pre [(component? c)]}
  (let [m (props c)]
    (assert (not (nil? m)) "get-ident invoked on component with nil props")
    (ident c m)))

;; ===================================================================

(defn tempid
  "Return a temporary id."
  ([] (tempid/tempid))
  ([id] (tempid/tempid id)))

(defn ^boolean tempid?
  "Return true if x is a tempid, false otherwise"
  [x]
  (tempid/tempid? x))

(defn reader
  "Create a Om Next transit reader. This reader can handler the tempid type.
   Can pass transit reader customization opts map."
  ([] (transit/reader))
  ([opts] (transit/reader opts)))

(defn writer
  "Create a Om Next transit writer. This writer can handler the tempid type.
   Can pass transit writer customization opts map."
  ([] (transit/writer))
  ([opts] (transit/writer opts)))

(defn force
  "Given a query expression return an equivalent query expression that can be
   spliced into a transaction that will force a read of that key from the
   specified remote target."
  ([expr]
    (force expr :remote))
  ([expr target]
    (with-meta (list 'quote expr) {:target target})))


(comment
  (defui* 'Artist
    '(static IQuery
       (query [_]
         [:foo :bar])))
  )
