(ns cellophane.next
  (:refer-clojure :exclude [var?])
  (:require [cellophane.protocols :as p]
            [clojure.walk :as walk]))

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
  (p/-props component))

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
       (defrecord ~name [~'state props# children#]
         ;; TODO: non-lifecycle methods defined in the JS prototype
         cellophane.protocols/IReactLifecycle
         ~@(rest dt)

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

(defn factory
  ([class]
   (factory class nil))
  ;; TODO: support key-fn etc.
  ([class {:keys [validator key-fn] :as opts}]
   {:pre [(class? class)]}
   (fn self
     ([] (self nil))
     ([props & children]
      (let [ctor (.getConstructor class
                   (into-array java.lang.Class [java.lang.Object
                                                java.lang.Object
                                                java.lang.Object]))
            component (.newInstance ctor (object-array [(atom nil) props children]))
            init-state (try
                         (.initLocalState component)
                         (catch AbstractMethodError _))]
        (when init-state
          (reset! (:state component) init-state))
        component)))))

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

(comment
  (defui* 'Artist
     '(static IQuery
        (query [_]
          [:foo :bar])
        Object
        (render [_] "hello world"))))
