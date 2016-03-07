(ns cellophane.next
  (:require [cellophane.protocols :as p]))

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
  (let [{:keys [dt statics]} (collect-statics forms)]
    `(defrecord ~name [~'state props# children#]
       ;; TODO: props & children
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
            (println "abstrctmethoderror")))))))

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



(comment
  (defui* 'Artist
     '(static IQuery
        (query [_]
          [:foo :bar])
        Object
        (render [_] "hello world"))))
