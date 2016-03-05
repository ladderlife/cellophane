(ns cellophane.next
  (:require [cellophane.protocols :as p]))

;; ===================================================================
;; Protocols

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
;; defui

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
    `(defrecord ~name [state# props# children#]
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
   class
   #_(fn self
     ([] (self nil))
     ([props & children]
      ))
   ))

#_(defn add-root!
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   ))

(comment
  (defui* 'Artist
     '(static IQuery
        (query [_]
          [:foo :bar])
        Object
        (render [_] "hello world"))))
