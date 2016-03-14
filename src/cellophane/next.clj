(ns cellophane.next
  (:refer-clojure :exclude [var? force])
  (:require [cellophane.protocols :as p]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [om.next.impl.parser :as parser]
            [om.tempid :as tempid]
            [om.transit :as transit]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true :private true} *parent* nil)

;; =============================================================================
;; Utilities

(defn ident?
  "Returns true if x is an ident."
  [x]
  (and (vector? x)
    (== 2 (count x))
    (keyword? (nth x 0))))

(defn recursion? [x]
  (or (identical? '... x)
      (number? x)))


(defn union? [expr]
  (let [expr (cond-> expr (seq? expr) first)]
    (and (map? expr)
         (map? (-> expr first second)))))

(defn- join-entry [expr]
  (if (seq? expr)
    (ffirst expr)
    (first expr)))

(defn- join? [x]
  (let [x (if (seq? x) (first x) x)]
    (map? x)))

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

(defn- class-dispatch
  "Helper function for implementing static `query` and `params` multimethods.
   Dispatches on the (component) class"
  ([class] class)
  ([class _] class))

(defmulti class-query class-dispatch)

(defmulti class-params class-dispatch)
(defmethod class-params :default [_])

(defmulti class-ident class-dispatch)

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
          (reset! (:cellophaneclj$mounted? props#) true)
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
                   :cellophaneclj$mounted? (atom false)
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

(defn- mounted? [c]
  {:pre [(component? c)]}
  (-> c p/-props :cellophaneclj$mounted? deref))

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
  (some-> @(:refs component) (get name)))

(defn class-path [c]
  "Return the component class path associated with a component."
  {:pre [(component? c)]}
  (loop [c c ret (list (type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (type p) ret))
        (recur p ret))
      (let [seen (atom #{})]
        (take-while
          (fn [x]
            (when-not (contains? @seen x)
              (swap! seen conj x)
              x))
          ret)))))

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
  (let [qm (meta query)
        tr (map #(bind-query % params))
        ret (cond
              (seq? query) (sequence tr query)
              (instance? clojure.lang.IMapEntry query) (into [] tr query)
              (coll? query) (into (empty query) tr query)
              :else (replace-var query params))]
    (cond-> ret
      (and qm (instance? clojure.lang.IObj ret)) (with-meta qm))))

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
    (with-meta
      (bind-query q (:params qps (params c)))
      {:component (class c)})))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (when (iquery? x)
    (if (component? x)
      (get-component-query x)
      (with-meta (bind-query (class-query x) (class-params x)) {:component x}))))

(defn tag [x class]
  (vary-meta x assoc :component class))

(defn subquery
  "Given a class or mounted component x and a ref to an instantiated component
   or class that defines a subquery, pick the most specific subquery. If the
   component is mounted subquery-ref will be used, subquery-class otherwise."
  [x subquery-ref subquery-class]
  {:pre [(or (keyword? subquery-ref) (string? subquery-ref))
         (class? subquery-class)]}
  (if (and (component? x) (mounted? x))
    (get-query (react-ref x subquery-ref))
    (get-query subquery-class)))

(defn get-ident [c]
  {:pre [(component? c)]}
  (let [m (props c)]
    (assert (not (nil? m)) "get-ident invoked on component with nil props")
    (ident c m)))

;; ===================================================================
;; Parser

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [{:keys [read mutate] :as opts}]
  {:pre [(map? opts)]}
  (parser/parser opts))

(defn dispatch
  "Helper function for implementing :read and :mutate as multimethods. Use this
   as the dispatch-fn."
  [_ key _] key)

(defn query->ast
  "Given a query expression convert it into an AST."
  [query-expr]
  (parser/query->ast query-expr))

(defn ast->query [query-ast]
  "Given an AST convert it back into a query expression."
  (parser/ast->expr query-ast true))


(defn- ^boolean unique-ident? [x]
  (and (ident? x) (= '_ (second x))))

(defn- normalize* [query data refs union-seen]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (-> query meta :component)
          ident   (when (extends? Ident class)
                    (class-ident class data))]
      (if-not (nil? ident)
        (vary-meta (normalize* (get query (first ident)) data refs union-seen)
          assoc :om/tag (first ident))
        (throw (IllegalArgumentException. "Union components must implement Ident"))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [expr (first q)]
          (if (join? expr)
            (let [[k sel] (join-entry expr)
                  recursive? (recursion? sel)
                  union-entry (if (union? expr) sel union-seen)
                  sel     (if recursive?
                            (if-not (nil? union-seen)
                              union-seen
                              query)
                            sel)
                  class   (-> sel meta :component)
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs union-entry)]
                  (if-not (or (nil? class) (not (extends? Ident class)))
                    (let [i (class-ident class v)]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
                  (if-not (or (nil? class) (not (extends? Ident class)))
                    (let [is (into [] (map #(class-ident class %)) xs)]
                      (if (vector? sel)
                        (when-not (empty? is)
                          (swap! refs update-in [(ffirst is)]
                            (fn [ys]
                              (merge-with merge ys
                                (zipmap (map second is) xs)))))
                        ;; union case
                        (swap! refs
                          (fn [refs']
                            (reduce
                              (fn [ret [i x]]
                                (update-in ret i merge x))
                              refs' (map vector is xs)))))
                      (recur (next q) (assoc ret k is)))
                    (recur (next q) (assoc ret k xs))))

                ;; missing key
                (nil? v)
                (recur (next q) ret)

                ;; can't handle
                :else (recur (next q) (assoc ret k v))))
            (let [k (if (seq? expr) (first expr) expr)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defn tree->db
  "Given a Om component class or instance and a tree of data, use the component's
   query to transform the tree into the default database format. All nodes that
   can be mapped via Ident implementations wil be replaced with ident links. The
   original node data will be moved into tables indexed by ident. If merge-idents
   option is true, will return these tables in the result instead of as metadata."
  ([x data]
    (tree->db x data false))
  ([x data merge-idents]
   (let [refs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs nil)]
     (if merge-idents
       (let [refs' @refs]
         (assoc (merge ret refs')
           ::tables (into #{} (keys refs'))))
       (with-meta ret @refs)))))

(defn reduce-query-depth
  "Changes a join on key k with depth limit from [:a {:k n}] to [:a {:k (dec n)}]"
  [q k]
  (if-not (empty? (focus-query q [k]))
    (let [pos (query-template q [k])
          node (zip/node pos)
          node' (cond-> node (number? node) dec)]
      (replace pos node'))
    q))

(defn- reduce-union-recursion-depth
  "Given a union expression decrement each of the query roots by one if it
   is recursive."
  [union-expr recursion-key]
  (->> union-expr
    (map (fn [[k q]] [k (reduce-query-depth q recursion-key)]))
    (into {})))

;; TODO: easy to optimize

(defn- denormalize*
  "Denormalize a data based on query. refs is a data structure which maps idents
   to their values. map-ident is a function taking a ident to another ident,
   used during tempid transition. idents-seen is the set of idents encountered,
   used to limit recursion. union-expr is the current union expression being
   evaluated. recurse-key is key representing the current recursive query being
   evaluted."
  [query data refs map-ident idents-seen union-expr recurse-key]
  {:pre [(map? refs)]}
  ;; support taking ident for data param
  (let [data (cond-> data (ident? data) (->> map-ident (get-in refs)))]
    (if (vector? data)
      ;; join
      (let [step (fn [ident]
                   (let [ident'       (get-in refs (map-ident ident))
                         union-recur? (and union-expr recurse-key)
                         query        (cond-> query
                                        union-recur? (reduce-union-recursion-depth recurse-key))
                         ;; also reduce query depth of union-seen, there can
                         ;; be more union recursions inside
                         union-seen'  (cond-> union-expr
                                        union-recur? (reduce-union-recursion-depth recurse-key))
                         query'       (cond-> query
                                        (map? query) (get (first ident)))] ;; UNION
                     (denormalize* query' ident' refs map-ident idents-seen union-seen' nil)))]
        (into [] (map step) data))
      ;; map case
      (if (= '[*] query)
        data
        (let [{props false joins true} (group-by #(or (join? %) (ident? %)) query)
              props (mapv #(cond-> % (seq? %) first) props)]
          (loop [joins (seq joins) ret {}]
            (if-not (nil? joins)
              (let [join        (first joins)
                    join        (cond-> join (ident? join) (hash-map '[*]))
                    [key sel]   (join-entry join)
                    recurse?    (recursion? sel)
                    recurse-key (when recurse? key)
                    v           (if (ident? key)
                                  (if (= '_ (second key))
                                    (get refs (first key))
                                    (get-in refs (map-ident key)))
                                  (get data key))
                    key         (cond-> key (unique-ident? key) first)
                    v           (if (ident? v) (map-ident v) v)
                    limit       (if (number? sel) sel :none)
                    union-entry (if (union? join) sel union-expr)
                    sel         (cond
                                  recurse? (if-not (nil? union-expr)
                                             union-entry
                                             (reduce-query-depth query key))
                                  (and (ident? key) (union? join)) (get sel (first key))
                                  (and (ident? v) (union? join)) (get sel (first v))
                                  :else sel)
                    graph-loop? (and recurse?
                                  (contains? (set (get idents-seen key)) v)
                                  (= :none limit))
                    idents-seen (if (and (ident? v) recurse?)
                                  (-> idents-seen
                                    (update-in [key] (fnil conj #{}) v)
                                    (assoc-in [:last-ident key] v)) idents-seen)]
                (cond
                  (= 0 limit) (recur (next joins) ret)
                  graph-loop? (recur (next joins) ret)
                  (nil? v)    (recur (next joins) ret)
                  :else       (recur (next joins)
                                (assoc ret
                                  key (denormalize* sel v refs map-ident
                                        idents-seen union-entry recurse-key)))))
              (if-let [looped-key (some
                                    (fn [[k identset]]
                                      (if (contains? identset (get data k))
                                        (get-in idents-seen [:last-ident k])
                                        nil))
                                    (dissoc idents-seen :last-ident))]
                looped-key
                (merge (select-keys data props) ret)))))))))

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  ([query data refs]
   (denormalize* query data refs identity {} nil nil))
  ([query data refs map-ident]
   (denormalize* query data refs map-ident {} nil nil)))


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
