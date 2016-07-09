(ns cellophane.next
  (:refer-clojure :exclude [var? force replace])
  (:require [cellophane.protocols :as p]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [om.next.protocols :as om-p]
            [om.next.impl.parser :as parser]
            [om.tempid :as tempid]
            [om.transit :as transit]
            [om.util :as util]))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true :private true} *reconciler* nil)
(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *shared* nil)
(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================
;; Utilities

(defn- expr->key
  "Given a query expression return its key."
  [expr]
  (cond
    (keyword? expr) expr
    (map? expr)     (ffirst expr)
    (seq? expr)     (let [expr' (first expr)]
                      (when (map? expr')
                        (ffirst expr')))
    (util/ident? expr)   (cond-> expr (= '_ (second expr)) first)
    :else
    (throw
      (ex-info (str "Invalid query expr " expr)
        {:type :error/invalid-expression}))))

(defn- query-zip
  "Return a zipper on a query expression."
  [root]
  (zip/zipper
    #(or (vector? %) (map? %) (seq? %))
    seq
    (fn [node children]
      (let [ret (cond
                  (vector? node) (vec children)
                  (map? node)    (into {} children)
                  (seq? node)    children)]
        (with-meta ret (meta node))))
    root))

(defn- move-to-key
  "Move from the current zipper location to the specified key. loc must be a
   hash map node."
  [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)
        (recur (zip/right loc))))))

(defn- query-template
  "Given a query and a path into a query return a zipper focused at the location
   specified by the path. This location can be replaced to customize / alter
   the query."
  [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path
                        k' (expr->key node)]
                    (if (identical? k k')
                      (if (or (map? node)
                              (and (seq? node) (map? (first node))))
                        (let [loc'  (move-to-key (cond-> loc (seq? node) zip/down) k)
                              node' (zip/node loc')]
                          (if (map? node') ;; UNION
                            (if (seq ks)
                              (recur
                                (zip/replace loc'
                                  (zip/node (move-to-key loc' (first ks))))
                                (next ks))
                              loc')
                            (recur loc' ks))) ;; JOIN
                        (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                      (recur (zip/right loc) path)))))))]
    (query-template* (query-zip query) path)))

(defn- replace [template new-query]
  (-> template (zip/replace new-query) zip/root))

(declare focus-query*)

(defn- focused-join [expr ks full-expr union-expr]
  (let [expr-meta (meta expr)
        expr' (cond
                (map? expr)
                (let [join-value (-> expr first second)
                      join-value (if (and (util/recursion? join-value)
                                          (seq ks))
                                   (if-not (nil? union-expr)
                                     union-expr
                                     full-expr)
                                   join-value)]
                  {(ffirst expr) (focus-query* join-value ks nil)})

                (seq? expr) (list (focused-join (first expr) ks nil nil) (second expr))
                :else       expr)]
    (cond-> expr'
      (some? expr-meta) (with-meta expr-meta))))

(defn- focus-query*
  [query path union-expr]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (util/join-key x)))
              (value [x]
                (focused-join x ks query union-expr))]
        (if (map? query) ;; UNION
          {k (focus-query* (get query k) ks query)}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

(defn focus-query
  "Given a query, focus it along the specified path.
  Examples:
    (om.next/focus-query [:foo :bar :baz] [:foo])
    => [:foo]
    (om.next/focus-query [{:foo [:bar :baz]} :woz] [:foo :bar])
    => [{:foo [:bar]}]"
  [query path]
  (focus-query* query path nil))

(defn- focus->path
  "Given a focused query return the path represented by the query.

   Examples:

     (om.next/focus->path [{:foo [{:bar {:baz []}]}])
     => [:foo :bar :baz]"
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*)
                (and (not= path bound)
                     (< (count path) (count bound))))
            (some util/join? focus)
            (== 1 (count focus)))
     (let [[k focus'] (util/join-entry (first focus))
           focus'     (if (util/recursion? focus')
                        focus
                        focus')]
       (recur focus' bound (conj path k)))
     path)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

(extend-type Object
  IQueryParams
  (params [this]
    (when-let [ps (-> this meta :params)]
      (ps this))))

(defprotocol IQuery
  (query [this] "Return the component's unbound query"))

(defprotocol ILocalState
  (-set-state! [this new-state] "Set the component's local state")
  (-get-state [this] "Get the component's local state")
  (-get-rendered-state [this] "Get the component's rendered local state")
  (-merge-pending-state! [this] "Get the component's pending local state"))

;; =============================================================================
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
    (reset! (p/-local-state component) new-state)))

(defn get-state
  ([component]
   (get-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (satisfies? ILocalState component)
               (-get-state component)
               @(p/-local-state component))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

(defn react-set-state!
  ([component new-state]
   (react-set-state! component new-state nil))
  ([component new-state cb]
   {:pre [(component? component)]}
   (set-state! component new-state)
   (cb)))

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
  ([component]
   (get-rendered-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (satisfies? ILocalState component)
               (-get-rendered-state component)
               (get-state component))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

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
           (binding [cellophane.next/*reconciler* (cellophane.next/get-reconciler this#)
                     cellophane.next/*depth*      (inc (cellophane.next/depth this#))
                     cellophane.next/*shared*     (cellophane.next/shared this#)
                     cellophane.next/*instrument* (cellophane.next/instrument this#)
                     cellophane.next/*parent*     this#]
            ~@body))))}
   :defaults
   `{~'componentWillMount
     ([this#])
     ~'componentDidMount
     ([this#])
     ~'render
     ([this#])}})

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
        [other-protocols obj-dt] (split-with (complement '#{Object}) dt)
        class-methods (when-not (empty? (:protocols statics))
                        (->> (partition 2 (:protocols statics))
                          (filter (fn [[p _]]
                                    (some #{(clojure.core/name p)}
                                      '#{"IQuery" "Ident" "IQueryParams"})))
                          (reduce
                            (fn [r [_ impl]]
                              (assoc r (keyword (first impl))
                                (cons 'fn (rest impl)))) {:params '(fn [this])})))]
    `(let [c# (fn ~name [state# refs# props# children#]
                ;; TODO: non-lifecycle methods defined in the JS prototype
                (reify
                  cellophane.protocols/IReactLifecycle
                  ~@(rest (reshape obj-dt reshape-map))

                  ~@other-protocols

                  ~@(:protocols statics)

                  cellophane.protocols/IReactChildren
                  (~'-children [this#]
                   children#)

                  cellophane.protocols/IReactComponent
                  (~'-render [this#]
                   (when-not @(:cellophaneclj$mounted? props#)
                     (swap! (:cellophaneclj$mounted? props#) not))
                   (p/componentWillMount this#)
                   ;(p/render this#)
                   ;(p/componentDidMount this#)
                   ;; simulate re-render
                   ;; TODO: use the actual `reconcile!` implementation, or actually re-render somehow
                   (p/render this#))
                  (~'-props [this]
                   props#)
                  (~'-local-state [this]
                   state#)
                  (~'-refs [this]
                   refs#)))]
       (def ~name
         (with-meta c# (merge {:component c#
                               :component-name (str (ns-name *ns*) "$" ~(str name))}
                         ~class-methods))))))

(defmacro defui [name & forms]
  (defui* name forms))

(defmacro ui
  [& forms]
  (let [t (gensym "ui_")]
    `(do (defui ~t ~@forms) ~t)))

(defn- munge-component-name [x]
  (let [cl (-> x meta :component-name)
        [ns-name cl-name] (str/split cl #"\.(?=[^.]*$)")]
    (munge
      (str (str/replace (str ns-name) "." "$") "$" cl-name))))

(defn- compute-react-key [cl props]
  (when-let [idx (-> props meta :om-path)]
    (str (munge-component-name cl) "_" idx)))

(declare iquery?)

(defn factory
  ([class]
   (factory class nil))
  ([class {:keys [validator keyfn instrument?]
           :or {instrument? true} :as opts}]
   {:pre [(fn? class)]}
   (fn self
     ([] (self nil))
     ([props & children]
      (when-not (nil? validator)
        (assert (validator props)))
      (if (and *instrument* instrument?)
        (*instrument*
          {:props    props
           :children children
           :class    class
           :factory  (factory class (assoc opts :instrument? false))})
        (let [react-key (cond
                          (some? keyfn) (keyfn props)
                          (some? (:react-key props)) (:react-key props)
                          :else (compute-react-key class props))
              ctor class
              ref (:ref props)
              props {:cellophaneclj$reactRef   ref
                     :cellophaneclj$reactKey   react-key
                     :cellophaneclj$value      (cond-> props
                                                 (map? props) (dissoc :ref))
                     :cellophaneclj$mounted?   (atom false)
                     :cellophaneclj$path       (-> props meta :om-path)
                     :cellophaneclj$reconciler *reconciler*
                     :cellophaneclj$parent     *parent*
                     :cellophaneclj$shared     *shared*
                     :cellophaneclj$instrument *instrument*
                     :cellophaneclj$depth      *depth*}
              component (ctor (atom nil) (atom nil) props children)
              init-state (try
                           (.initLocalState component)
                           (catch AbstractMethodError _))]
          (when ref
            (assert (some? *parent*))
            (swap! (p/-refs *parent*) assoc ref component))
          (when init-state
            (reset! (p/-local-state component) init-state))
          component))))))

(defn- get-prop [c prop]
  (get (p/-props c) prop))

(defn- mounted? [c]
  {:pre [(component? c)]}
  @(get-prop c :cellophaneclj$mounted?))

(defn- parent
  "Returns the parent Om component."
  [component]
  {:pre [(component? component)]}
  (get-prop component :cellophaneclj$parent))

(defn depth
  "Returns the render depth (a integer) of the component relative to
  the mount root."
  [component]
  (when (component? component)
    (get-prop component :cellophaneclj$depth)))

(defn get-reconciler
  [c]
  {:pre [(component? c)]}
  (get-prop c :cellophaneclj$reconciler))

(defn react-key [component]
  {:pre [(component? component)]}
  (get-prop component :cellophaneclj$reactKey))

(defn react-type [component]
  {:pre [(component? component)]}
  (let [[ns _ c] (str/split (reflect/typename (type component)) #"\$")
        ns (clojure.main/demunge ns)]
    @(find-var (symbol ns c))))

(defn react-ref [component name]
  {:pre [(component? component)]}
  (some-> @(p/-refs component) (get name)))

(defn- path
  "Returns the component's Om data path."
  [c]
  (get-prop c :cellophaneclj$path))

(defn shared
  "Return the global shared properties of the Om Next root. See :shared and
   :shared-fn reconciler options."
  ([component]
   (shared component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [shared (get-prop component :cellophaneclj$shared)
         ks     (cond-> k-or-ks
                  (not (sequential? k-or-ks)) vector)]
     (cond-> shared
       (not (empty? ks)) (get-in ks)))))

(defn instrument [component]
  {:pre [(component? component)]}
  (get-prop component :cellophaneclj$instrument))

(defn- raw-class-path [c]
  "Return the raw component class path associated with a component. Contains
   duplicates for recursive component trees."
  (loop [c c ret (list (react-type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (react-type p) ret))
        (recur p ret))
      ret)))

(defn class-path [c]
  "Return the component class path associated with a component."
  {:pre [(component? c)]}
  (let [raw-cp (raw-class-path c)]
    (loop [cp (seq raw-cp) ret [] seen #{}]
      (if cp
        (let [c (first cp)]
          (if (contains? seen c)
            (recur (next cp) ret seen)
            (recur (next cp) (conj ret c) (conj seen c))))
        (seq ret)))))

(defn- ^boolean recursive-class-path? [c]
  "Returns true if a component's classpath is recursive"
  {:pre [(component? c)]}
  (not (apply distinct? (raw-class-path c))))

;; =============================================================================
;; Query implementations

(defn iquery? [x]
  (if (fn? x)
    (some? (-> x meta :query))
    (let [class (cond-> x (component? x) class)]
      (extends? IQuery class))))

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

(defn- component->query-data [component]
  (some-> (get-reconciler component)
    :config :state deref ::queries (get component)))

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (component->query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (component->query-data component) (params component)))

(defn- get-component-query
  ([component]
   (get-component-query component (component->query-data component)))
  ([component query-data]
   (let [q  (:query query-data (query component))
         c' (-> q meta :component)]
     (assert (nil? c')
       (str "Query violation, " component " reuses " c' " query"))
     (with-meta
       (bind-query q (:params query-data (params component)))
       {:component (react-type component)}))))

(defn- get-class-or-instance-query
  "Return a IQuery/IParams local bound query. Works for component classes
   and component instances. Does not use the indexer."
  [x]
  (if (component? x)
    (get-component-query x)
    (let [q ((-> x meta :query) x)
          c (-> q meta :component)]
      (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
      (with-meta (bind-query q ((-> x meta :params) x)) {:component x}))))

(declare get-indexer)

(defn- get-indexed-query
  "Get a component's static query from the indexer. For recursive queries, recurses
   up the data path. Falls back to `get-class-or-instance-query` if nothing is
   found in the indexer."
  [component class-path-query-data data-path]
  (let [qs (filter #(= data-path (-> % zip/root (focus->path data-path)))
             class-path-query-data)
        qs (if (empty? qs) class-path-query-data qs)]
    (if-not (empty? qs)
      (let [q (first qs)
            node (zip/node q)]
        (if-not (util/recursion? node)
          node
          (recur component class-path-query-data (pop data-path))))
      (get-class-or-instance-query component))))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (when (iquery? x)
    (if (component? x)
      (if-let [query-data (component->query-data x)]
        (get-component-query x query-data)
        (let [cp (class-path x)
              r (get-reconciler x)
              data-path (into [] (remove number?) (path x))
              class-path-query-data (get (:class-path->query @(get-indexer r)) cp)]
          (get-indexed-query x class-path-query-data data-path)))
      (get-class-or-instance-query x))))

(defn tag [x class]
  (vary-meta x assoc :component class))

(defn subquery
  "Given a class or mounted component x and a ref to an instantiated component
   or class that defines a subquery, pick the most specific subquery. If the
   component is mounted subquery-ref will be used, subquery-class otherwise."
  [x subquery-ref subquery-class]
  {:pre [(or (keyword? subquery-ref) (string? subquery-ref))
         (fn? subquery-class)]}
  (if (and (component? x) (mounted? x))
    (get-query (react-ref x subquery-ref))
    (get-query subquery-class)))

(defn get-ident [c]
  {:pre [(component? c)]}
  (let [m (props c)]
    (assert (not (nil? m)) "get-ident invoked on component with nil props")
    (ident c m)))

(declare full-query force ref->components reconciler? to-env schedule-sends!)

(defn gather-sends
  [{:keys [parser] :as env} q remotes]
  (into {}
    (comp
      (map #(vector % (parser env q %)))
      (filter (fn [[_ v]] (pos? (count v)))))
    remotes))

(defn transform-reads
  "Given r (a reconciler) and a query expression including a mutation and
   any simple reads, return the equivalent query expression where the simple
   reads have been replaced by the full query for each component that cares about
   the specified read."
  [r tx]
  (letfn [(with-target [target q]
            (if-not (nil? target)
              [(force (first q) target)]
              q))
          (add-focused-query [k target tx c]
            (->> (focus-query (get-query c) [k])
              (with-target target)
              (full-query c)
              (into tx)))]
    (loop [exprs (seq tx) tx' []]
      (if-not (nil? exprs)
        (let [expr (first exprs)
              ast  (parser/expr->ast expr)
              key  (:key ast)
              tgt  (:target ast)]
          (if (keyword? key)
            (recur (next exprs)
              (reduce #(add-focused-query key tgt %1 %2)
                tx' (ref->components r key)))
            (recur (next exprs) (conj tx' expr))))
        tx'))))

(defn set-query!
  "Change the query of a component. Takes a map containing :params and/or
   :query. :params should be a map of new bindings and :query should be a query
   expression. Will schedule a re-render as well as remote re-sends if
   necessary."
  ([x params&query]
    (set-query! x params&query nil))
  ([x {:keys [params query]} reads]
   {:pre [(or (reconciler? x)
              (component? x))
          (or (not (nil? params))
              (not (nil? query)))
          (or (nil? reads)
              (vector? reads))]}
   (let [r    (if (component? x)
                (get-reconciler x)
                x)
         c    (when (component? x) x)
         root (:root @(:state r))
         cfg  (:config r)
         st   (:state cfg)
         ;; id   (random-uuid)
         ;; _    (.add (:history cfg) id @st)
         ]
     ;; (when-let [l (:logger cfg)]
     ;;   (glog/info l
     ;;     (str (when-let [ident (when (implements? Ident c)
     ;;                             (ident c (props c)))]
     ;;            (str (pr-str ident) " "))
     ;;       (when (reconciler? x) "reconciler ")
     ;;       (when query "changed query '" query ", ")
     ;;       (when params "changed params " params " ")
     ;;       (pr-str id))))
     (swap! st update-in [:cellophane.next/queries (or c root)] merge
       (merge (when query {:query query}) (when params {:params params})))
     (when (and (not (nil? c)) (nil? reads))
       (om-p/queue! r [c]))
     (when-not (nil? reads)
       (om-p/queue! r reads))
     (om-p/reindex! r)
     (let [rootq (if (not (nil? c))
                   (full-query c)
                   (when (nil? reads)
                     (get-query root)))
           sends (gather-sends (to-env cfg)
                   (into (or rootq []) (transform-reads r reads)) (:remotes cfg))]
       (when-not (empty? sends)
         (om-p/queue-sends! r sends)
         (schedule-sends! r)))
     nil)))

(defn update-query!
  "Update a component's query and query parameters with a function."
  ([component f]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)})))
  ([component f arg0]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0)))
  ([component f arg0 arg1]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-query! component
     (apply f {:query  (get-unbound-query component)
               :params (get-params component)}
       arg0 arg1 arg2 arg3 arg-rest))))

;; =============================================================================
;; Reconciler API

(declare remove-root!)

(defn schedule-sends! [reconciler]
  (when (om-p/schedule-sends! reconciler)
    (om-p/send! reconciler)))

(defn add-root!
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   {:pre [(reconciler? reconciler) (fn? root-class)]}
   (om-p/add-root! reconciler root-class target options)))

(defn remove-root!
  [reconciler target]
  (om-p/remove-root! reconciler target))

;; =============================================================================
;; Transactions

(defprotocol ITxIntercept
  (tx-intercept [c tx]
    "An optional protocol that component may implement to intercept child
     transactions."))

(defn- to-env [x]
  (let [config (if (reconciler? x) (:config x) x)]
    (select-keys config [:state :shared :parser :logger :pathopt])))

(defn transact* [r c ref tx]
  (let [cfg  (:config r)
        ref  (if (and c (satisfies? Ident c) (not ref))
               (ident c (props c))
               ref)
        env  (merge
               (to-env cfg)
               {:reconciler r :component c}
               (when ref
                 {:ref ref}))
        ;; id   (random-uuid)
        ;; _    (.add (:history cfg) id @(:state cfg))
        ;; _    (when-let [l (:logger cfg)]
        ;;        (glog/info l
        ;;          (str (when ref (str (pr-str ref) " "))
        ;;            "transacted '" tx ", " (pr-str id))))
        v    ((:parser cfg) env tx)
        snds (gather-sends env tx (:remotes cfg))
        q    (cond-> []
               (not (nil? c)) (conj c)
               (not (nil? ref)) (conj ref))]
    (om-p/queue! r (into q (remove symbol?) (keys v)))
    (when-not (empty? snds)
      (om-p/queue-sends! r snds)
      (schedule-sends! r))))

(defn annotate-mutations
  "Given a query expression annotate all mutations by adding a :mutator -> ident
   entry to the metadata of each mutation expression in the query."
  [tx ident]
  (letfn [(annotate [expr ident]
            (cond-> expr
              (util/mutation? expr) (vary-meta assoc :mutator ident)))]
    (into [] (map #(annotate % ident)) tx)))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(or (component? x)
              (reconciler? x))
          (vector? tx)]}
   (let [tx (cond-> tx
              (and (component? x) (satisfies? Ident x))
              (annotate-mutations (get-ident x)))]
     (if (reconciler? x)
       (transact* x nil nil tx)
       (do
         (assert (iquery? x)
           (str "transact! invoked by component " x
             " that does not implement IQuery"))
         (loop [p (parent x) x x tx tx]
           (if (nil? p)
             (let [r (get-reconciler x)]
               (transact* r x nil (transform-reads r tx)))
             (let [[x' tx] (if (satisfies? ITxIntercept p)
                             [p (tx-intercept p tx)]
                             [x tx])]
               (recur (parent p) x' tx))))))))
  ([r ref tx]
   (transact* r nil ref tx)))

;; =============================================================================
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

;; ===================================================================
;; Indexer

(defn- cascade-query
  "Cascades a query up the classpath. class-path->query is a map of classpaths
   to their queries. classpath is the classpath where we start cascading (typically
   the direct parent's classpath of the component changing query). data-path is
   the data path in the classpath's query to the new query. new-query is the
   query to be applied to the classpaths. union-keys are any keys into union
   queries found during index building; they are used to access union queries
   when cascading the classpath, and to generate the classpaths' rendered-paths,
   which skip these keys."
  [class-path->query classpath data-path new-query union-keys]
  (loop [cp classpath
         data-path data-path
         new-query new-query
         ret {}]
    (if-not (empty? cp)
      (let [rendered-data-path (into [] (remove (set union-keys)) data-path)
            filter-data-path (cond-> rendered-data-path
                               (not (empty? rendered-data-path)) pop)
            qs (filter #(= filter-data-path
                           (-> % zip/root (focus->path filter-data-path)))
                 (get class-path->query cp))
            qs' (into #{}
                  (map (fn [q]
                         (let [new-query (if (or (map? (zip/node q))
                                               (some #{(peek data-path)} union-keys))
                                           (let [union-key (peek data-path)]
                                             (-> (query-template (zip/root q)
                                                   rendered-data-path)
                                                 zip/node
                                                 (assoc union-key new-query)))
                                           new-query)]
                           (-> (zip/root q)
                             (query-template rendered-data-path)
                             (replace new-query)
                             (focus-query filter-data-path)
                             (query-template filter-data-path)))))
                  qs)]
        (recur (pop cp) (pop data-path)
          (-> qs' first zip/node) (assoc ret cp qs')))
      ret)))

(defrecord Indexer [indexes extfs]
  clojure.lang.IDeref
  (deref [_] @indexes)

  om-p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (get-query x)
          class             (cond-> x (component? x) react-type)]
      (letfn [(get-dispatch-key [prop]
                (cond-> prop
                  (or (not (util/ident? prop))
                      (= (second prop) '_))
                  ((comp :dispatch-key parser/expr->ast))))
              (build-index* [class query path classpath union-expr union-keys]
                (let [recursive? (some #{class} classpath)
                      classpath  (cond-> classpath
                                   (and (not (nil? class))
                                        (not recursive?))
                                   (conj class))
                      dp->cs     (get-in @indexes [:data-path->components])]
                  (when class
                    (let [focused-query (query-template
                                          (focus-query rootq path)
                                          path)
                          cp-query      (cond-> focused-query
                                          (and (not= (zip/node focused-query) query)
                                               (not recursive?))
                                          (zip/replace query))]
                      (swap! class-path->query update-in [classpath]
                        (fnil conj #{}) cp-query)))
                  (let [recursive-join? (and recursive?
                                          (some (fn [e]
                                                  (and (util/join? e)
                                                    (not (util/recursion?
                                                           (util/join-value e)))))
                                            query)
                                          (= (distinct path) path))
                        recursive-union? (and recursive?
                                           union-expr
                                           (= (distinct path) path))]
                    (when (or (not recursive?)
                            recursive-join?
                            recursive-union?)
                      (cond
                        (vector? query)
                        (let [{props false joins true} (group-by util/join? query)]
                          (swap! prop->classes
                            #(merge-with into %
                               (zipmap
                                 (map get-dispatch-key props)
                                 (repeat #{class}))))
                          (doseq [join joins]
                            (let [[prop query']  (util/join-entry join)
                                  prop-dispatch-key (get-dispatch-key prop)
                                  recursion?     (util/recursion? query')
                                  union-recursion? (and recursion? union-expr)
                                  query'         (if recursion?
                                                   (if-not (nil? union-expr)
                                                     union-expr
                                                     query)
                                                   query')
                                  path'          (conj path prop)
                                  rendered-path' (into [] (remove (set union-keys) path'))
                                  cs (get dp->cs rendered-path')
                                  cascade-query? (and (= (count cs) 1)
                                                   (= (-> query' meta :component)
                                                     (react-type (first cs)))
                                                   (not (map? query')))
                                  query''        (if cascade-query?
                                                   (get-query (first cs))
                                                   query')]
                              (swap! prop->classes
                                #(merge-with into % {prop-dispatch-key #{class}}))
                              (when (and cascade-query? (not= query' query''))
                                (let [cp->q' (cascade-query @class-path->query classpath
                                               path' query'' union-keys)]
                                  (swap! class-path->query merge cp->q')))
                              (let [class' (-> query'' meta :component)]
                                (when-not (and recursion? (nil? class'))
                                  (build-index* class' query''
                                    path' classpath (if recursion? union-expr nil) union-keys))))))

                        ;; Union query case
                        (map? query)
                        (doseq [[prop query'] query]
                          (let [path'          (conj path prop)
                                class'         (-> query' meta :component)
                                cs             (filter #(= class' (react-type %))
                                                 (get dp->cs path))
                                cascade-query? (and class' (= (count cs) 1))
                                query''        (if cascade-query?
                                                 (get-query (first cs))
                                                 query')]
                            (when (and cascade-query? (not= query' query''))
                              (let [qs        (get @class-path->query classpath)
                                    q         (first qs)
                                    qnode     (zip/node
                                                (cond-> q
                                                  (nil? class) (query-template path)))
                                    new-query (assoc qnode
                                                prop query'')
                                    q'        (cond-> (zip/replace
                                                        (query-template (zip/root q) path)
                                                        new-query)
                                                (nil? class)
                                                (-> zip/root
                                                  (focus-query (pop path))
                                                  (query-template (pop path))))
                                    qs'       (into #{q'} (remove #{q}) qs)
                                    cp->q'    (merge {classpath qs'}
                                                (cascade-query @class-path->query
                                                  (pop classpath) path
                                                  (zip/node q') union-keys))]
                                (swap! class-path->query merge cp->q')))
                            (build-index* class' query'' path' classpath query (conj union-keys prop)))))))))]
        (build-index* class rootq [] [] nil [])
        (swap! indexes merge
          {:prop->classes     @prop->classes
           :class-path->query @class-path->query}))))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:index-component extfs) indexes c)
                        [:class->components (react-type c)]
                        (fnil conj #{}) c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:index-component extfs) indexes c)
                        [:data-path->components data-path]
                        (fnil conj #{}) c)
              ident     (when (satisfies? Ident c)
                          (ident c (props c)))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] (fnil conj #{}) c))
            indexes)))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:drop-component extfs) indexes c)
                        [:class->components (react-type c)]
                        disj c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:drop-component extfs) indexes c)
                        [:data-path->components data-path]
                        disj c)
              ident     (when (satisfies? Ident c)
                        (ident c (props c)))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] disj c))
            indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (component? k)
        #{k}
        (if-let [cs ((:ref->components extfs) indexes k)]
          cs
          (let [cs (get-in indexes [:ref->components k] ::not-found)]
            (if-not (identical? ::not-found cs)
              cs
              (if (keyword? k)
                ;; TODO: more robust validation, might be bogus key
                (let [cs (get-in indexes [:prop->classes k])]
                  (transduce (map #(get-in indexes [:class->components %]))
                    (completing into) #{} cs))
                #{}))))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  ([]
    (indexer
      {:index-component (fn [indexes component] indexes)
       :drop-component  (fn [indexes component] indexes)
       :ref->components (fn [indexes ref] nil)}))
  ([extfs]
   (Indexer.
     (atom
       {:class->components {}
        :data-path->components {}
        :ref->components   {}})
     extfs)))

(defn- get-indexer
  "Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn ref->components
  "Return all components for a given ref."
  [x ref]
  (when-not (nil? ref)
    (let [indexer (if (reconciler? x) (get-indexer x) x)]
      (om-p/key->components indexer ref))))

(defn ref->any
  "Get any component from the indexer that matches the ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (om-p/key->components indexer ref))))

(defn class->any
  "Get any component from the indexer that matches the component class."
  [x class]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (get-in @indexer [:class->components class]))))

(defn class-path->queries
  "Given x (a reconciler or indexer) and y (a component or component class
   path), return the queries for that path."
  [x y]
  (let [indexer (if (reconciler? x) (get-indexer x) x)
        cp      (if (component? y) (class-path y) y)]
    (into #{} (map zip/root)
      (get-in @indexer [:class-path->query cp]))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (iquery? component)
     (if (nil? (path component))
       (replace
         (first
           (get-in @(-> component get-reconciler get-indexer)
             [:class-path->query (class-path component)]))
         (get-query component))
       (full-query component (get-query component)))))
  ([component query]
   (when (iquery? component)
     (let [xf    (cond->> (remove number?)
                   (recursive-class-path? component) (comp (distinct)))
           path' (into [] xf (path component))
           cp    (class-path component)
           qs    (get-in @(-> component get-reconciler get-indexer)
                   [:class-path->query cp])]
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (first (filter #(= path' (-> % zip/root (focus->path path'))) qs))]
           (if-not (nil? q)
             (replace q query)
             (throw
               (ex-info (str "No queries exist for component path " cp " or data path " path')
                 {:type :cellophane.next/no-queries}))))
         (throw
           (ex-info (str "No queries exist for component path " cp)
             {:type :cellophane.next/no-queries})))))))

(defn- normalize* [query data refs union-seen]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (-> query meta :component)
          ident   (when-let [ident (-> class meta :ident)]
                    (ident class data))]
      (if-not (nil? ident)
        (vary-meta (normalize* (get query (first ident)) data refs union-seen)
          assoc :om/tag (first ident))
        (throw (IllegalArgumentException. "Union components must implement Ident"))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [expr (first q)]
          (if (util/join? expr)
            (let [[k sel] (util/join-entry expr)
                  recursive? (util/recursion? sel)
                  union-entry (if (util/union? expr) sel union-seen)
                  sel     (if recursive?
                            (if-not (nil? union-seen)
                              union-seen
                              query)
                            sel)
                  class   (-> sel meta :component)
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (util/ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs union-entry)]
                  (if-not (or (nil? class) (not (-> class meta :ident)))
                    (let [i ((-> class meta :ident) class v)]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
                  (if-not (or (nil? class) (not (-> class meta :ident)))
                    (let [is (into [] (map #((-> class meta :ident) class %)) xs)]
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
  (let [data (loop [data data]
               (if (util/ident? data)
                 (recur (get-in refs (map-ident data)))
                 data))]
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
        (let [{props false joins true} (group-by #(or (util/join? %)
                                                      (util/ident? %)
                                                      (and (seq? %)
                                                           (util/ident? (first %))))
                                         query)
              props (mapv #(cond-> % (seq? %) first) props)]
          (loop [joins (seq joins) ret {}]
            (if-not (nil? joins)
              (let [join        (first joins)
                    join        (cond-> join
                                  (seq? join) first)
                    join        (cond-> join (util/ident? join) (hash-map '[*]))
                    [key sel]   (util/join-entry join)
                    recurse?    (util/recursion? sel)
                    recurse-key (when recurse? key)
                    v           (if (util/ident? key)
                                  (if (= '_ (second key))
                                    (get refs (first key))
                                    (get-in refs (map-ident key)))
                                  (get data key))
                    key         (cond-> key (util/unique-ident? key) first)
                    v           (if (util/ident? v)
                                  (loop [v v]
                                    (let [next (get-in refs (map-ident v))]
                                      (if (util/ident? next)
                                        (recur next)
                                        (map-ident v))))
                                  v)
                    limit       (if (number? sel) sel :none)
                    union-entry (if (util/union? join) sel union-expr)
                    sel         (cond
                                  recurse? (if-not (nil? union-expr)
                                             union-entry
                                             (reduce-query-depth query key))
                                  (and (util/ident? v) (util/union? join)) (get sel (first v))
                                  (and (util/ident? key) (util/union? join)) (get sel (first key))
                                  :else sel)
                    graph-loop? (and recurse?
                                  (contains? (set (get idents-seen key)) v)
                                  (= :none limit))
                    idents-seen (if (and (util/ident? v) recurse?)
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

;; ===================================================================
;; Reconciler

(defn- sift-idents [res]
  (let [{idents true rest false} (group-by #(vector? (first %)) res)]
    [(into {} idents) (into {} rest)]))

(defn- merge-idents [tree config refs query]
  (let [{:keys [merge-ident indexer]} config
        ident-joins (into {} (filter #(and (util/join? %)
                                           (util/ident? (util/join-key %)))
                               query))]
    (letfn [(step [tree' [ident props]]
              (if (:normalize config)
                (let [c-or-q (or (get ident-joins ident) (ref->any indexer ident))
                      props' (tree->db c-or-q props)
                      refs   (meta props')]
                  ((:merge-tree config)
                   (merge-ident config tree' ident props') refs))
                (merge-ident config tree' ident props)))]
      (reduce step tree refs))))

(defn- merge-novelty!
  [reconciler state res query]
  (let [config      (:config reconciler)
        [idts res'] (sift-idents res)
        res'        (if (:normalize config)
                      (tree->db
                        (or query (:root @(:state reconciler)))
                        res' true)
                      res')]
    (-> state
      (merge-idents config idts query)
      ((:merge-tree config) res'))))

(defn default-merge [reconciler state res query]
  {:keys    (into [] (remove symbol?) (keys res))
   :next    (merge-novelty! reconciler state res query)
   :tempids (->> (filter (comp symbol? first) res)
              (map (comp :tempids second))
              (reduce merge {}))})

(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  ([reconciler delta]
    (merge! reconciler delta nil))
  ([reconciler delta query]
   (let [config (:config reconciler)
         state (:state config)
         merge* (:merge config)
         {:keys [keys next tempids]} (merge* reconciler @state delta query)]
     (om-p/queue! reconciler keys)
     (reset! state
       (if-let [migrate (:migrate config)]
         (merge (select-keys next [:cellophane.next/queries])
           (migrate next
             (or query (get-query (:root @(:state reconciler))))
             tempids (:id-key config)))
         next)))))

(defn- default-merge-ident
  [_ tree ref props]
  (update-in tree ref merge props))

(defn- default-merge-tree
  [a b]
  (if (map? a)
    (merge a b)
    b))

(defn- default-migrate
  "Given app-state-pure (the application state as an immutable value), a query,
   tempids (a hash map from tempid to stable id), and an optional id-key
   keyword, return a new application state value with the tempids replaced by
   the stable ids."
  ([app-state-pure query tempids]
    (default-migrate app-state-pure query tempids nil))
  ([app-state-pure query tempids id-key]
   (letfn [(dissoc-in [pure [table id]]
             (assoc pure table (dissoc (get pure table) id)))
           (step [pure [old [_ id :as new]]]
             (-> pure
               (dissoc-in old)
               (assoc-in new
                 (cond-> (merge (get-in pure old) (get-in pure new))
                   (not (nil? id-key)) (assoc id-key id)))))]
     (if-not (empty? tempids)
       (let [pure' (reduce step app-state-pure tempids)]
         (tree->db query
           (db->tree query pure' pure'
             (fn [ident] (get tempids ident ident))) true))
       app-state-pure))))

(defrecord Reconciler [config state]
  clojure.lang.IDeref
  (deref [this] @(:state config))

  om-p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class)
          ;; TODO: remove this or switch to UUID if we actually watch the state atom.
          guid  (Math/random)
          ]
      (when (iquery? root-class)
        (om-p/index-root (:indexer config) root-class))
      (when (and (:normalize config)
                 (not (:normalized @state)))
        (let [new-state (tree->db root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)))
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *shared*     (merge
                                               (:shared config)
                                               (when (:shared-fn config)
                                                 ((:shared-fn config) data)))
                                *instrument* (:instrument config)]
                        ;; TODO: think of ways not to create so many instances
                        (let [c (rctor data)]
                          ;(om-p/-render c)
                          (swap! state assoc :root c)
                          (reset! ret c))))
            parsef  (fn []
                      (let [sel (get-query (or @ret root-class))]
                        (assert (or (nil? sel) (vector? sel))
                          "Application root query must be a vector")
                        (if-not (nil? sel)
                          (let [env (to-env config)
                                v   ((:parser config) env sel)]
                            (when-not (empty? v)
                              (renderf v)))
                          (renderf @(:state config)))))]
        (swap! state merge
          {:target target :render parsef :root root-class
           :remove (fn []
                     (remove-watch (:state config) (or target guid))
                     (swap! state
                       #(-> %
                         (dissoc :target) (dissoc :render) (dissoc :root)
                         (dissoc :remove)))
                     (when-not (nil? target)
                       ((:root-unmount config) target)))})
        (add-watch (:state config) (or target guid)
          (fn [_ _ _ _]
            (swap! state update-in [:t] inc)
            ;(schedule-render! this)
            ))
        (parsef)
        (when-let [sel (get-query (or @ret root-class))]
          (let [env  (to-env config)
                snds (gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds
                  (fn send-cb
                    ([res]
                     (send-cb res nil))
                    ([res query]
                     (merge! this res query)
                     (parsef))))))))
        @ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [this]
    (let [root (get @state :root)]
      (when (iquery? root)
        (let [indexer (:indexer config)
              c (first (get-in @indexer [:class->components root]))]
          (om-p/index-root indexer (or c root))))))

  (queue! [_ ks]
    (swap! state update-in [:queue] into ks))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
      (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (swap! state update-in [:queued] not)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc :sends-queued true)
        true)
      false))

  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [_]
    (let [st @state
          q  (:queue st)]
      (swap! state update-in [:queued] not)
      (swap! state assoc :queue [])
      (cond
        ;; TODO: need to move root re-render logic outside of batching logic
        (empty? q) ((:render st))

        :else
        (let [cs (transduce
                   (map #(om-p/key->components (:indexer config) %))
                   #(into %1 %2) #{} q)
              {:keys [ui->props]} config
              env (to-env config)]
          (doseq [c ((:optimize config) cs)]
            (when (mounted? c)
              (let [computed   (get-computed (props c))
                    next-props (cellophane.next/computed (ui->props env c) computed)]
                #_(when (should-update? c next-props (get-state c))
                  (if-not (nil? next-props)
                    (update-component! c next-props)
                    (.forceUpdate c))))))))))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
          (fn [state]
            (-> state
              (assoc :queued-sends {})
              (assoc :sends-queued false))))
        ((:send config) sends
         (fn send-cb
           ([res]
            (send-cb res nil))
           ([res query]
            (merge! this res query))))))))

(defn reconciler
  [{:keys [state shared shared-fn
           parser indexer
           ui->props normalize
           send merge-sends remotes
           merge merge-tree merge-ident
           prune-tree
           optimize
           history
           root-render root-unmount
           pathopt
           migrate id-key
           instrument]
    :or {;ui->props    default-ui->props
         indexer      cellophane.next/indexer
         merge-sends  #(merge-with into %1 %2)
         remotes      [:remote]
         merge        default-merge
         merge-tree   default-merge-tree
         merge-ident  default-merge-ident
         ;; prune-tree   default-extract-errors
         ;; optimize     (fn [cs] (sort-by depth cs))
         history      100
         ;; root-render  #(js/ReactDOM.render %1 %2)
         ;; root-unmount #(js/ReactDOM.unmountComponentAtNode %)
         pathopt      false
         migrate      default-migrate}
    :as config}]
  {:pre [(map? config)]}
  (let [idxr   (indexer)
        norm?  (instance? clojure.lang.Atom state)
        state' (if norm? state (atom state))
        ;; logger (if (contains? config :logger)
        ;;          (:logger config)
        ;;          *logger*)
        ret    (->Reconciler
                 {:state state' :shared shared :shared-fn shared-fn
                  :parser parser :indexer idxr
                  ;; :ui->props ui->props
                  :send send :merge-sends merge-sends :remotes remotes
                  :merge merge :merge-tree merge-tree :merge-ident merge-ident
                  ;; :prune-tree prune-tree
                  ;; :optimize optimize
                  :normalize (or (not norm?) normalize)
                  ;; :history (c/cache history)
                  :root-render root-render :root-unmount root-unmount
                  ;; :logger logger
                  :pathopt pathopt
                  :migrate migrate
                  :id-key id-key
                  :instrument instrument}
                 (atom {:queue [] :queued false :queued-sends {}
                        :sends-queued false
                        :target nil :root nil :render nil :remove nil
                        :t 0 :normalized norm?}))]
    ret))

(defn reconciler? [x]
  (instance? Reconciler x))

(defn app-state
  "Return the reconciler's application state atom. Useful when the reconciler
   was initialized via denormalized data."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :state))

(defn app-root
  "Return the application's root component."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (get @(:state reconciler) :root))

(defn force-root-render!
  "Force a re-render of the root. Not recommended for anything except
   recomputing :shared."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  ((get @(:state reconciler) :render)))

(defn tempid
  "Return a temporary id."
  ([] (tempid/tempid))
  ([id] (tempid/tempid id)))

(defn tempid?
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
