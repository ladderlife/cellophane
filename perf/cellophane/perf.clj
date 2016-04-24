(ns cellophane.perf
  (:require [cellophane.dom :as dom]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as enlive]
            [clojure.test :refer [deftest]]
            [criterium.core :as criterium]
            [hiccup.core :as hiccup]))

(def ^:dynamic *convert-style?* true)
(def ^:dynamic *hiccup* false)

(defn convert-tag-name [tag attrs]
  (let [id (:id attrs)
        classes (when-not (str/blank? (:class attrs))
                  (->> (str/split (:class attrs) #"\s+")
                       (remove str/blank?)))]
    (keyword
      (str tag
           (when id (str "#" id))
           (when-not (empty? classes)
             (str "." (str/join "." classes)))))))

(defn convert-style [s]
  (into {}
    (for [[_ k v] (re-seq #"([\w+\-]+)\s*:\s*([^;]+)" s)]
      (let [k' (keyword k)
            v' (condp re-matches v
                 #"(\d+)px"      :>> (fn [[_ n]] (Long/parseLong n))
                 #"(\d+\.\d+)px" :>> (fn [[_ n]] (Double/parseDouble n))
                 v)]
        [k' v']))))

(defn- convert-opt-key [[k v]]
  [(get
     {:class :className
      :for :htmlFor}
     k k)
   v])

(defn convert-attrs [attrs]
  (let [attrs' (cond-> attrs
                 true (dissoc :data-bem)
                 (and *convert-style?*
                   (contains? attrs :style)) (update :style convert-style)
                 true not-empty)]
    (cond->> attrs'
      (and (not *hiccup*)
        (some #{:for :class} (keys attrs))) (into {} (map convert-opt-key)))))

(defn convert-tag* [form]
  (cond
    ;; tag
    (map? form)
    (when-not (= :comment (:type form))
      (let [{:keys [tag attrs content type]} form
            tag'      (if *hiccup*
                        (convert-tag-name (name tag) attrs)
                        (symbol "cellophane.dom" (name tag)))
            attrs'    (convert-attrs attrs)
            children  (->> (map convert-tag* content)
                           (keep identity))]
        (if *hiccup*
          (vec
            (concat [tag'] (when attrs' [attrs']) children))
          `(~tag' ~attrs' ~@children))))
    
    ;; text node
    (string? form)
    (when-not (str/blank? form)
      form)))

(defmacro convert-tag [form]
  `(convert-tag* ~form))

(defn convert-page [page]
  (let [page (-> (slurp page)
               .getBytes
               java.io.ByteArrayInputStream.
               enlive/html-resource
               (enlive/select [:body])
               first
               convert-tag
               )]
    (cond-> page
      (not *hiccup*) eval)))

(defn file-size [path]
  (-> (count (slurp path)) (/ 1000) (long) (str " kB")))

(defn -main [& args]
  (doseq [page ["page1.html"
                "page2.html"
                "page3.html"]
          :let [path (str "perf/pages/" page)]]
    (let [comp (convert-page path)]
      (println "\n--- Testing" page (str "(" (file-size path) ")") "---")
      (criterium/quick-bench (dom/render-to-str comp)))
      
    (let [comp (binding [*convert-style?* false
                         *hiccup*         true]
                 (convert-page path))]
      (println "\n+++ With Hiccup +++")
      (criterium/quick-bench (hiccup/html comp)))))
