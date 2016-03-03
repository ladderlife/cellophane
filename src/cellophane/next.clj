(ns cellophane.next
  (:require [cellophane.protocols :as p]))


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

(defn defui* [name forms]
  (let [{:keys [dt statics]} (collect-statics forms)]
    `(def ~name
      (reify cellophane.protocols/ReactLifecycle
        ~@(rest dt)
        cellophane.protocols/ReactComponent
        (~'-render [this#]
         (.render this#))
        ))))

(defmacro defui [name & forms]
  (defui* name forms))

(defn factory [])

(comment
  (defui* 'Artist
     '(static IFoo
        (foo [_])
        (bar [_])
        static field sel '[:woz ?noz]
        Object
        (render [_] "hello world"))))
