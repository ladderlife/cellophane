(ns todomvc.util
  (:require [cognitect.transit :as t]
            #?@(:clj [[todomvc.parser :as server-parser]
                      [cellophane.next :as om]]))
  #?(:cljs (:import [goog.net XhrIo])))

(defn hidden [is-hidden]
  (if is-hidden
    #js {:display "none"}
    #js {}))

(defn pluralize [n word]
  (if (== n 1)
    word
    (str word "s")))

#?(:cljs
   (defn transit-post [url]
     (fn [{:keys [remote]} cb]
       (.send XhrIo url
         (fn [e]
           (this-as this
             (cb (t/read (t/reader :json) (.getResponseText this)))))
         "POST" (t/write (t/writer :json) remote)
         #js {"Content-Type" "application/transit+json"}))))

#?(:clj
   (def parser
     (om/parser {:read server-parser/readf :mutate server-parser/mutatef})))

#?(:clj
   (defn server-send [conn]
     (fn [{:keys [remote]} cb]
       (let [res (parser {:conn conn} remote)]
         (cb res remote)))))

(comment
  (def sel [{:todos/list [:db/id :todo/title :todo/completed :todo/created]}])

  (t/write (t/writer :json) sel)
  )
