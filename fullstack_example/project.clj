(defproject om-next-demo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ^:replace ["-Xms512m" "-Xmx512m" "-server"]

  :dependencies [[org.clojure/clojure "1.9.0-alpha7"]
                 [org.clojure/clojurescript "1.9.76"]
                 [com.datomic/datomic-free "0.9.5372"]
                 [hiccup "1.0.5"]
                 [bidi "2.0.9"]
                 [com.ladderlife/cellophane "0.3.2"]
                 [org.omcljs/om "1.0.0-alpha36"]
                 [ring/ring "1.4.0"]
                 [ring/ring-headers "0.2.0"]
                 [com.cognitect/transit-clj "0.8.285"]
                 [com.cognitect/transit-cljs "0.8.237"]
                 [com.stuartsierra/component "0.3.1"]

                 [figwheel-sidecar "0.5.4" :scope "test"]]
  :clean-targets ^{:protect false} ["resources/public/js"]
  :source-paths ["src/clj" "src/shared"]
  )
