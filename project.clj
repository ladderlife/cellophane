(defproject com.ladderlife/cellophane "0.2.4"
  :description "Server-side rendering for Om Next components"
  :url "http://github.com/ladderlife/cellophane"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]

                 [org.omcljs/om "1.0.0-alpha31"]]

  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :source-paths ["src" "test"]
  :test-paths ["test"]
  :clean-targets ["target"]
  :target-path "target"

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.7.228"]
                                  [cljsjs/react-dom-server "0.14.3-0"]]}})
