(defproject com.ladderlife/cellophane "0.3.6-SNAPSHOT"
  :description "Server-side rendering for Om Next components"
  :url "http://github.com/ladderlife/cellophane"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.9.0-alpha12" :scope "provided"]
                 [org.omcljs/om "1.0.0-alpha44" :scope "provided"]]

  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :source-paths ["src" "test"]
  :test-paths ["test"]
  :clean-targets ["target"]
  :target-path "target"

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.9.229"]
                                  [cljsjs/react-dom-server "15.3.1-0"]]}
             :perf {:source-paths ["perf"]
                    :dependencies
                    [[enlive    "1.1.6"]
                     [criterium "0.4.4"]
                     [hiccup    "1.0.5"]] }}
  :aliases {"perf" ["with-profile" "perf" "trampoline" "run" "-m" "cellophane.perf"]})
