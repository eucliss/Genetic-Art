(defproject Genetic-Art "0.1.0"
  :description "PushGP, as implemented by Hamilton's CS 307 class."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot Genetic-Art.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
