(defproject Genetic-Art "0.1.0"
  :description "PushGP, as implemented by Hamilton's CS 307 class."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/imagez "0.12.0"]
		 [net.mikera/core.matrix "0.61.0"]
		 [net.mikera/vectorz-clj "0.47.0"]]
  :main ^:skip-aot Genetic-Art.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
  :resource-paths ["~/307/Genetic-Art/resource"]
                
