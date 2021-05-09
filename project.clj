(defproject layout-gen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [datascript "1.1.0"]
                 [metosin/malli "0.4.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [semantic-csv "0.2.1-alpha1"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot layout-gen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
