(defproject box-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.5.648"]
                 [org.clojure/tools.cli "1.0.194"]]
  :aot :all
  :main box-solver.core
  :jvm-opts ["-Xmx6g" "-XX:MaxMetaspaceSize=1024m"]
  :resource-paths ["resources"]
  :repl-options {:init-ns box-solver.core})
