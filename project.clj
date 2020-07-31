(defproject bouncer "0.1.0-SNAPSHOT"

  :description "Employ when someone looks like a troublemaker"
  :url "http://github.com/rbkmoney/bouncer"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies
  [[org.clojure/clojure "1.10.1"]
   [datascript "1.0.0"]
   [criterium "0.4.6"]
   [com.clojure-goes-fast/clj-async-profiler "0.4.1"]]

  :jvm-opts ["-Xmx2g" "-server"]

  :target-path "target/%s"
  
  :main bouncer.main

  :profiles
  {:uberjar
   {:aot :all}

   :aot
   {:jvm-opts ^:replace ["-Xmx2g"
                         "-server"
                         "-Dclojure.compiler.direct-linking=true"
                         "-Djdk.attach.allowAttachSelf"
                         "-XX:+UnlockDiagnosticVMOptions"
                         "-XX:+DebugNonSafepoints"]}})
