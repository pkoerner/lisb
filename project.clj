(defproject lisb "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["snapshot" "https://oss.sonatype.org/content/repositories/snapshots"]
                 ["release" "https://oss.sonatype.org/content/repositories/releases"]]
  :jvm-opts ["-Xss1g"]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [de.hhu.stups/de.prob2.kernel,"3.14.0"]
                 [de.hhu.stups/value-translator "0.1.2"]
                 [de.hhu.stups/bparser "2.9.28"]])
