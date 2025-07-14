(defproject org.clojars.pkoerner/lisb "0.0.6-SNAPSHOT"
  :description "lisb: A Clojure library that wraps ProB's constraint solver. Use it to write/transform B machines with less pain or interact with the constraint solver."
  :url "https://github.com/pkoerner/lisb" ;; subject to change
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["central" {:url "https://repo1.maven.org/maven2/"}]
                 ["sonatype" {:url "https://central.sonatype.com/repository/maven-snapshots/"}]
                 ["clojars" {:url "https://repo.clojars.org"}]]
  :deploy-repositories [["releases"  {:sign-releases false :url "https://repo.clojars.org/"}]
                        ["snapshots" {:sign-releases false :url "https://repo.clojars.org/"}]]
  :jvm-opts ["-Xss1g"]
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/test.check "1.1.1"]
                 [potemkin "0.4.8"]
                 [com.rpl/specter "1.1.4"]
                 [de.hhu.stups/prob-java "4.15.1-SNAPSHOT"]
                 [de.hhu.stups/value-translator "0.2.1"]
                 ])
