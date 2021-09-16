(defproject org.clojars.pkoerner/lisb "0.0.1-SNAPSHOT"
  :description "lisb: A Clojure library that wraps ProB's constraint solver. Use it to write/transform B machines with less pain or interact with the constraint solver."
  :url "https://gitlab.cs.uni-duesseldorf.de/mager/lisb-clone" ;; subject to change
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["snapshot" "https://oss.sonatype.org/content/repositories/snapshots"]
                 ["release" "https://oss.sonatype.org/content/repositories/releases"]]
  :jvm-opts ["-Xss1g"]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [potemkin "0.4.5"]
                 [de.hhu.stups/de.prob2.kernel,"3.14.0"]
                 [de.hhu.stups/value-translator "0.1.2"]
                 [de.hhu.stups/bparser "2.9.28"]
                 [clj-wallhack "1.0.1"]])
