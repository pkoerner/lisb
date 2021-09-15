(ns lisb.fset.core
  (:require
   [clojure.java.io :as io]
   [lisb.core :refer [eval-ir-as-predicate eval-ir-as-expression]]
   [clojure.pprint :refer [pprint]]
   [lisb.high-level :refer [load-mch! save-mch! make-mch!]]
   [lisb.fset.transform :refer [transform]]))

;; This is the entry point.

;; define working Directories
(def source-dir "resources/machines/fset-source/")

(def target-dir "resources/machines/fset-target/")

;; Prefix of the resulting files after running transform-save-machines!
(def prefix "rw_") ;

;; Specify a List of Machines if you don't want all of them to transform.
(def machines-to-transform
  '("Lift.mch"
    ;"scheduler.mch"
    ))

(defn load-transform-machine!
  [source-filename]
  (transform (load-mch! source-filename)))

(defn load-transform-save-machine!
  [source-filename target-filename]
  (save-mch! (load-transform-machine! source-filename) target-filename))

(defn transform-machines!
  ([]
   (let [machines (.list (io/file source-dir))]
     (for [m machines]
       (load-transform-machine! (str source-dir m)))))
  ([machines]
   (for [m machines]
     (load-transform-machine! (str source-dir m)))))

(defn transform-save-machines!
  ([]
   (let [machines (.list (io/file source-dir))]
     (for [m machines]
       (load-transform-save-machine! (str source-dir m) (str target-dir prefix m)))))
  ([machines]
   (for [m machines]
     (load-transform-save-machine! (str source-dir m) (str target-dir prefix m)))))

(defn print-transform!
  "Takes a machine in IR and pprints it and its transformation."
  [machine]
  (let [m (make-mch! machine)
        transformed-machine (:ir (transform m))]
    (pprint "--------------------------")
    (pprint machine)
    (pprint "--------------------->>>>>")
    (pprint transformed-machine)
    (pprint "--------------------------")))

(comment

  (transform-machines! machines-to-transform)

  (transform-machines)

  (transform-save-machines! machines-to-transform)

  (transform-save-machines)

)
