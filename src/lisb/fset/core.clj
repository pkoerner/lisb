(ns lisb.fset.core
  (:require
   [clojure.java.io :as io]
   [lisb.core :refer [eval-ir-as-predicate eval-ir-as-expression]]
   [clojure.pprint :refer [pprint]]
   [lisb.high-level :refer [load-mch! save-mch! make-mch!]]
   [lisb.fset.transform :refer [transform]]))

;; This is the entry point for the module.

;; define working Directories
(def source-dir "resources/machines/fset-source/")

(def target-dir "resources/machines/fset-target/")

;; Prefix of the resulting files after running transform-save-machines!
(def prefix "rw_") ;

;; Set the metadata for the Translation Process.
(def meta-data {:deferred-size 3})

;; Specify a List of Machines if you don't want all of them to transform.
(def machines-to-transform
  '("Lift.mch"
    ; "scheduler.mch"
    ))

(defn load-transform-machine!
  [source-filename]
  (transform (load-mch! source-filename meta-data)))

(defn load-transform-save-machine!
  [source-filename target-filename]
  (save-mch! (:ir (load-transform-machine! source-filename)) target-filename))

(defn transform-machines!
  "Transforms all or just a list of the B machines in the set source directory and
   returns a list of the IR's"
  ([]
   (let [machines (.list (io/file source-dir))]
     (for [m machines]
       (load-transform-machine! (str source-dir m)))))
  ([machines]
   (for [m machines]
     (load-transform-machine! (str source-dir m)))))

(defn transform-save-machines!
  "Transforms all or just a list of the B machines in the set source directory and
   saves the as files in the set target directory."
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
  (let [m (make-mch! machine meta-data)
        transformed-machine (:ir (transform m))]
    (pprint "--------------------------")
    (pprint machine)
    (pprint "--------------------->>>>>")
    (pprint transformed-machine)
    (pprint "--------------------------")))

(comment

  (transform-machines! machines-to-transform)

  (transform-machines!)

  (transform-save-machines!)

  (transform-save-machines! machines-to-transform)

)
