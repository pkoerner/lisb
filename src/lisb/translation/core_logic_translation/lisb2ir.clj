(ns lisb.translation.core-logic-translation.lisb2ir
  (:require [clojure.core.logic.pldb :as pldb])
  (:require [lisb.translation.core-logic-translation.database :as db])
  (:require [clojure.core.logic :refer :all])
  (:require [clojure.core.logic.protocols :refer [IWalkTerm walk-term]]))

(declare translato pre-translato helper facts_special_ir facts_special_lisb)


;; this code snippet is directly taken from the first answer to https://stackoverflow.com/questions/18017924/core-logic-stackoverflow-when-using-sets
(extend-protocol IWalkTerm
  clojure.lang.IPersistentSet
  (walk-term [v f] (with-meta (set (walk-term (seq v) f)) (meta v))))


;; special case functions

(defn lisb-name [[name args body]]
  `(~'op [] ~name ~args ~body))

(defn ir->lisb "Initiates main query IR to lisb." [ir]
  (pldb/with-dbs [db/facts_args_ir db/facts_ops facts_special_ir]
    (run 1 [q] (pre-translato q ir))))

(defn lisb->ir "Initiates main query lisb to IR." [lisb]
  (pldb/with-dbs [db/facts_args_ir db/facts_ops facts_special_lisb]
    (run 1 [q] (pre-translato lisb q))))

(def primitive? 
  "predicate that returns true if argument is not a seq or a symbol." 
  (some-fn number? boolean? string? keyword? set?))

(defn translato "Returns IR of lisb input, and lisb of IR input."[input]
  (cond 
    (and (seqable? input) (empty? input)) (list input) 
    (map? input) (ir->lisb input)
    :else (lisb->ir input)))


(defn fa-sugar [[f ids p c]]
  `(~'for-all ~ids (~'implication ~p ~c)))

(defn <--name [[_ returns [name args body]]]
  `(~'op ~returns ~name ~args ~body))

(defn <--call [[_ returns [_ op args]]]
  `(~'op-call ~returns ~op ~args))

(defn <--name-IR [{:keys [returns name args body]}]
  (if (empty? returns)
    `(~name ~args ~(first (translato body)))
    `(~'<-- ~returns (~name ~args ~(first (translato body))))))


(defn <--op-IR [{:keys [returns op args]}]
  `(~'<-- ~returns (~'op-call ~op ~args)))


(defn parse-machine-reference-ir->lisb
  [{:keys [tag name abstract-machine-name machine-clauses]}]
  (concat `(~(symbol tag) ~name ~(first (translato abstract-machine-name))) (map #(first (translato %)) machine-clauses)))

(defn parse-machine-reference-lisb->ir
  [[op name ref & clauses]]
  (if (seqable? ref)
    `(~op ~name (~'machine-reference ~@ref) ~@clauses)
    `(~op ~name (~'machine-reference ~ref) ~@clauses)))


(defn machine-reference [n]
  (if (empty? (dissoc n :name :tag))
    (:name n)
    (map #(first (translato %)) (vals (dissoc n :tag)))))


;; special case databases

(def facts_special_lisb
  (pldb/db
   [db/override 'for-all #(= 4 (count %)) fa-sugar]
   [db/override '<-- #(keyword? (first (last %))) <--name]
   [db/override '<-- #(= 'op-call (first (last %))) <--call]
   [db/override 'refinement (constantly true) parse-machine-reference-lisb->ir]
   [db/override 'implementation (constantly true) parse-machine-reference-lisb->ir]))



(def facts_special_ir
  (pldb/db
   [db/override :op #(= % %) <--name-IR]
   [db/override :op-call #(contains? % :op) <--op-IR]
   [db/override :machine-reference (constantly true) machine-reference]
   [db/override :refinement (constantly true) parse-machine-reference-ir->lisb]
   [db/override :implementation (constantly true) parse-machine-reference-ir->lisb]))


;; translato

(defn translato-helper
  "helper relation for translating nested IR or lisb."
  [args] 
  (pldb/with-dbs [db/facts_args_ir db/facts_ops facts_special_lisb facts_special_ir] 
    (run 1 [res]
         (fresh [head tail rhead condition f]
            (conda
             [(emptyo args)
              (== res (empty args))] 
             
              [(pred args keyword?)
              (== res args)] 
             
             [(fresh [irop] 
                     (conde
                      [(pred args (complement sequential?))] 
                      [(pred args seq?)
                       (pred head keyword?)]
                      [(firsto args head)
                       (db/override head condition f)]
                      [(firsto args head)
                       (db/matches head irop)]) 
                     (firsto (translato args) res))] 
             
             [(conso head tail args)
              (fresh [temp-res] 
                     (project [head] (firsto (translato head) rhead))
                     (project [rhead tail] (conso rhead (helper tail) res)))])))))

(defn helper [args] (first (translato-helper args)))
(defn flatten-last
  "Flattens the last element of `c` if it is a sequence and not a lisb form."
  [c]
  (let [last (last c)] 
    (if (and (seqable? last) (not (symbol? (first last)))) (concat (drop-last c) last) c)))

(defn zip-with-rest 
  "Calls `zipmap` with `keys` and `values`. If `vals` is longer, the extra elements will be put into
   a list before calling `zipmap`."
  [keys vals] 
  (let [mk-count (- (count keys) 1) rest (drop mk-count vals)] 
    (if (< 1 (count rest))
      (assoc (zipmap keys (take mk-count vals)) (last keys) rest)
      (zipmap keys vals))))

(defn translato2lisb 
  "relation that translates IR to lisb. All arguments are logic variables."
  [lisb ir lisbop arg-keys]
  (fresh [key arg args] 
         (conde
          [(emptyo arg-keys)
           (== lisb lisbop)]
          
          [(fresh [args tail]
                  (== args (map ir arg-keys)) 
                  (project [args] (== tail (map helper args))) 
                  (project [tail](conso lisbop (flatten-last tail) lisb)))]))) 
                  

(defn translato2ir 
  "relation that translates lisb to IR. All arguments are logic variables."
  [lisb ir irop arg-keys] 
  (fresh [key arg args] 
         (condu
          [(emptyo arg-keys) 
           (conjo {:tag irop} ir)] 
          
          [(fresh [args tail]
                  (resto lisb args)
                  (project [args tail] (is tail arg-keys #(zip-with-rest % (map helper args))))
                  (project [tail] (conjo {:tag irop} tail ir)))]))) ;; conjo doesn't work with lvar as appended element in maps

(defn transform-lisb 
  "helper relation that transforms special lisb forms if certain condition is fulfilled. Refer to database
   `special_case_lisb`."
  [lisb newlisb lisbop lisbop2]
  (fresh [special? condition f]
        (conda
         [(db/override lisbop condition f)
          (project [condition] (== special? (condition lisb))) 
          (pred special? true?) 
          (project [f] (is newlisb lisb f)) 
          (project [newlisb] (== lisbop2 (first newlisb)))]
         [(pred lisbop keyword?) 
          (project [lisb lisbop]
                   (== newlisb (lisb-name lisb))
                   (== lisbop2 'op))]
         [(== newlisb lisb)
          (== lisbop2 lisbop)])))

(defn transform-ir
  "helper relation that transforms special IR forms if condition is fulfilled. Refer to database
   `special_case_ir`."
  [ir irop lisb]
  (fresh [special? condition f]
       (conde
        [(db/override irop condition f) 
         (project [condition] (== special? (condition ir)))
         (pred special? true?) 
         (project [f](== lisb (f ir)))]
        [fail])))


(defn pre-translato [lisb ir] 
  (fresh [l lisbop irop arg-keys arg newlisb lisbop2]
         (conde
          [(pred lisb primitive?)
           (== lisb ir)]

          [(pred ir primitive?)
           (== lisb ir)]
          
          [(pred lisb symbol?)
           (db/matches lisb irop)
           (== ir {:tag irop})]
          
          [(lvaro lisb)
           (== irop (:tag ir))
           (conde
            [(transform-ir ir irop lisb)]
            [(db/matches lisbop irop)
             (db/has-args irop arg-keys)
             (project [arg-keys] (translato2lisb lisb ir lisbop arg-keys))])]

          [(lvaro ir)
           (firsto lisb lisbop)
           (transform-lisb lisb newlisb lisbop lisbop2) 
           (db/matches lisbop2 irop) 
           (db/has-args irop arg-keys) 
           (project [arg-keys] (translato2ir newlisb ir irop arg-keys))])))
           