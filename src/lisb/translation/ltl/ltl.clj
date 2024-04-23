(ns lisb.translation.ltl.ltl)

;; TODO: This is a quick experiment that does not cover all operators.
;; The code is thus small enough that it is not split into several namespaces.
;; This should be addressed once it grows to cover the entirety of LTL.

;; Code to inspect Java AST
;(import de.be4.ltl.core.parser.LtlParser)
;
;(defn call-method
;  [obj method-name & args]
;  (let [m (first (filter (fn [x] (.. x getName (equals method-name)))
;                         (.. obj getClass getDeclaredMethods)))]
;    (. m (setAccessible true))
;    (. m (invoke obj (into-array Object args)))))
;
;(def node (call-method (LtlParser. (de.be4.classicalb.core.parser.ClassicalBParser.)) "parseFormula" "G {x = 4}"))
;
;(require '[clojure.reflect :as cr])
;(require '[clojure.pprint :as pp])
;
;(defn inspect [x]
;  (->> (class x)
;     cr/reflect
;     :members
;     (filter #(contains? (:flags %) :public))
;     pp/print-table))
;(inspect node)
;(.getPLtl node) ; #object[de.be4.ltl.core.parser.node.AGloballyLtl 0x19bd745d "x = 4 "]
;(.getLtl (.getPLtl node)) ; #object[de.be4.ltl.core.parser.node.AUnparsedLtl 0x559c97b9 "x = 4 "]
;(.getPredicate (.getLtl (.getPLtl node))) ;  #object[de.be4.ltl.core.parser.node.TAtomicPropositionBegin 0x57af8209 "x = 4 "]
;(inspect (.getLtl (.getPLtl node)))
;(inspect (.getPredicate (.getLtl (.getPLtl node))))

;; unfinished list of imports of Java AST nodes
(import [de.be4.ltl.core.parser.node
           Start
           EOF
           AGloballyLtl
           AFinallyLtl
           ANextLtl
           AUntilLtl
           AAndLtl
           AImpliesLtl
           ANotLtl
           AOrLtl
           AUnparsedLtl
           TAtomicPropositionBegin
         ])


;; generate IR
(defn globally [x] {:tag :globally, :ltl x})
(defn next [x] {:tag :next, :ltl x})
(defn finally [x] {:tag :finally, :ltl x})
(defn until [x y] {:tag :until, :lhs x, :rhs y})

(defn ltl-and [x y] {:tag :and, :lhs x, :rhs y})
(defn ltl-or [x y] {:tag :or, :lhs x, :rhs y})
(defn negate [x] {:tag :not, :ltl x})
(defn implies [x y] {:tag :implies, :lhs x, :rhs y})

;; translate IR to Java AST
(defmulti ltl->ast-dev2 (fn [x] (or (:tag x) (class x))))
(defmethod ltl->ast-dev2 String [x] (AUnparsedLtl. (TAtomicPropositionBegin. x)))
(defmethod ltl->ast-dev2 :globally [x] (AGloballyLtl. (ltl->ast-dev2 (:ltl x))))
(defmethod ltl->ast-dev2 :finally [x] (AFinallyLtl. (ltl->ast-dev2 (:ltl x))))
(defmethod ltl->ast-dev2 :next [x] (ANextLtl. (ltl->ast-dev2 (:ltl x))))
(defmethod ltl->ast-dev2 :until [x] (AUntilLtl. (ltl->ast-dev2 (:lhs x))
                                                (ltl->ast-dev2 (:rhs x))))

(defmethod ltl->ast-dev2 :and [x] (AAndLtl. (ltl->ast-dev2 (:lhs x))
                                            (ltl->ast-dev2 (:rhs x))))
(defmethod ltl->ast-dev2 :or [x] (AOrLtl. (ltl->ast-dev2 (:lhs x))
                                           (ltl->ast-dev2 (:rhs x))))
(defmethod ltl->ast-dev2 :not [x] (ANotLtl. (ltl->ast-dev2 (:ltl x))))
(defmethod ltl->ast-dev2 :implies [x] (AImpliesLtl. (ltl->ast-dev2 (:lhs x))
                                                    (ltl->ast-dev2 (:rhs x))))
(defmethod ltl->ast-dev2 :default [x] (println x))

(defn ltl->ast [x]
  (Start. (ltl->ast-dev2 x) (EOF.)))

;; pretty print because ProB's LTL parser generates Prolog terms
(defmulti pp type)
(defmethod pp Start [x] (pp (.getPLtl x)))
(defmethod pp AGloballyLtl [x] (str "☐(" (pp (.getLtl x)) ")"))
(defmethod pp AFinallyLtl [x] (str "◇(" (pp (.getLtl x)) ")"))
(defmethod pp ANextLtl [x] (str "○(" (pp (.getLtl x)) ")"))
(defmethod pp AUntilLtl [x] (str "(" (pp (.getLeft x)) ") U (" (pp (.getRight x)) ")"))
(defmethod pp AAndLtl [x] (str "(" (pp (.getLeft x)) ") ⋀ (" (pp (.getRight x)) ")"))
(defmethod pp AImpliesLtl [x] (str "(" (pp (.getLeft x)) ") => (" (pp (.getRight x)) ")"))
(defmethod pp ANotLtl [x] (str "¬(" (pp (.getLtl x))")"))
(defmethod pp AOrLtl [x] (str "(" (pp (.getLeft x)) ") ⋁ (" (pp (.getRight x)) ")"))
(defmethod pp AUnparsedLtl [x] (pp (.getPredicate x)))
(defmethod pp TAtomicPropositionBegin [x]  (.getText x))

;; define unicode operators so it aligns with Dwyer et al.'s paper
(def ☐ globally)
(def ◇ finally)
(def ○ next)
(def U until)
(def => implies)
(def ⋀ ltl-and)
(def ⋁ ltl-or)
(def ¬ negate)

;; some Dwyer patterns
(defn dwyer-s-responds-p-between-q-and-r [S P Q R]
  (☐ (U (=> (⋀ Q (○ (◇ R)))
            (=> P (U (¬ R)
                     (⋀ S (¬ R)))))
        R)))

(defn dwyer-s-responds-p-before-r [S P R]
  (U (=> P
         (U (¬ R)
            (⋀ S (¬ R))))
     (⋁ R (☐ (¬ R)))))

(defn dwyer-s-responds-p-globally [S P]
  (☐ (=> P (◇ S))))

;; a tad of a DSL
(defn dwyer-response [S P & [opt-kw opt-Q _opt-kw2 opt-R]]
  (cond (not opt-kw) (dwyer-s-responds-p-globally S P)
        (= opt-kw :between) (dwyer-s-responds-p-between-q-and-r S P opt-Q opt-R)
        (= opt-kw :before) (dwyer-s-responds-p-before-r S P opt-Q)
        :else :not-implemented))

(defn dwyer [tag & args]
  (case tag
    :response (pp (ltl->ast (apply dwyer-response args)))))

;; example calls: pretty print the generated Java ASTs for the formulas
(pp (ltl->ast (dwyer-response "x=1" "y=2")))
(pp (ltl->ast (dwyer-response "x=1" "y=2" :before "a=2")))
(pp (ltl->ast (dwyer-response "x=1" "y=2" :between "a=3" :and "b=42")))

(dwyer :response "x=1" "y=2")
"☐((y=2) => (◇(x=1)))"
(dwyer :response "x=1" "y=2" :before "a=2")
"((y=2) => ((¬(a=2)) U ((x=1) ⋁ (¬(a=2))))) U ((a=2) ⋁ (☐(¬(a=2))))"
(dwyer :response "x=1" "y=2" :between "a=3" :and "b=42")
"☐((((a=3) ⋁ (○(◇(b=42)))) => ((y=2) => ((¬(b=42)) U ((x=1) ⋁ (¬(b=42)))))) U (b=42))"
