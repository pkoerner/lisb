(ns lisb.examples.function-returns
  (:require [lisb.representation :refer :all]))


(def function-returns (bmachine
            (bmachine-variant)
            (bmachine-header :FunctionReturns [])
            (boperations
                (boperation [:a] :f1 [] (bprecondition (bmember? :a bnat-set) (bassign :a 1)))
                (boperation [:a :b] :f2 [] (bprecondition (band (bmember? :a bnat-set) (bmember? :b bint-set))
                                                                (bparallel-substitution (bassign :a 1) (bassign :b (b- 1))))))))
