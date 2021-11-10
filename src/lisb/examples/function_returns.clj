(ns lisb.examples.function-returns
  (:require [lisb.translation.lisb2ir :refer :all]))


(def function-returns (b (machine
                           :FunctionReturns
                           (operations
                             (<-- [:a] (:f1 [] (bprecondition (bmember? :a bnat-set) (bassign :a 1))))
                             (<-- [:a :b] (:f2 [] (bprecondition (band (bmember? :a bnat-set) (bmember? :b bint-set))
                                                                 (bparallel-sub (bassign :a 1) (bassign :b (b- 1))))))))))
