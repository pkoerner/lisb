(ns lisb.examples.function-returns
  (:require [lisb.translation.lisb2ir :refer :all]))


(def function-returns (bmachine
                        :FunctionReturns
                        (boperations
                          (boperation [:a] :f1 [] (bprecondition (bmember? :a bnat-set) (bassign :a 1)))
                          (boperation [:a :b] :f2 [] (bprecondition (band (bmember? :a bnat-set) (bmember? :b bint-set))
                                                                    (bparallel-sub (bassign :a 1) (bassign :b (b- 1))))))))
