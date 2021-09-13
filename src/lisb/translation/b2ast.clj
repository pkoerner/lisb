(ns lisb.translation.b2ast
  (:import (de.be4.classicalb.core.parser BParser)))

(defn b->ast [b] (.parse (BParser.) b false))
(defn b-formula->ast [b-formula] (.parseFormula (BParser.) b-formula))
(defn b-expression->ast [b-expression] (.parseExpression (BParser.) b-expression))
(defn b-substitution->ast [b-substitution] (.parseSubstitution (BParser.) b-substitution))
(defn b-predicate->ast [b-predicate] (.parsePredicate (BParser.) b-predicate))
(defn b-operation->ast [b-operation] (.parseTransition (BParser.) b-operation))
