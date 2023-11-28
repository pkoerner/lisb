(ns lisb.dsls.infix
  (:require [lisb.translation.lisb2ir :refer :all]))

;; Implementation follows https://en.wikipedia.org/wiki/Operator-precedence_parser

(declare parse-n parse-primary parse-expression parse-expression-1)

(def operator? #{'+ '* '/ '- 'pow})
(def precedence {'+ 2, '- 2, '/ 3, '* 3, 'pow 4})
(def left-associative? #{'+ '* '/ '-})
(def function? #{'max 'sin})
(def function-arity {'max 2, 'sin 1})



(defn parse-n [tokens n]
  (loop [tokens tokens
         exprs []
         n n]
    (if (zero? n)
      {:exprs exprs, :rest tokens}
      (let [res (parse-expression tokens)]
        (assert (seq tokens) "not enough arguments")
        (recur (:rest res) (conj exprs (:expr res)) (dec n))))))

(defn parse-primary [tokens]
  (cond
    (list? (first tokens))
      {:expr (:expr (parse-expression (first tokens))),
       :rest (rest tokens)}
    (function? (first tokens)) ;; check for aliases
      (let [res (parse-n (first (rest tokens)) (get function-arity (first tokens)))]
        (assert (empty? (:rest res)) (str "too many arguments to function " (first tokens)))
        {:expr (cons (first tokens) (:exprs res))
         :rest (drop 2 tokens)})
    :otherwise {:expr (first tokens), :rest (rest tokens)}))

(defn parse-expression [tokens]
  (let [primary (parse-primary tokens)]
    (parse-expression-1 (:rest primary)
                        (:expr primary)
                        0)))

;; this function is a mess
;; OTOH it is a direct, functional implementation of the operator precedence parser algorithm
(defn parse-expression-1 [tokens lhs min-precedence]
  (let [lookahead (first tokens)]
    (if (and (operator? lookahead)
             (>= (precedence lookahead) min-precedence))
      (let [{tokens :tokens,
             rhs :rhs}
            (let [op lookahead
                  tokens (rest tokens)
                  primary (parse-primary tokens)]
              (loop [tokens (:rest primary)
                     rhs (:expr primary)
                     lookahead (first tokens)]
                (if (or
                      (and (operator? lookahead)
                           (not (left-associative? lookahead))
                           (= (precedence lookahead) (precedence op)))
                      (and (operator? lookahead)
                           (>= (precedence lookahead) (precedence op))))
                  (let [parsed (parse-expression-1 tokens rhs (if (> (precedence lookahead) (precedence op))
                                                                (inc (precedence op))
                                                                (precedence op)))]
                    (recur (:rest parsed) 
                           (:expr parsed)
                           (first (:rest parsed))))
                  {:tokens tokens, :rhs rhs})))]
        (recur tokens, (list (get aliases lookahead lookahead), lhs, rhs) min-precedence))
      {:expr lhs, :rest tokens})))

(defmacro infix [x]
  (let [res (:expr (parse-expression x))]
    `(quote ~res)))

; (parse-expression '(3 + 4 * 2))
; (parse-expression '(3 + 4 * 2 / (1 - 5) pow 2 pow 3))
; (parse-expression '(3 + max (sin ( 4) 5) * sin(2) / (1 - 5) pow 2 pow 3))
; (parse-expression '( (1 - 5) + 1))
; 
; (infix (3 + max (sin ( 4) 5) * sin(2) / (1 - 5) pow 2 pow 3))
; (eval (infix (3 + 2 - 1)))


;; TODO: couple (a,b) 
;; TODO: unary minus
;; TODO: reverse expression (...)~
;; TODO: image [...]

		; prio.put(AParallelProductExpression.class, 20);
		;; prio.put(ARingExpression.class, 160);
		; prio.put(ARestrictFrontExpression.class, 160); /|\
		; prio.put(ARestrictTailExpression.class, 160); \|/
		; prio.put(ACoupleExpression.class, 160);
		; prio.put(ASetSubtractionExpression.class, 180);
		; prio.put(AMultiplicationExpression.class, 190); ;; a priority for MultiplicationExpression is defined, but no case
		;prio.put(AReverseExpression.class, 230);
		; prio.put(ARecordFieldExpression.class, 231); a'b ;; cannot be written in Clojure
		; prio.put(AFunctionExpression.class, 231); ;; f(..., ..., ...)

(def b-priorities
  {'=> 30,
   'or 40, 'and 40,
   '<=> 60,
   '+-> 125, '--> 125, '+->> 125, '-->> 125, '>+> 125, '>-> 125, '>+>> 125, '>->> 125, '<-> 125, '<<-> 125, '<<->> 125, '<->> 125
   '>< 160, '<+ 160, '<| 160, '<<| 160, '|> 160, '|>> 160, '-> 160, '<- 160,
   'concat 160, ;; ^ is not a symbol in Clojure
   'U 160, ;; \/ cannot be written in Clojure (it can be, but it is not a symbol) 
   'cap 160, ;; /\ cannot be written in Clojure
   '.. 170,
   '- 180, '+ 180,
   '* 190, '/ 190, 'mod 190,
   '** 200, ;; TODO: right-associative
   })

bimplies
(def aliases
  {
;           ; records
;           ~'struct bstruct
;           ~'record brecord
;           ~'record-get brecord-get

           ; sequences
           'concat bconcat
           '-> bprepend                                    ; sugar
           '<- bappend                                     ; sugar

           ; functions
           '+-> bpartial-function                          ; sugar
           '--> btotal-function
           '+->> bpartial-surjection
           '-->> btotal-surjection
           '>+> bpartial-injection
           '>-> btotal-injection
           '>+>> bpartial-bijection
           '>->> btotal-bijection

           ; relations
           '<-> brelation                                  ; sugar
           '<<-> btotal-relation                           ; sugar
           '<->> bsurjective-relation                      ; sugar
           '<<->> btotal-surjective-relation               ; sugar
           '|-> bmaplet                                    ; sugar
           '<| bdomain-restriction                         ; sugar
           '<<| bdomain-subtraction                        ; sugar
           '|> brange-restriction                          ; sugar
           '|>> brange-subtraction                         ; sugar
           '<+ boverride                                   ; sugar
           '>< bdirect-product                             ; sugar

           ; numbers
           '.. brange
           '> b>
           '< b<
           '>= b>=
           '<= b<=
           'max bmax
           'min bmin
           '+ b+
           '- b-
           '* bcart-or-mult
           'div bdiv
           '/ bdiv
           '** b**
           'mod bmod

           ;;; sets
           'U bunion
           'cap bintersection
           ; ~'set- bset-
           ; 'member? bmember?
           ; '= b=

           ;;; logical predicates
           'and band
           'or bor
           '<=> bequivalence                               ; sugar
           '=> bimplication                                ; sugar
   }
  )
