#lang racket/base

(require racket/contract)

(struct a-program (exp1) #:transparent)
(struct expression () #:transparent)
(struct const-exp expression (num) #:transparent)
(struct if-exp expression (exp1 exp2 exp3) #:transparent)
(struct var-exp expression (var) #:transparent)
(struct let-exp expression (bindings body) #:transparent)
(struct null?-exp expression (exp1) #:transparent)
(struct cons-exp expression (exp1 exp2) #:transparent)
(struct car-exp expression (exp1) #:transparent)
(struct cdr-exp expression (exp1) #:transparent)
(struct emptylist-exp expression () #:transparent)
(struct list-exp expression (exps) #:transparent)
(struct unpack-exp expression (ids exp1 exp2) #:transparent)
(struct print-exp expression (exp1) #:transparent)
(struct cond-exp expression (clauses) #:transparent)
(struct let*-exp expression (bindings body) #:transparent)
(struct opcall-exp expression (op args) #:transparent)

(provide
 expression?
 (contract-out
  [struct a-program ((exp1 expression?))]
  [struct const-exp ((num integer?))]
  [struct if-exp ((exp1 expression?) (exp2 expression?) (exp3 expression?))]
  [struct var-exp ((var symbol?))]
  [struct let-exp ((bindings (listof (cons/c symbol? expression?))) (body expression?))]
  [struct null?-exp ((exp1 expression?))]
  [struct cons-exp ((exp1 expression?) (exp2 expression?))]
  [struct car-exp ((exp1 expression?))]
  [struct cdr-exp ((exp1 expression?))]
  [struct emptylist-exp ()]
  [struct list-exp ((exps (listof expression?)))]
  [struct unpack-exp ((ids (listof symbol?)) (exp1 expression?) (exp2 expression?))]
  [struct print-exp ((exp1 expression?))]
  [struct cond-exp ((clauses (listof (cons/c expression? expression?))))]
  [struct let*-exp ((bindings (listof (cons/c symbol? expression?))) (body expression?))]
  [struct opcall-exp ((op symbol?) (args (listof expression?)))]))
