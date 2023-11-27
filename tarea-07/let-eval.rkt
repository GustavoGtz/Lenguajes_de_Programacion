#lang racket/base

(require racket/contract
         racket/match
         "let-ast.rkt"
         "let-vals.rkt"
         "let-env.rkt")

(define (value-of-program pgm)
  (match pgm
    [(a-program exp1)
     (value-of exp1 (init-env))]
    [_
     (error (format "Expected program but got ~a" pgm))]))

(define (printval str)
  (match str
    [(num-val x) (printf "~a" x)]
    [(bool-val x) (printf "~a" (if x "true" "false"))]
    [(pair-val x)
     (printf "(")
     (printval (car x))
     (printlist (cdr x))
     (printf ")")]
    [(null-val) (printf "()")]))

(define (printlist val)
  (match val
    [(null-val) (printf "")]
    [(pair-val pair)
     (printf " ")
     (printval (car pair))
     (printlist (cdr pair))]
    [_
     (printf " . ")
     (printval val)]))

; returns a pair: car = function, cdr = No. Args
(define (op-table id)
  (match id
    ['- (cons (lambda (x y) (num-val (- (expval->num x) (expval->num y)))) 2)]
    ['+ (cons (lambda (x y) (num-val (+ (expval->num x) (expval->num y)))) 2)]
    ['* (cons (lambda (x y) (num-val (* (expval->num x) (expval->num y)))) 2)]
    ['zero? (cons (lambda (x) (bool-val (zero? (expval->num x)))) 1)]))

(define (value-of exp env)
  (match exp
    [(const-exp num)
     (num-val num)]
    [(var-exp var)
     (apply-env env var)]
;    [(diff-exp exp1 exp2)
;     (num-val (- (expval->num (value-of exp1 env))
;                 (expval->num (value-of exp2 env))))]
;    [(zero?-exp exp1)
;     (bool-val (zero? (expval->num (value-of exp1 env))))]
    [(if-exp exp1 exp2 exp3)
     (if (expval->bool (value-of exp1 env))
         (value-of exp2 env)
         (value-of exp3 env))]
    [(let-exp var exp1 body)
     (value-of body (extend-env var (value-of exp1 env) env))]
    [(null?-exp exp1)
     (bool-val (equal? (value-of exp1 env) null-val))]
    [(cons-exp exp1 exp2)
     (let ([val1 (value-of exp1 env)]
           [val2 (value-of exp2 env)])
       (pair-val (cons val1 val2)))]
    [(car-exp exp1)
     (let ([val (value-of exp1 env)])
       (match val
         [(pair-val x) (car x)]))]
    [(cdr-exp exp1)
     (let ([val (value-of exp1 env)])
       (match val
         [(pair-val x) (cdr x)]))]
    [(emptylist-exp)
     (null-val)]
    [(list-exp exps)
     (define (list-reader lst)
       (if (equal? '() lst)
           (null-val)
           (let ([x (value-of (car lst) env)])
             (pair-val (cons x (list-reader (cdr lst)))))))
     (list-reader exps)]
    [(unpack-exp ids exp1 exp2)
     (define (aux-value-of idlst explst exp env)
       (if (null? idlst)
           (if (null-val? explst)
               (value-of exp env)
               (error (format "The number of elements in the list does not match the required")))
           (if (null-val? explst)
               (error (format "The number of elements in the list does not match the required"))
               (let [(id (car idlst))
                     (val (car (pair-val-x explst)))
                     (expsrst (cdr (pair-val-x explst)))]
                 (aux-value-of (cdr idlst) expsrst exp (extend-env id val env))))))
     (let ([expslst (value-of exp1 env)])
       (if (or (null-val? expslst) (pair-val? expslst))
           (aux-value-of ids expslst exp2 env)
           (error (format "Expected a list but got something else"))))
     ]
    [(print-exp exp1)
     (printf "\"")
     (printval (value-of exp1 env))
     (printf "\"\n")
     (num-val 1)]
    [(opcall-exp op args)
     (let* ([matching-op (op-table op)]
            [func (car matching-op)]
            [min-args (cdr matching-op)]
            [eval-args (map (lambda (y) (value-of y env)) args)])
       (if (equal? (length args) min-args)
           (apply func eval-args)
           (error (format "The number of elements arguments does not match the required"))))]
    [_
     (error (format "Expected expression but got ~a" exp))]))

(provide
 (contract-out
  [value-of-program (-> a-program? expval?)]
  [value-of (-> expression? environment? expval?)]))
