#lang racket
;; Implementacion de una gramatica sobre un lenguaje aritmetico.

(define identifier-regex #px"[xyz][xyz[:digit:]]*")

(define (empty-env)
    '())
    
(define (apply-env env var)
    (if (empty? env)
        (error "Var no definida")
        (let ((fs (car env)))
            (if (eq? (car fs) var)
                (cdr fs)
                (apply-env (rest env) var)))))
              
 (define (extend-env var val env)
    (cons (cons var val) env))

; Prog -> (Exp)
;      |  (Def . Prog)
(define (eval-prog prog env)
  (if (list? prog)
      (if (cons? prog)
          (let ([env* (eval-def (car prog))])
           (eval-prog (cdr prog) env*))
          (error "error"))
      (error "error")))

; Def -> (define Var Exp)
(define (eval-def def env)
  (cond
    [(and (list? def)
         (= (length def) 3)
         (eq? (first def) 'define)
         (regexp-match-exact? identifier-regex (second def)))
     (let ([var* (second def)]
           [val* (eval-exp (third def) env)])
       (extend-env var* val* env))]
    [else (error "Error")]))

; Exp -> Int
;     |  (+ Exp Exp)
;     |  (* Exp Exp)
(define (eval-exp exp env)
  (cond
    [(integer? exp) exp]
    [(symbol? exp) (apply-env env exp)]
    [(and (list? exp)
          (= (length exp) 3)
          (eq? (first exp) '+))
     (let* ([exp1 (second exp)]
            [exp2 (third exp)]
            [x1 (eval-exp exp1 env)]
            [x2 (eval-exp exp2 env)])
       (+ x1 x2))]
    [(and (list? exp)
          (= (length exp) 3)
          (eq? (first exp) '*))
     (let* ([exp1 (second exp)]
            [exp2 (third exp)]
            [x1 (eval-exp exp1 env)]
            [x2 (eval-exp exp2 env)])
       (* x1 x2))]
    [else (error "Error")]))
