#lang racket

;1 n -> Número natural
(define (countdown n)
  (if (>= n 0)
      (cons n (countdown (- n 1)))
      empty))

;2 
(define (insertL x y lst)
  (cond
    [(empty? lst) empty]
    [else
     (let ([i (first lst)])
       (if (eqv? i x)
           (cons y (cons x (insertL x y (rest lst))))
           (cons i (insertL x y (rest lst)))))]))

;3
(define (remv-1st x lst)
  (cond
    [(empty? lst) empty]
    [else
     (let ([i (first lst)])
       (if (eqv? i x)
           (remv-1st empty (rest lst))
           (cons i (remv-1st x (rest lst)))))]))

;4
