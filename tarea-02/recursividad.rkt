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
(define (map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst))
                (map f (rest lst)))]))

;5 f returns a boolean.
(define (filter f lst)
  (cond
    [(empty? lst) empty]
    [else (let ([x (first lst)])
            (cond
              [(f x) (cons x (filter f (rest lst)))]
              [else (filter f (rest lst))]))]))

;6 flst = first list, slst = second list
(define (zip flst slst)
  (cond
    [(empty? flst) empty]
    [(empty? slst) empty]
    [else (cons (cons (first flst) (first slst))
                (zip (rest flst) (rest slst)))]))

;7
(define (list-index-ofv x lst)
  (define (iter x lst index)
    (cond
      [(empty? lst) -1]
      [else
       (cond
         [(eq? x (first lst)) index]
         [else (iter x (rest lst) (add1 index))])]))
  (iter x lst 0))

;8 flst = first list, slst = second list
(define (append flst slst)
  (cond
    [(empty? flst)
     (cond
       [(empty? slst) empty]
       [else (cons (first slst) (append flst (rest slst)))])]
    [else (cons (first flst) (append (rest flst) slst))]))

;9 drop-right with index 1 always delete the last element in the list.
(define (reverse lst)
  (cond
    [(empty? lst) empty]
    [else (cons (last lst) (reverse (drop-right lst 1)))]))

;10
(define (repeat lst n)
  (cond
    [(eqv? 0 n) empty]
    [else (append lst (repeat lst (sub1 n)))]))

;11
