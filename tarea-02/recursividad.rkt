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

;11 flst = first list, slst = second list
(define (same-lists* flst slst)
  (cond
    [(and (empty? flst) (empty? slst)) #t]
    [(or (empty? flst) (empty? slst)) #f]
    [else
     (let ([x (first flst)]
           [y (first slst)])
       (cond
         [(and (list? x) (list? y)) (cond
                                      [(same-lists* x y) (same-lists* (rest flst) (rest slst))]
                                      [else #f])]
         [(eqv? x y) (same-lists* (rest flst) (rest slst))]
         [else #f]))]))

;12
(equal? '((w x) y (z)) '((w . (x . ())) y (z . ())))


;13 blst = binary list
(define (binary->natural blst)
  (cond
    [(empty? blst) 0]
    [else (+ (if (eqv? (first blst) 1) 1 0) (binary->natural (rest blst)) (binary->natural (rest blst)))]))

;14 num = numerator, den = denominator
(define (div num den)
  (cond
    [(eqv? num 0) 0]
    [(< num 0) -666]
    [else (+ 1 (div (- num den) den))]))

;15 flst = first list, slst = second list
(define (append-map func lst)
  (cond
    [(empty? lst) empty]
    [else (append (func (first lst)) (append-map func (rest lst)))]))

;16
(define (set-difference flst slst)
  (cond
    [(empty? flst) empty]
    [else
     (let ([x (first flst)])
       (cond
         [(eqv? -1 (list-index-ofv x slst)) (cons x (set-difference (rest flst) slst))]
         [else (set-difference (rest flst) slst)]))]))
         

;17 func must be a binary operation, acumulator must be compatible with the function and the list
(define (foldr func acumulator lst)
  (cond
    [(empty? lst) acumulator]
    [else (func (first lst) (foldr func acumulator (rest lst)))]))

;18
(define (powerset lst)
  (if (empty? lst)
      '(())
      (let ([e (first lst)]
            [t (rest lst)])
        (append (for/list ([i (powerset t)]) (append (list e) i)) (powerset t)))))

;19 lst must be a list of list of numbers
(define (cartesian-product lst)
  (if (empty? (first lst))
      empty
      (let ([i (first (first lst))])
        (append (map (lambda (j) (list i j)) (second lst)) (cartesian-product (list (rest (first lst)) (second lst)))))))

;20

;20.1
(define (insertL-fr x y lst)
  (foldr (lambda (a b) (if (eqv? a x) (cons y (cons x b)) (cons a b))) empty lst))

;20.2
(define (filter-fr f lst)
  (foldr (lambda (a b) (if (f a) (cons a b) b)) empty lst))

;20.3
(define (map-fr f lst)
  (foldr (lambda (a b) (cons (f a) b)) empty lst))

;20.4
(define (append-fr flst slst)
  (foldr (lambda (a b) (cons a b)) slst flst))

;20.5
(define (reverse-fr lst)
  (foldr (lambda (a b) (cons b a)) empty lst))

;20.6
(define (binary->natural-fr lst)
  (foldr (lambda (a b) (+ a (* 2 b))) 0 lst))

;20.7
(define (append-map-fr f lst)
  (foldr (lambda (a b) (cons (f a) b)) empty lst))

;20.8
(define (set-difference-fr flst slst)
  (foldr (lambda (a b) (if (eqv? -1 (list-index-ofv a slst)) (cons a b) b)) empty flst))

;20.9
(define (powerset-fr lst)
  (foldr (lambda (a b) (append-map (lambda (i) (list (cons a i) i)) b)) (list (list)) lst))

;21
(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (odd? x) (snowball (add1 (* x 3)))))
               (else (fix-odd x))))))
        (even-case
         (lambda (fix-even)
           (lambda (x)
             (cond
               ((and (exact-integer? x) (positive? x) (even? x)) (snowball (/ x 2)))
               (else (fix-even x))))))
        (one-case
         (lambda (fix-one)
           (lambda (x)
             (cond
               ((zero? (sub1 x)) 1)
               (else (fix-one x))))))
        (base
         (lambda (x)
           (error 'error "Invalid value s n" x))))
    (one-case (even-case (odd-case base)))))

;22

(define quine "Dijo el gato con los pies de trapo y los ojos alrevez")



  


       
       


