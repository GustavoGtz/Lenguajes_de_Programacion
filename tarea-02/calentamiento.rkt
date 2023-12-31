#lang racket
; 1.
(define pi 3.14)

; 2.
(define (area-circle radius) (* pi (* radius radius)))

; 3.
(define (perimeter-circle radius) (* 2 pi radius))
(define (circle-properties radius) (list (area-circle radius) (perimeter-circle radius)))

;4 lst -> (width height)
(define (area-rectangle width height) (* width height))
(define (perimeter-rectangle width height) (+ (* 2 width) (* 2 height)))
(define (rectangle-properties lst)
  (list (area-rectangle (list-ref lst 0) (list-ref lst 1))
        (perimeter-rectangle (list-ref lst 0) (list-ref lst 1))))

;5
(define (find-needle lst)
  (define (iter lst index)
    (cond [(empty? lst) -1]
          [else (let ([x (first lst)])
                (cond [(eq? x 'needle) index]
                [(eq? x 'hay) (iter (rest lst) (+ index 1))]
                [else 'error]))]))
  (iter lst 0))

;6
(define (abs x) (if (< x 0) (* x -1) x))

;7
;(define (increase-all-one lst)
;  (cond [(empty? lst) empty]
;        [else (cons (+ (first lst) 1)
;                   (increase-all-one (rest lst)))]))
(define (inclis1 lst)
  (map (lambda (number) (+ 1 number)) lst))
;(define lst '(1 2 3))
;(map (lambda (number ) (+ 1 number)) lst)

;8
(define (even? x)
  (define (mod a b)
    (if (< a b)
      a
      (mod (- a b) b)))
  (if (equal? (mod x 2) 0)
      #t
      #f))

;9
(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else (add1 (another-add m (sub1 n)))])))


  
  