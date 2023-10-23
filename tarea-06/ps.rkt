#lang racket

; Funciones auxiliares
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (car x))
           (= (string-length (car x)) 1)
           (unit-string-list? (cdr x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

; Problema 2.1
(define (take l n)
  (if (or (empty? l) (equal? n 0))
      empty
      (cons (first l) (take (rest l) (sub1 n)))))

; Problema 2.2
(define (drop l n)
  (if (or (empty? l) (equal? n 0))
      l
      (drop (rest l) (sub1 n))))

; Implementacion de bundle
(define (bundle s n)
  (cond
    [(equal? n 0) null] 
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

; Problema 5
(define (list->chunks l n)
  (if (equal? n 0)
      empty
      (if (empty? l)
          l
          (cons (take l n) (list->chunks (drop l n) n)))))

(define (bundle-chunk s n)
  (map implode (list->chunks s n)))

; Problema 6

(define (partition s n)
  (let ([l (string-length s)])
    (if (or (equal? n 0) (equal? l 0))
        empty
        (cons (substring s 0 (if (>= l n) n l)) (partition (substring s (if (>= l n) n l)) n)))))

; Problema 7
; ord = #t or #f, if #t -> ascending order, else descending order 
(define (isort ls ord)
  (if (empty? ls)
      null
      (insert (first ls) (isort (rest ls) ord) ord)))

(define (insert n ls ord)
  (cond
    [(empty? ls) (list n)]
    [(equal? ord #t)
     (if (< n (first ls))
         (cons n ls)
         (cons (first ls) (insert n (rest ls) ord)))]
    [(equal? ord #f)
     (if (>= n (first ls))
         (cons n ls)
         (cons (first ls) (insert n (rest ls) ord)))]))

; Implementacion de quicksort
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (list pivot)
             (quicksort (largers ls pivot)))]))
; Problema 9.1
(define (smallers ls) 0)
(define (largers ls) 0)
; Problema 9.2

(provide (all-defined-out))
    
    