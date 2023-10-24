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

; Implementacion de quicksort (Problema 11)
; ord = #t or #f, if #t -> ascending order, else descending order
(define (quicksort ls ord)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (define notpivot (rest ls))
     (if (equal? ord #t)
         (append (quicksort (smallers notpivot pivot) ord)
                 (list pivot)
                 (quicksort (largers notpivot pivot) ord))
                 
         (append (quicksort (largers notpivot pivot) ord)
                 (list pivot)
                 (quicksort (smallers notpivot pivot) ord)))]))

; Problema 9.1
(define (smallers ls filt)
  (if (empty? ls)
      null
      (let [(fs (first ls))]
        (if (< fs filt)
            (cons fs (smallers (rest ls) filt))
            (smallers (rest ls) filt)))))

; Problema 9.2
(define (largers ls filt)
  (if (empty? ls)
      null
      (let [(fs (first ls))]
        (if (>= fs filt)
            (cons fs (largers (rest ls) filt))
            (largers (rest ls) filt)))))

; Problema 12
(define (maliya-sort ls ord)
  (define umb 5)
  (if (> (length ls) umb)
      (quicksort ls ord)
      (isort ls ord)))

; Problema 13.1
(define (smallers-filter ls filt)
  (filter (lambda (x) (< x filt)) ls))

; Problema 13.2
(define (largers-filter ls filt)
  (filter (lambda (x) (>= x filt)) ls))

; Problema 14
; ord = #t or #f, if #t -> ascending order, else descending order
(define (quicksort-filter ls ord)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (define notpivot (rest ls))
     (if (equal? ord #t)
         (append (quicksort-filter (filter (lambda (x) (< x pivot)) notpivot) ord)
                 (list pivot)
                 (quicksort-filter (filter (lambda (x) (>= x pivot)) notpivot) ord))
                 
         (append (quicksort-filter (filter (lambda (x) (>= x pivot)) notpivot) ord)
                 (list pivot)
                 (quicksort-filter (filter (lambda (x) (< x pivot)) notpivot) ord)))]))

(provide (all-defined-out))
    
