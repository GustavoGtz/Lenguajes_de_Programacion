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
  (if (or (equal? n 0) (equal? s ""))
      empty
      (cons (substring s 0 (if (< (string-length s) n) (string-length s) n) (partition (substring s n) n)))))

; Implementacion de bundle
(define (bundle s n)
  (cond
    [(equal? n 0) null] 
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

; 


(provide (all-defined-out))