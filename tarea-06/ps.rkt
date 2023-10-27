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

;;; take : list? integer? -> list
;;;; Agarra los primeros n (o los que alcanze) elementos de una lista.
;;;;; Se recorre la lista agarrando cada elemento hasta que la lista se acabe o n sea 0.
; Problema 2.1
(define (take l n)
  (if (or (empty? l) (equal? n 0))
      empty
      (cons (first l) (take (rest l) (sub1 n)))))

;;; drop : list? integer? -> list
;;;; Agarra los ultimos n (o una lista vacia si no alcanza) elementos de una lista.
;;;;; Se recorre la lista quitando cada elemento hasta que n sea 0 o la lista se acabe. 
; Problema 2.2
(define (drop l n)
  (if (or (empty? l) (equal? n 0))
      l
      (drop (rest l) (sub1 n))))

;;; bundle : (listof (and string? (equal? string-length 1))) integer? -> (listof string?)
;;;; Crea una lista que contiene de elementos trozos de la cadena s de tamaño n (o menores).
;;;;; Recorre la lista de cadenas unitarias, en cada iteracion agarra las primeras n y evalua el
;;;;; resto de la cadena hasta que la lista se quede vacia.
; Implementacion de bundle
(define (bundle s n)
  (cond
    [(equal? n 0) null] 
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

;;; list->chunks : (listof (and string? (equal? string-length 1))) integer? -> (listof (listof string?))
;;;; Crea una lista que contiene listas de tamaño a lo mas n que corresponden a los elementos de
;;;; la lista original pero divididos en fragmentos.
;;;;; Recorre la lista de cadenas unitarias, en cada iteracion agarra las primeras n y evalua el
;;;;; resto de la cadena hasta que la lista se quede vacia.
; Problema 5
(define (list->chunks l n)
  (if (equal? n 0)
      empty
      (if (empty? l)
          l
          (cons (take l n) (list->chunks (drop l n) n)))))

;;; bundle-chunk : (listof (listof string?)) integer? -> (listof string?)
;;;; Crea una lista que contiene de elementos trozos de la cadena s de tamaño n (o menores).
;;;;; Mapea cada elemento de la lista para que se convierta en un solo string.
(define (bundle-chunk s n)
  (map implode (list->chunks s n)))

;;; partition : string? integer? -> (listof string?)
;;;; Crea una lista que contiene de elementos trozos de la cadena s de tamaño n (o menores).
;;;;; Recorre los elementos de la lista con un paso de a lo mas n (cuando no alcanza se ajusta al maximo
;;;;; posible) y en cada iteracion se crea un nuevo elemento de la lista y se recorre el resto.
; Problema 6
(define (partition s n)
  (let ([l (string-length s)])
    (if (or (equal? n 0) (equal? l 0))
        empty
        (cons (substring s 0 (if (>= l n) n l)) (partition (substring s (if (>= l n) n l)) n)))))

;;; isort : (listof integer?) bool? -> (listof integer?)
;;;; Ordena los elementos de la lista en orden ascendente o descendiente dependiendo del valor de ord.
;;;;; El proceso de ordenamiento sigue una regla unitaria, en la que unicamente se concentra en tener una
;;;;; lista y poder meter un nuevo elemento que se ordene automaticamente, de esta forma al recorrer la lista
;;;;; se van metiendo todos los elementos de manera indiviudal para preservar este ordenamiento.
; Problema 7
; ord = #t or #f, if #t -> ascending order, else descending order 
(define (isort ls ord)
  (if (empty? ls)
      null
      (insert (first ls) (isort (rest ls) ord) ord)))

;;; insert : integer? (listof integer?) bool? -> (listof integer?)
;;;; Dada una lista, un tipo de ordenamiento (ascendnete o descendente), acomoda el nuevo elemento n en la posicion
;;;; que no afecte el ordenamiento de la lista.
;;;;; Recorre la lista comparando binariamente el nuevo elemnto con uno de la lista para checar si ahi esta su lugar
;;;;; o si por el contrario debe seguir buscando en el resto de la lista.
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
;;; quicksort : (listof integer?) bool? -> (listof integer?)
;;;; Ordena los elementos de la lista en orden ascendente o descendiente dependiendo del valor de ord.
;;;;; El proceso para ordenar sigue el principio de dividir para vencer, en este caso no se recorre la lista completa
;;;;; simplemente se toma el primer elemento (pivote) para posteriormente dividir la lista en dos nodos hijos, cuyos valores
;;;;; seran estrictamente menores o mayores al pivote, de esta forma, si se sigue un proceso recursivo al finalizar, se juntaran
;;;;; todas las ramas para obtener la lista ordenada.
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
;;; smallers : (listof integer?) integer? : (listof integer?)
;;;; Dada una lista y un filtro, se crea una nueva lista con los valores de la lista original que son estrictamente menores
;;;; al filtro.
;;;;; Se recorre la lista hasta que se acabe, durante el proceso si la condicion se cumple se van agregando elementos.
; Problema 9.1
(define (smallers ls filt)
  (if (empty? ls)
      null
      (let [(fs (first ls))]
        (if (< fs filt)
            (cons fs (smallers (rest ls) filt))
            (smallers (rest ls) filt)))))

;;; largers : (listof integer?) integer? : (listof integer?)
;;;; Dada una lista y un filtro, se crea una nueva lista con los valores de la lista original que son estrictamente mayores
;;;; o iguales al filtro.
;;;;; Se recorre la lista hasta que se acabe, durante el proceso si la condicion se cumple se van agregando elementos.
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

;;; smallers : (listof integer?) integer? : (listof integer?)
;;;; Dada una lista y un filtro, se crea una nueva lista con los valores de la lista original que son estrictamente menores
;;;; al filtro.
;;;;; Se utiliza la funcion filtro del propio racket.
; Problema 13.1
(define (smallers-filter ls filt)
  (filter (lambda (x) (< x filt)) ls))

;;; largers : (listof integer?) integer? : (listof integer?)
;;;; Dada una lista y un filtro, se crea una nueva lista con los valores de la lista original que son estrictamente mayores
;;;; o iguales al filtro.
;;;;; Se utiliza la funcion filtro del propio racket.
; Problema 13.2
(define (largers-filter ls filt)
  (filter (lambda (x) (>= x filt)) ls))

;;; quicksort : (listof integer?) bool? -> (listof integer?)
;;;; Ordena los elementos de la lista en orden ascendente o descendiente dependiendo del valor de ord.
;;;;; El proceso para ordenar sigue el principio de dividir para vencer, en este caso no se recorre la lista completa
;;;;; simplemente se toma el primer elemento (pivote) para posteriormente dividir la lista en dos nodos hijos, cuyos valores
;;;;; seran estrictamente menores o mayores al pivote, de esta forma, si se sigue un proceso recursivo al finalizar, se juntaran
;;;;; todas las ramas para obtener la lista ordenada.
; Implementacion de quicksort (Problema 11)
; ord = #t or #f, if #t -> ascending order, else descending order
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
    
