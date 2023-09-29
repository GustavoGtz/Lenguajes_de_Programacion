#lang racket

(define open-paren-regex #px"\\(")

(define close-paren-regex #px"\\)")

(define define-regex #px"define")

(define sum-regex #px"\\+")

(define mult-regex #px"\\*")

(define identifier-regex #px"[xyz][xyz[:digit:]]*")

(define number-regex #px"[\\+-]?[[:digit:]]+")


(provide open-paren-regex
         close-paren-regex
         define-regex
         sum-regex
         mult-regex
         identifier-regex
         number-regex)
