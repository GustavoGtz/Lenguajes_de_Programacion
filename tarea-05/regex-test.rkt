#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")

(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (regexp-match? open-paren-regex "(")
              (not (regexp-match? open-paren-regex ")")))
   (test-case "close-paren-regex"
              (regexp-match? close-paren-regex ")")
              (not (regexp-match? close-paren-regex "(")))
   (test-case "define-regex"
              (regexp-match? define-regex "define")
              (not (regexp-match? define-regex "difine")))
   (test-case "sum-regex"
              (regexp-match? sum-regex "+")
              (not (regexp-match? sum-regex "-")))
   (test-case "mult-regex"
              (regexp-match? mult-regex "*")
              (not (regexp-match? mult-regex "/")))
   (test-case "identifier-regex"
              (regexp-match? identifier-regex "y")
              (regexp-match? identifier-regex "x1234")
              (regexp-match? identifier-regex "xyz5243")
              (not (regexp-match? identifier-regex "155"))
              (not (regexp-match? identifier-regex "a")))
   (test-case "number-regex"
              (regexp-match? number-regex "2")
              (regexp-match? number-regex "+26423")
              (regexp-match? number-regex "-25234")
              (not (regexp-match? number-regex "a")))))

(run-tests regex-tests 'verbose)
 
