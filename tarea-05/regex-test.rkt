#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")
(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-true (regexp-match? open-paren-regex "("))
              (check-false (regexp-match? open-paren-regex ")")))
   (test-case "close-paren-regex"
              (check-true (regexp-match? close-paren-regex ")"))
              (check-false (regexp-match? close-paren-regex "(")))
   (test-case "define-regex"
              (check-true (regexp-match? define-regex "define"))
              (check-false (regexp-match? define-regex "difine")))
   (test-case "sum-regex"
              (check-true (regexp-match? sum-regex "+"))
              (check-false (regexp-match? sum-regex "-")))
   (test-case "mult-regex"
              (check-true (regexp-match? mult-regex "*"))
              (check-false (regexp-match? mult-regex "/")))
   (test-case "identifier-regex"
              (check-true (regexp-match? identifier-regex "y"))
              (check-true (regexp-match? identifier-regex "x1234"))
              (check-true (regexp-match? identifier-regex "xyz5243"))
              (check-false (regexp-match? identifier-regex "155"))
              (check-false (regexp-match? identifier-regex "a")))
   (test-case "number-regex"
              (check-true (regexp-match? number-regex "2"))
              (check-true (regexp-match? number-regex "+26423"))
              (check-true (regexp-match? number-regex "-25234"))
              (check-false (regexp-match? number-regex "a")))))
(run-tests regex-tests 'verbose)
 
