#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")
(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-true (regexp-match-exact? open-paren-regex "("))
              (check-false (regexp-match-exact? open-paren-regex ")"))
              (check-false (regexp-match-exact? open-paren-regex "[")))
   (test-case "close-paren-regex"
              (check-true (regexp-match-exact? close-paren-regex ")"))
              (check-false (regexp-match-exact? close-paren-regex "("))
              (check-false (regexp-match-exact? close-paren-regex "}")))
   (test-case "define-regex"
              (check-true (regexp-match-exact? define-regex "define"))
              (check-false (regexp-match-exact? define-regex "Bill Gates"))
              (check-false (regexp-match-exact? define-regex "difine")))
   (test-case "sum-regex"
              (check-true (regexp-match-exact? sum-regex "+"))
              (check-false (regexp-match-exact? sum-regex "-"))
              (check-false (regexp-match-exact? sum-regex "mas")))
   (test-case "mult-regex"
              (check-true (regexp-match-exact? mult-regex "*"))
              (check-false (regexp-match-exact? mult-regex "/"))
              (check-false (regexp-match-exact? mult-regex "X")))
   (test-case "identifier-regex"
              (check-true (regexp-match-exact? identifier-regex "y"))
              (check-true (regexp-match-exact? identifier-regex "x1234"))
              (check-true (regexp-match-exact? identifier-regex "xyz5243"))
              (check-false (regexp-match-exact? identifier-regex "69Satanas69"))
              (check-false (regexp-match-exact? identifier-regex "xyz123456789w")))
   (test-case "number-regex"
              (check-true (regexp-match-exact? number-regex "2"))
              (check-true (regexp-match-exact? number-regex "+26423"))
              (check-true (regexp-match-exact? number-regex "-25234"))
              (check-false (regexp-match-exact? number-regex "seisnueve"))
              (check-false (regexp-match-exact? number-regex "+987654321i")))))
(run-tests regex-tests 'verbose)

 
