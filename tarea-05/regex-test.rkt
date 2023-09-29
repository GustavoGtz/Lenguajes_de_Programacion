#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")

(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
             (check-equal? (bundle (explode "abcdefg") 3)
                           (list "abc" "def" "g"))
             (check-equal? (bundle (explode "abcdefgh") 2)
                           (list "ab" "cd" "ef" "gh"))
             (check-equal? (bundle (explode "abcdefgh") 1)
                           (list "a" "b" "c" "d" "e" "f" "g" "h"))
             (check-equal? (bundle '() 2)
                           '())
             (check-equal? (bundle '("a" "b") 3)
                           (list "ab")))
   (test-case "close-paren-regex"
              ...)
   (test-case "define-regex"
              ...)
   (test-case "sum-regex"
              ...)
   (test-case "mult-regex"
              ...)
   (test-case "identifier-regex"
              ...)
   (test-case "number-regex"
              ...)))

(run-tests regex-tests 'verbose)

