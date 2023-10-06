#lang racket

(require rackunit
         rackunit/text-ui
         "lexer.rkt")

(define regex-tests
  (test-suite
   "Pruebas para lexer.rkt"
   (test-case "Lexer"
              (check-equal?
               (stream->list (lex-from-string "(define x 123) (+ 2 (* 42 25)))"))
               (list
 (token 'open-paren #f 1 0)
 (token 'define #f 1 1)
 (token 'identifier 'x 1 8)
 (token 'number 123 1 10)
 (token 'close-paren #f 1 13)
 (token 'open-paren #f 1 15)
 (token 'binop '+ 1 16)
 (token 'number 2 1 18)
 (token 'open-paren #f 1 20)
 (token 'binop '* 1 21)
 (token 'number 42 1 23)
 (token 'number 25 1 26)
 (token 'close-paren #f 1 28)
 (token 'close-paren #f 1 29)
 (token 'close-paren #f 1 30))))))

(run-tests regex-tests 'verbose)
 
