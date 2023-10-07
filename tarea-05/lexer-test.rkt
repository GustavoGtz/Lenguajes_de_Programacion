#lang racket

(require rackunit
         rackunit/text-ui
         "lexer.rkt")

(define lexer-tests
  (test-suite
   "Pruebas para lexer.rkt"
   (test-case "Lexer"
              (check-equal?
               (stream->list (lex-from-string "("))
               (list
                (token 'open-paren #f 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string ")"))
               (list
                (token 'close-paren #f 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string "+"))
               (list
                (token 'binop '+ 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string "*"))
               (list
                (token 'binop '* 1 0)))

              (check-equal?
               (stream->list (lex-from-string "xyz"))
               (list
                (token 'identifier 'xyz 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string "123"))
               (list
                (token 'number 123 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string "(define x -123) (+ 2 (* 42 25)))"))
               (list
                (token 'open-paren #f 1 0)
                (token 'define #f 1 1)
                (token 'identifier 'x 1 8)
                (token 'number -123 1 10)
                (token 'close-paren #f 1 14)
                (token 'open-paren #f 1 16)
                (token 'binop '+ 1 17)
                (token 'number 2 1 19)
                (token 'open-paren #f 1 21)
                (token 'binop '* 1 22)
                (token 'number 42 1 24)
                (token 'number 25 1 27)
                (token 'close-paren #f 1 29)
                (token 'close-paren #f 1 30)
                (token 'close-paren #f 1 31)))
              )))

(run-tests lexer-tests 'verbose)
 
