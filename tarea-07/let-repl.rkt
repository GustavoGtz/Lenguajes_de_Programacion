#lang racket

(require "let-interp.rkt"
         "let-vals.rkt")

(define (printval str)
  (match str
    [(num-val x) (printf "~a" x)]
    [(bool-val x) (printf "~a" (if x "true" "false"))]
    [(pair-val x)
     (printf "(")
     (printval (car x))
     (printf " . ")
     (printval (cdr x))
     (printf ")")]
    [(null-val) (printf "()")]))

(define (get-line)
  (define ch (peek-char))
  (cond [(eof-object? ch)
         "exit"]
        [(char-whitespace? ch)
         (read-char)
         (get-line)]
        [else
         (read-line)]))

(define (repl)
  (printf "LET> ")
  (flush-output)
  (let ([str (string-trim (get-line))])
    (match str
      ["exit"
       (printf "bye bye\n")]
      ["quit"
       (printf "adios\n")]
      [_
       (with-handlers ([exn:fail:read?
                        (lambda (exn)
                          (printf "Error in ~a\n" (exn-message exn)))]
                       [exn:fail?
                        (lambda (exn)
                          (printf "Error: ~a\n" (exn-message exn)))])
         (printval (interpret-string str))
         (printf "\n"))
       (repl)])))

(with-handlers ([exn:break?
                 (lambda args
                   (printf "User break, exiting...\n"))])
  (repl))


