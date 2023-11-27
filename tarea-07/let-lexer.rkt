#lang racket/base

(require racket/stream
         (except-in parser-tools/lex token?)
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr
         "let-errors.rkt"
         "let-locs.rkt"
         "let-tokens.rkt")

(define-lex-abbrev comment-re
  (:: "#" (:* (:~ #\newline))))

(define-lex-abbrev digit
  (:/ "0" "9"))

(define-lex-abbrev int-re
  (:: (:? (:or "+" "-")) (:+ digit)))

(define-lex-abbrev id-re
  (:: (:or alphabetic "+" "-" "/" "*" "?" "=" "," "(" ")") (:* (:or "+" "-" "/" "*" "?" "=" "," "(" ")" alphabetic digit))))

(define (make-int-token lexeme)
  (int-token (string->number lexeme)))

(define (make-id-token lexeme)
  (id-token (string->symbol lexeme)))

(define lex-let
  (lexer
   [(eof) empty-stream]
   [whitespace (lex-let input-port)]
   [comment-re (lex-let input-port)]
   [int-re
    (stream-cons
     (pos-token (make-int-token lexeme) start-pos end-pos)
     (lex-let input-port))]
   [id-re
    (stream-cons
     (pos-token (make-id-token lexeme) start-pos end-pos)
     (lex-let input-port))]
   [any-char
    (raise-lexer-error start-pos end-pos input-port lexeme)]))

(define (raise-lexer-error beg end ip irritant)
  (let ((rest-of-line (read-line ip)))
    (read-error
     (format "Unexpected character in input: ~a~a"
             irritant
             (if (eof-object? rest-of-line) "" rest-of-line))
     beg end)))

(provide lex-let)
