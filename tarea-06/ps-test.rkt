#lang racket


(require rackunit
         rackunit/text-ui
         "ps.rkt")

(define ps-tests
  (test-suite
   "Pruebas para los problemas de ps.rkt"
   (test-case "Bundle"
              (check-equal? (bundle (explode "abcdefg") 3)
                            (list "abc" "def" "g"))
              (check-equal? (bundle (explode "abcdefgh") 2)
                            (list "ab" "cd" "ef" "gh"))
              (check-equal? (bundle (explode "abcdefg") 0)
                            (list)))
   (test-case "take"
              (check-equal? (take '() 3)
                            (list))
              (check-equal? (take '(a) 3)
                            (list 'a))
              (check-equal? (take '(a b c) 3)
                            (list 'a 'b 'c))
              (check-equal? (take '(a b c) 4)
                            (list 'a 'b 'c))
              (check-equal? (take '(a b c) 2)
                            (list 'a 'b)))
   
   (test-case "drop"
              (check-equal? (drop '(a b c) 0)
                            (list 'a 'b 'c))
              (check-equal? (drop '(a b c) 1)
                            (list 'b 'c))
              (check-equal? (drop '(a b c) 2)
                            (list 'c))
              (check-equal? (drop '(a b c) 3)
                            (list))
              (check-equal? (drop '() 5544)
                            (list)))

   (test-case "list->chunks"
              (check-equal? (list->chunks '("h" "o" "l" "a") 2)
                            '(("h" "o") ("l" "a")))
              (check-equal? (list->chunks '("h" "o" "l" "a") 3)
                            '(("h" "o" "l") ("a")))
              (check-equal? (list->chunks '("h" "o" "l" "a") 4)
                            '(("h" "o" "l" "a")))
              (check-equal? (list->chunks '("h" "o" "l" "a") 1)
                            '(("h") ("o") ("l") ("a")))
              (check-equal? (list->chunks '("h" "o" "l" "a") 0)
                            '()))
   
   (test-case "bundle-chunk"
              (check-equal? (bundle-chunk (explode "abcdefg") 3)
                            (list "abc" "def" "g"))
              (check-equal? (bundle-chunk (explode "abcdefgh") 2)
                            (list "ab" "cd" "ef" "gh"))
              (check-equal? (bundle-chunk (explode "abcdefg") 0)
                            (list)))
   
             ))
              
  
              
(run-tests ps-tests 'verbose)
