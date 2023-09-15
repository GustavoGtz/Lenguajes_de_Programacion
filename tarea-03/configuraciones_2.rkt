; # Segunda configuración
#lang stacker/smol/hof

(letrec ((x (lambda (z) (+ x z))))
 (letrec ((f x))
   (set! x 3)
  (letrec ((x 4))
    (f 5))))