; # Segunda configuración
#lang stacker/smol/hof

(let ((x 3))
 (let ((f (lambda (z) (+ x z))))
  (let ((x 4)) (f 5))))