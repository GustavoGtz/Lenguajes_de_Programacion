#lang stacker/smol/hof

(deffun (f z) (lambda (w) (+ w z)))

((f 3) 4)