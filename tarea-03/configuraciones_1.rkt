; #Primer configuración
#lang stacker/smol/fun
(deffun (pause) 0)

(defvar v0 (mvec (mvec 0) (mvec 0)))
(defvar v1 (vec-ref v0 0))
(defvar v2 (vec-ref v0 1))

(vec-set! (vec-ref v0 0) 0 (vec-ref v0 1))
(vec-set! (vec-ref v0 1) 0 (vec-ref v0 0))

(pause)
