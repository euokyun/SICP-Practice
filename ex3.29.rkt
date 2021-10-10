#lang sicp
(#%provide (all-defined))
(#%require "3.3.4.rkt")
;; and
;; 1 1 1
;; 1 0 0
;; 0 1 0
;; 0 0 0
;; or
;; 1 1 1
;; 1 0 1
;; 0 1 1
;; 0 0 0

(define (or-gate a1 a2 output)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w1 w3)
    (inverter w3 output)))
