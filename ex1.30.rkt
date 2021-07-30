#lang sicp
; (define (sum term a next b)
;     (if (> a b)
;         0
;         (+ (term a)
;             (sum term (next a) next b)    
;         )))

(define (sum term a next b)
    (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))


(define (inc a)
    (+ a 1))

(define (identify x) x)

(define (sum-integers a b)
    (sum identify a inc b))

(sum-integers 50 100)