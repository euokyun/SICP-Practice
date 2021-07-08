#lang sicp
; (define (sum term a next b)
;     (if (> a b)
;         0
;         (+ (term a)
;             (sum term (next a) next b)    
;         )))
; (define (sum term a next b)
;     (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (+ result (term a)))))
;     (iter a 0))

(define (product_1 term a next b)    
    (if (> a b)
        0
        (* (term a)
            (product_1 term (next a) next b)    
        )))


(define (product term a next b)
    (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
    (iter a 1))

; (2n/(2n-1)) * (2n/(2n+1)) = pi/2
(define (double n) (* n 2))
(define (square n) (* n n))

(define (wallis n)
    ; (*  (/ (double n) (- (double n) 1))
    ;     (/ (double n) (+ (double n) 1)))
    (/ (* 4 (square n)) (- (* 4 (square n)) 1))
)
(define (inc n)
    (+ n 1))

(define (wallis-pi a b)
    (* 2 (product wallis a inc b)))

(wallis-pi 1 50)
; (product wallis 1 inc 1000)

; 2*2*4*4*6*6*8*8 = 
; 1*3*3*5*5*7*7*9 = 

