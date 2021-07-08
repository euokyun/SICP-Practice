#lang sicp

(define (s-integral f a b n)
    (define (h) (/ (- b a) n))
    ; (display (h))
    (define (y n) (f (+ a (* (h) n))))
    (define (mult count) 
        (cond ((or (= n count) (= n 0)) (y count)) 
        ((even? count) (* 2 (y count)))
        (else (* 4 (y count)))
    ))
    (* (/ (h) 3) 
        (sum mult 0 inc n) ;->+ (mult count) (sum mult (inc count) inc n)
    ))

; (integral cube 0 1 0.001)
; (define (testthis a b c)
;     (define (tt) (+ a b c))
;     (tt))

; (testthis 1 2 3)

(s-integral cube 0 1 1000)

; (define (test a b n)
;     (define (h) (/ (- b a) n))
;     (define (y n) (+ a (* (h) n)))
;     (y n)
; )
; (test 0 1 1000)

; f(a+k(b-a)/0) + 4f(a+k(b-a)/1) + 2f(a+k(b-a)/2) + 4f(a+k(b-a)/3) + 2f(a+k(b-a)/4) ... 

