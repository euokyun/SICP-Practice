#lang sicp
; combiner - 두 값을 묶는 프로시저
; null-value 계산값 없을때 쓰는 인자

(define (inc a) (+ a 1))
(define (square x)
    (* x x))

(define (accumulate combiner null-value term a next b)
    (if (> a b) 
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))
    )
)
(define (accumulate-rec combiner null-value term a next b)
    (define (iter a b rtn)
        (if (> a b) 
            rtn
            (iter (next a) b (combiner rtn (term a)))
        )
    )
    (iter a b null-value)
)
(define (sumacc a b)
        (accumulate + 0 + a inc b))

(define (sumacc-rec a b)
        (accumulate-rec + 0 + a inc b))


(sumacc 1 100)
(sumacc-rec 1 100)

(define (prodacc a b)
    (accumulate * 1 * a inc b))


(define (prodacc-rec a b)
    (accumulate-rec * 1 * a inc b))


; (define (prodacc a b)
;     (define (product term a next b)
;         (accumulate * 1 term a next b)
;     )
;     (product * a inc b)
; )
(prodacc 1 5)
(prodacc-rec 1 5)