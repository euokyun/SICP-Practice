#lang sicp
(define (cont-frac n d k)
    (define (cont-frac-iter i res)
        (if (= i 0) res
            (cont-frac-iter (- i 1) (/ (n i) (- (d i) res)))))
    (cont-frac-iter k (/ (n k) (d k))))

(define (tan-cf x k)
    (cont-frac 
        (lambda (i) (if (= i 1) x
                (square x)))
        (lambda (i) (- (* 2 i) 1))
        k
    ))

(tan-cf 0.7854 100000)