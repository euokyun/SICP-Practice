#lang sicp
;n d ni값과 di값 k=k마디
(define (cont-frac n d k)
    (define (cont-frac-iter i res)
        (if (= i 0) res
            (cont-frac-iter (- i 1) (/ (n i) (+ (d i) res)))
        )
    )
    (cont-frac-iter k (/ (n k) (d k)))
)

(cont-frac
    (lambda (i) 1.0)
    (lambda (i) (if (= (remainder i 3) 2) (* 2(/ (+ i 1) 3))
                    1
                )
    )
    11)


(newline)