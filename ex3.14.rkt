#lang sicp
(define (mystery x)
    (define (loop x y)
        (if (null? x) y
            (let ([temp (cdr x)])
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))

(define v '(a b c d))
(mystery v) ; (d c b a)

