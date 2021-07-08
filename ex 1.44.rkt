#lang sicp
(define (compose f g)
    (lambda (x) (f (g x))))
    
(define (repeated f n)
    (if (> n 1)
        (compose f (repeated f (- n 1)))
        f
    ))
((repeated square 2) 5)

(define dx 0.00001)

(define (smooth f)
    (lambda (x) 
        (/ 
            (+ 
                (f (- x dx))
                (f x)
                (f (+ x dx))
            )
            3
        )
    )
)

((smooth square) 4)

(define (nsmooth f n)
    (repeated (smooth f) n)
)

((nsmooth square 2) 4)