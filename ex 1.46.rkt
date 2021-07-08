#lang sicp
;;old fixed point function
;; (define (fixed-point f first-guess) 
;;     (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
;;     (define (try guess)
;;         (let ((next (f guess)))
;;             (if (close-enough? guess next)
;;                 next
;;                 (try next))))
;;     (try first-guess))

(define (fixed-point f first-guess) 
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))



(define (iterative-improve f enough?) 
    (define (try guess)
        (if (enough?)
            guess
            (f guess)
        )
    )
    (lambda (g) (try g))
)

(define (fixed-point f first-guess) 
    ((iterative-improve 
        f 
        (lambda (g) (< (abs (- x g)) 0.00001)))
    first-guess)
)

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

    
(define (compose f g)
    (lambda (x) (f (g x))))
    
(define (repeated f n)
    (if (> n 1)
        (compose f (repeated f (- n 1)))
        f
    ))


(define (average-damp f)
    (define (average x y) (/ (+ x y) 2))
    (lambda (x) (average x (f x))))



(sqrt 4)