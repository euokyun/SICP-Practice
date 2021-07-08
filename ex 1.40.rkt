#lang sicp
(define (fixed-point f first-guess) 
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))
(define dx 0.00001)
(define (deriv g)
    (lambda (x) 
        (/ 
            (- (g (+ x dx)) (g x)) 
            dx
        )))

; ((deriv cube) 5)
(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

;; (define (cubic a b c)
;;     (+ 
;;         (lambda (y) (cube x))
;;         (* a (lambda (y) (square x)))
;;         (* b (lambda (y) x))
;;         c
;;     )
;; )

(define (cubic a b c)
    (lambda (x) 
        (+ 
            (cube x)
            (* a (square x))
            (* b x)
            c
        )
    ))
(newtons-method (cubic 11 -6 -6) 1)
