#lang sicp
(define (fixed-point f first-guess) 
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))


(define (average-damp f)
    (define (average x y) (/ (+ x y) 2))
    (lambda (x) (average x (f x))))

(define (compose f g)
    (lambda (x) (f (g x))))
    
(define (repeated f n)
    (if (> n 1)
        (compose f (repeated f (- n 1)))
        f
    ))

(define (fast-expt b n)
    (cond  
        ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)(= (remainder n 2) 0))
(define (test x n k)
    (fixed-point 
        ((repeated average-damp k) (lambda (y) (/ x (fast-expt y (- n 1)))))
        1.0
    )
)
(define (test-1 x n)
    (fixed-point 
        ((repeated average-damp (round (sqrt n))) (lambda (y) (/ x (fast-expt y (- n 1)))))
        1.0
    )
)


; (test 16 4 1) 고정점으로 못감
(test 16 4 2) ;성공
(test 16 2 1)
;(test 256 8 2) ;실패
(test 256 8 3) ;성공
;(test 256 16 3);실패
(test 256 16 4);성공
;(test 512 32 4);실패
(test 512 32 5);성공

;k가 2^k보다 작으면 실패
