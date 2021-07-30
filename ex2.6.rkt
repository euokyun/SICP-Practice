#lang sicp
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define (add a b)
    (lambda (f) 
        (lambda (x)
            ((a f)((b f) x)))))
(define (inc a) (+ a 1))
(((add three three) inc) 2)

;;(a f)가 f를 a번 반복시킴 - 밑의 one inc, two inc, three inc

(display (add-1 zero))
((zero inc) 2)
((one inc) 2)
((two inc) 2)
((three inc) 2)
(((add-1 one) inc) 2)
(((add-1 zero) inc) 2)
; ((three square) 2)