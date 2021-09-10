#lang sicp
; test procedure --
(define random-init 1)
(define (rand-update x) (+ 2 x))
; -- test procedure

(define rand
    (let ([x random-init])
        (define generate (lambda () [set! x (rand-update x)] x))
        (define reset (lambda (v) [set! x v] v))
        (define (dispatch m)
            (cond
                [(equal? m 'generate) (generate)]
                [(equal? m 'reset) reset]
                [else (error "Unknown request -- RAND") m]))
        dispatch))

(rand 'generate) ; 3
(rand 'generate) ; 5
(rand 'generate) ; 7
((rand 'reset) 0) ; 0
(rand 'generate) ; 2
(rand 'generate) ; 4