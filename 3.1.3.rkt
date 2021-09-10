#lang sicp
(define (make-simplified-withdraw balance)
    (lambda (amount) (set! balance (- balance amount)) balance))

(define W (make-simplified-withdraw 25))

(W 20) ; 5
; ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
; (set! balance (- 25 20)) 25
; 25
(W 10) ; -5



; set!을 쓰지 않음
(define (make-decrementer balance)
    (lambda (amount) (- balance amount)))

(define D (make-decrementer 25))
(D 20) ; 5
; ((make-decrementer 25) 20)
; ((lambda (amount) (- 25 amount)) 20)
; (- 25 20)
; 5
(D 10) ; 15

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
; same

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
; not same 
(W1 20) ; 5
(W1 20) ; -15
(W2 20) ; 5
; from 3.1.1
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))
    (define (deposit amount) 
        [set! balance (+ balance amount)]
        balance)
    (define (dispatch m)
        (cond 
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [else (error "Unknown request -- MAKE-ACCOUNT") m]))
    dispatch)
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))

; alias
; (define peter-acc (make-account 100))
; (define paul-acc peter-acc)

; (define (factorial n)
;     (define (iter product counter)
;         (if (> counter n) product
;             (iter (* counter product) (+ counter 1))))
;     (iter 1 1))

(define (factorial n)
    (let ([product 1] [counter 1])
        (define (iter)
            (if (> counter n) product
                (begin 
                    [set! product (* counter product)] 
                    [set! counter (+ counter 1)]
                    (iter))))
        (iter)))