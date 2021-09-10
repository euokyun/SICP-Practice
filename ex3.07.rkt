#lang sicp
; from ex3.03.rkt
(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))
    (define (deposit amount) 
        [set! balance (+ balance amount)]
        balance)
    (define (dispatch p m)
        (cond 
            [(not (equal? password p)) (lambda (m) "Incorrect password")]
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [else (error "Unknown request -- MAKE-ACCOUNT") m]))
    dispatch)

(define (make-joint account password joint-password)
    (lambda (p m) 
        (if (equal? p joint-password) (account password m)
            (lambda (m) "Incorrect password"))))


(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10) ; 90
((paul-acc 'rosebud 'withdraw) 10) ; 80
((paul-acc 'rosebud 'deposit) 30) ; 110
((peter-acc 'open-sesame 'withdraw) 100) ; 10
((paul-acc 'rosebud 'withdraw) 50) ; "Insufficient funds"
((paul-acc 'rosaebud 'deposit) 100) ; "Incorrect password"

(define pass-incorrect-acc (make-joint peter-acc 'sesame 'rosebud))
((pass-incorrect-acc 'rosebud 'withdraw) 10) ; "Incorrect password"