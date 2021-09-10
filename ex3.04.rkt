#lang sicp
(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))
    (define (deposit amount) [set! balance (+ balance amount)] balance)
    (define (call-the-cops) "call-the-cops")
    (let ([count 0])
        (define (dispatch p m)
            (cond 
                [(>= count 7) (lambda (m) (call-the-cops))]
                [(not (equal? password p)) (lambda (m) [set! count (+ 1 count)] "Incorrect password")]
                [(eq? m 'withdraw) withdraw]
                [(eq? m 'deposit) deposit]
                [else (error "Unknown request -- MAKE-ACCOUNT") m]))
        dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "call-the-cops"
((acc 'some-other-password 'deposit) 50) ; "call-the-cops"
((acc 'some-other-password 'deposit) 50) ; "call-the-cops"