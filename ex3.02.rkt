#lang sicp

(define (make-monitored f)
    (let ([count 0])
        (define (how-many-calls?) count)
        (define (reset-count) [set! count 0] 0)
        (define this-is-not-work count)
        (define this-is-not-work-too [set! count 0])
        (lambda (m)
            (cond 
                [(eq? m 'this-is-not-work) this-is-not-work]
                [(eq? m 'this-is-not-work-too) this-is-not-work-too]
                [(eq? m 'how-many-calls?) (how-many-calls?)]
                [(eq? m 'reset-count) (reset-count)]
                [else [set! count (+ count 1)] (f m)]))))


(define s (make-monitored sqrt))
(s 100)                         ; 10
(s 'how-many-calls?)            ; 1
(s 'how-many-calls?)            ; 1
(s 100)                         ; 10
(s 100)                         ; 10
(s 'this-is-not-work-too)       ; if this works next line will be 0 -- not 3
(s 'how-many-calls?)            ; 3
(s 'reset-count)                ; 0
(s 'how-many-calls?)            ; 0
(s 100)                         ; 10
(s 'how-many-calls?)            ; 1
(s 'this-is-not-work)           ; 0 if this works result will be 1 not 0