#lang sicp
(define (filtered-accumulate combiner filter null-value term a next b)
    (define (iter a b rtn)
        (if (> a b) rtn
            (if (filter a) 
                (iter (next a) b (combiner rtn (term a)))
                (iter (next a) b rtn))))
    (iter a b null-value))

(define (sumprime a b)
    (filtered-accumulate + prime? 0 square a inc b))

(define (square n) (* n n))
(define (inc n) (+ n 1))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond
        ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))
(define (prime? n)
    (= n (smallest-divisor n)))

(define (gcd a b)
    (if (= b 0) a
        (gcd b (remainder a b))))

(sumprime 2 10)

(define (donothing n) n)

(define (gcdsum a b)
    (define (filter n) (= (gcd n b) 1))
    (filtered-accumulate * filter 1 donothing a inc b)
)
(gcdsum 1 10)