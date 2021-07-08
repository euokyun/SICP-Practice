#lang sicp
(define (gcd a b)
    (if 
        (= b 0)
        a
        gcd b (remainder a b)))

;; gcd 206 40
;; gcd 40 (remainder 206 40) 
;; gcd (remainder 206 40) (remainder 40 (remainder 206 40))
;; gcd 6 (remainder 40 6)
;; gcd 6 4
;; gcd 4 (remainder 6 4)
;; gcd (remainder 6 4) (remainder 4 (remainder 6 4))
;; gcd 2 (remainder 4 2)
;; gcd 2 0
;; 2
 

;; 6번?




;; 인자 먼저 계산법

;; 206 40 -ㄱ
;; 40 6 r
;; 6 4 r
;; 4 2 r
;; 2 0 
;; =2

;; 4번 사용