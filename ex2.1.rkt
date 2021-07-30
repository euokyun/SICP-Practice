#lang sicp
(define (gcd a b)
    (if (= b 0) 
        a
        (gcd b (remainder a b))))


(define (add-rat x y)
    (make-rat
        (+  (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat
        (-  (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat
        (*  (* (numer x) (numer y)))
            (* (denom x) (denom y))))
(define (div-rat x y)
    (make-rat
        (*  (* (numer x) (denom y)) 
            (* (denom x) (numer y)))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

;;scheme에서는 데이터의 쌍을 cons로 사용한다.

; (define (make-rat n d) (cons n d))

; (define (make-rat n d) 
;     (let ((g (gcd n d)))
;         (cons (/ n g) (/ d g))))
;최대공약수로 나눔


(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))


;음수도 되는 버전
(define (make-rat n d) 
    (let ((g (gcd n d))
        (isminus? (if (> 0 (* n d)) -1 1)))
    (cons (/ (* isminus? (abs n)) g) (/ (abs d) g))))



(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(print-rat (make-rat -1 -2))



(or (> 0 (* 2 -2)))