#lang sicp
(define (same-parity . e)
    (p e (lambda (a) (= (remainder a 2) (remainder (car e) 2))))
)

(define (append list1 list2)
    (if (null? list1) list2
        (cons (car list1) (append (cdr list1) list2))))

(define (p items f) 
    (define (pong i n l)
        (if (null? i) l
            (if (f n) (pong (cdr i) (+ n 1) (append l (list (car i))))
                (pong (cdr i) (+ n 1) l)
            )
        ))
    (pong items 1 (list))
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; 문제를 잘못 이해 - 짝/홀수번째 요소인줄 알았음
;; 문제는 짝, 홀수값을 가진 요소를 꺼내면 됨

(define (p items f) 
    (define (pong i l)
        (if (null? i) l
            (if (f (car i)) (pong (cdr i) (append l (list (car i))))
                (pong (cdr i) l)
            )
        ))
    (pong items (list))
)

(define (same-parity . e)
    (p e (lambda (a) (= (remainder a 2) (remainder (car e) 2))))
)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)