#lang sicp
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))


(define (left-branch mobile)
    (car mobile)
)
(define (right-branch mobile)
    (car (cdr mobile))
)

(define (branch-length branch)
    (car branch)
)
;; --cons일때 start
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
;; --cons일때 end


(define (total-weight branch)
    (let ((right (right-branch branch))
        (left (left-branch branch)))
        (if (pair? left) ;left가 pair = mobile
            (if (pair? right) 
                (+ (total-weight left) (total-weight right)))
            right)))

(define (total-weight-length branch)
    (let ((right (right-branch branch))
        (left (left-branch branch)))
        (if (pair? left) ;left가 pair = mobile
            (if (pair? right) 
                (+ (total-weight left) (total-weight right)))
            (* left right))))

(define (balanced-mobile? mobile)
    (= (total-weight-length (left-branch mobile))
        (total-weight-length (right-branch mobile))))

(define b1 (make-branch 10 10))
(define b2 (make-branch 10 20))

(define m1 (make-mobile b1 b2))

(define b3 (make-branch 10 20))
(define b4 (make-branch 10 10))

(define m2 (make-mobile b3 b4))

(define m3 (make-mobile m1 m2))

(define m0 (make-mobile m1 m3))

(total-weight m0)
(total-weight m1)
(total-weight b1)
(total-weight b2)
(balanced-mobile? m3)
;; (total-weight (left-branch (left-branch m0)))


