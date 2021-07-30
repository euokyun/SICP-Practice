#lang sicp
(#%require sicp-pict)
; a
(define wave (segments->painter (list
    (make-segment (make-vect 0.5 1) (make-vect 0.25 0.75))
    (make-segment (make-vect 0.25 0.75) (make-vect 0.4 0.75))
    (make-segment (make-vect 0.4 0.75) (make-vect 0.4 0))
    (make-segment (make-vect 0.4 0) (make-vect 0.6 0))
    (make-segment (make-vect 0.6 0) (make-vect 0.6 0.75))
    (make-segment (make-vect 0.6 0.75) (make-vect 0.75 0.75))
    (make-segment (make-vect 0.75 0.75) (make-vect 0.5 1))
    (make-segment (make-vect 0 0.6) (make-vect 0 0.4))
    (make-segment (make-vect 0 0.4) (make-vect 0.2 0.4))
    (make-segment (make-vect 0 0) (make-vect 0.1 0.1))
    (make-segment (make-vect 1 1) (make-vect 0.9 0.9)))))
(define wave1 (segments->painter (list
    (make-segment (make-vect 0 1) (make-vect 0.5 0))
    (make-segment (make-vect 0.5 0) (make-vect 1 1)))))

(define (corner-split painter n)
    (if (= n 0) painter
        (let (  (up (up-split painter (- n 1)))
                (right (right-split painter (- n 1))))
            (let (  (top-left up)
                    (bottom-right right)
                    (corner (corner-split painter (- n 1))))
            ; (let (  (top-left (beside up up))
            ;         (bottom-right (below right right))
            ;         (corner (corner-split painter (- n 1))))
                (beside 
                    (below painter top-left) 
                    (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
              (bottom (beside (bl painter) (br painter))))
            (below bottom top))))

(define (flipped-pairs painter)
    (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
        (combine4 painter)))
        
; c
(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
       (combine4 (corner-split painter n))))

(define (new-square-limit painter n)
    (let ((combine4 (square-of-four 
                        rotate270 
                        (lambda (painter) (flip-vert (rotate90 painter)))
                         (lambda (painter) (flip-horiz (rotate90 painter)))
                        rotate90)))
        (combine4 (corner-split painter n))))

(define (split a b)
    (lambda (painter n)
        (if (= n 0) painter
            (let ((smaller ((split a b) painter (- n 1))))
                (a painter (b smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

(paint (square-limit wave 2))
(paint (new-square-limit wave 2))