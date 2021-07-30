#lang sicp
(#%require sicp-pict)

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

(define (below up down)
    (let ((split-point (make-vect 0 0.5)))
        (let ((paint-down 
                (transform-painter down
                    (make-vect 0 0)
                    (make-vect 1 0)
                    split-point
                    ))
            (paint-up
                (transform-painter up
                    split-point
                    (make-vect 1 0.5)
                    (make-vect 0 1)
                    )))
    (lambda (frame) (paint-up frame) (paint-down frame)))))

(define (below-r up down)
    (rotate90 (beside (rotate270 down) (rotate270 up))))

(paint (below wave wave1))
(paint (below-r wave wave1))