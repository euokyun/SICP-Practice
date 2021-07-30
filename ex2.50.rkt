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

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 0 1)
                       (make-vect 1 1)
                       (make-vect 0 0)))

(define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1 1)
                       (make-vect 0 1)
                       (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint wave)
(paint (flip-horiz wave))
(paint (rotate180 wave))
(paint (rotate270 wave))