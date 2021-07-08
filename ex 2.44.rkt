#lang sicp
(#%require sicp-pict)

(define (up-split painter n)
    (if (= n 0) painter
    (let ((smaller (up-split painter (- n 1))))
        (beside (below smaller smaller) painter))))