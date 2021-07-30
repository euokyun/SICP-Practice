#lang sicp
(#%require sicp-pict)


(define (corner-split painter n)
    (if (= n 0) painter
        (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                (bottom-right (below right right))
                (corner (corner-split painter (- n 1))))
                (beside 
                    (below painter top-left) 
                    (below bottom-right corner))))))
(define (flipped-pairs painter)
    (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
        (combine4 painter)))
(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
              (bottom (beside (bl painter) (br painter))))
            (below bottom top))))

; (define (square-limit painter n)
;     (let ((quarter (corner-split painter n)))
;         (let ((half (beside (flip-horiz quarter) quarter)))
;             (below (flip-vert half) half))))
(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
        (combine4 (corner-split painter n))))

; (define (right-split painter n)
;     (if (= n 0) painter
;         (let ((smaller (right-split painter (- n 1))))
;             (beside painter (below smaller smaller)))))
; (define (up-split painter n)
;     (if (= n 0) painter
;         (let ((smaller (up-split painter (- n 1))))
;             (below painter (beside smaller smaller))))

(define (split a b)
    (lambda (painter n)
        (if (= n 0) painter
            (let ((smaller ((split a b) painter (- n 1))))
                (a painter (b smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

(paint einstein)

(paint (right-split einstein 5))
(paint (up-split einstein 5))

; (paint wave4)