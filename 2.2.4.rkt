#lang racket
(require "paint/painters.ss")

; (define wave2 (beside wave (flip-vert wave)))
; (define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter))))
        (below painter2 painter2)))

; (define wave4 (flipped-pairs wave))

; (define (right-split painter n)
;     (if (= n 0) painter
;         (let ((smaller (right-split painter (- n 1))))
;             (beside painter (below smaller smaller)))))

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

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half))))

; (define (up-split painter n)
;     (if (= n 0) painter
;     (let ((smaller (up-split painter (- n 1))))
;         (beside (below smaller smaller) painter))))

(define (split a b)
    (lambda (painter n)
        (if (= n 0) painter
            (let ((smaller ((split a b) painter (- n 1))))
                (a painter (b smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))



(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect a b)
    (make-vect 
        (+ (car a) (car b))
        (+ (cdr a) (cdr b))))
(define (sub-vect a b)
    (make-vect 
        (- (car a) (car b))
        (- (cdr a) (cdr b))))
(define (scale-vect s v)
    (make-vect 
        (* s (xcor-vect v))
        (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 (cons edge2 '()))))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)



(define (frame-coord-map frame)
  ;(display (origin-frame frame))
  (lambda (v)
        (add-vect 
            (origin-frame frame)
            (add-vect 
                (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))


; (define (make-segment start end) (cons start end))
; (define (start-segment segment) (car segment))
; (define (end-segment segment) (cdr segment))
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
;선분들의 리스트를 받아서 선을 그리는 페인터
; (define (segments->painter segment-list)
;     (lambda (frame)
;         (for-each
;             (lambda (segment)
;                 (draw-line
;                     ((frame-coord-map frame) (start-segment segment))
;                     ((frame-coord-map frame) (end-segment segment))))
;             segment-list)))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame 
            new-origin
            (sub-vect (m corner1) new-origin)
            (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
    (transform-painter painter (make-vect 0 1)
                               (make-vect 1 1)
                               (make-vect 0 0)))

(paint (flip-vert einstein))

(define (shrink-to-upper-right painter)
    (transform-painter painter
        (make-vect 0.5 0.5)
        (make-vect 1 0.5)
        (make-vect 0.5 1)))

(paint (shrink-to-upper-right einstein))

(define (rotate90 painter)
    (transform-painter painter
        (make-vect 1 0)
        (make-vect 1 1)
        (make-vect 0 1)))
(define (squash-inwards painter)
    (transform-painter painter
        (make-vect 0 0)
        (make-vect 0.65 0.35)
        (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
    (let ((split-point (make-vect 0.5 0)))
        (let (
            (paint-left
                (transform-painter 
                    painter1
                    (make-vect 0 0)
                    split-point
                    (make-vect 0 1)))
            (paint-right
                (transform-painter
                    painter2
                    split-point
                    (make-vect 1 0)
                    (make-vect 0.5 1))))
            (lambda (frame) 
                (paint-left frame)
                (paint-right frame)))))

