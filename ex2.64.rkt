#lang sicp
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (list->tree elements)
    (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                 (let ((left-tree (car left-result))
                       (non-left-elts (cdr left-result))
                       (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                          (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let ((right-tree (car right-result))
                             (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
(list->tree '(1 2 3 4 5 6 7))

; a
; 인자로 받은 요소를 반으로 잘라 트리를 만드는 구조
;       5
;   1       9
;     3   7   11
