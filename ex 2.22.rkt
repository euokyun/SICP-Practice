#lang sicp


(define (square-list items)
    (define (iter things answer)
        (if (null? things) answer
            (iter (cdr things) (cons (square (car things)) answer))))
    (iter items '()))

(square-list (list 1 2 3 4))

;; 뒤집힌 결과가 나오는 이유 - answer를 반환할때가 맨 마지막 요소에 다다랐을 때고
;; 그때 answer는 반대로 조립되었기 때문

;; (define (square-list items)
;;     (define (iter things answer)
;;         (if (null? things) answer
;;             (iter (cdr things) (cons answer (square (car things))))))
;;     (iter items '()))

;; 이게 안되는 이유는 cons에서 첫번째 원소를 연결할때 쌍의 원소가 아닌 쌍으로 연결하기 때문임
