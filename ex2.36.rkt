#lang sicp
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs)) '()
        (cons 
            (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs))
        )))
;;map의 꼴과 비슷함

;;리스트의 첫 원소들만 모은 리스트를 만드는 함수
;;그 나머지의 리스트를 만드는 함수

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; (map (lambda (x) (car x)) s)
;; (map (lambda (x) (cdr x)) s)


(accumulate-n + 0 s)
