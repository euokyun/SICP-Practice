#lang sicp
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
    (cond 
        ((null? sequence) '())
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
    (if (> low high) '()
        (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

;rest-of-queens는 세로줄 k-1개에 퀸 k-1개를 놓는 방법 하나를 나타내고
;new-row는 k번째 세로줄에 k번째 퀸을 놓을 수 있는 가로줄을 나타냄
;1. 체스판의 자리값을 원소로 하는 집합의 표현 방식을 정하라
(define (position row col) (cons row col))
;2. adjoin-position 프로시저(1에 새로운 자리값을 집어넣는 프로시저)
(define (adjoin-positions row col queens)
    ; (cons queens (map (lambda (j) (cons row j)) (enumerate-interval 1 row)))
    (cons (list row col) queens))

;3. empty-board 공집합을 나타냄
(define empty-board '())
;4. safe? k번째 퀸이 나머지 퀸들에게서 안전한지 확인

(define (safe? k positions)
    (define (safe-iter? p q)
        (define (pos-equal a b) 
            (and (= (car a) (car b)) (= (cdr a) (cdr b))))
        (define (pos-xy-equal a b)
            (or (= (car a) (car b)) (= (cadr a) (cadr b))))
        (define (upxy pos)
            (let ((minxy (- (min (car pos) (cadr pos)) 1)))
                (cons (- (car pos) minxy) (- (cadr pos) minxy))))
        (define (dnxy pos)
            (if (<= (+ (car pos) (cadr pos)) (+ k 1))
                (cons 1 (- (+ (car pos) (cadr pos)) 1))
                (cons (- (+ (car pos) (cadr pos)) k) k)))
        (cond 
            ((null? q) #t)
            ((not (or (pos-xy-equal p (car q))
                      (pos-equal (upxy (car q)) (upxy p))
                      (pos-equal (dnxy (car q)) (dnxy p))))
                (safe-iter? p (cdr q)))
            (else #f)))
    ; (display k)
    ; (newline)
    ; (display positions)
    ; (newline)
    (if (null? (cdr positions)) 
        #t
        (safe-iter? (car positions) (cdr positions))))

(define (pos-equal a b) 
    (= (car a) (car b)))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0) (list empty-board)
            (filter 
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens) 
                        (map 
                            (lambda (new-row) 
                                (adjoin-positions new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))


(define (queens-new board-size)
    (define (queen-cols k)
        (if (= k 0) (list empty-board)
            (filter 
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (new-row) 
                        (map 
                            (lambda (rest-of-queens) 
                                (adjoin-positions new-row k rest-of-queens))
                            (queen-cols (- k 1))))
                    (enumerate-interval 1 board-size)))))
    (queen-cols board-size))

; (queens 8)
(queens-new 8)

;queens-new가 더 빠른 이유
;queen-cols로 가장 작은 부분부터 먼저 시작하고
;따라서 safe?에서 확인해야 할 크기가 줄어들기 때문