#lang sicp

(define (square n) (* n n))

; (define (sum-odd-squares tree)
;     (cond 
;         ((null? tree) 0)
;         ((not (pair? tree))
;             (if (odd? tree) (square tree) 0))
;         (else (+ (sum-odd-squares (car tree))
;                 (sum-odd-squares (cdr tree))))))

(define (fib n)
    (define (fib-iter a b count)
        (if (= count 0) b
            (fib-iter (+ a b) a (- count 1))))
    (fib-iter 1 0 n))

; (define (even-fibs n)
;     (define (next k) 
;         (if (> k n) '()
;             (let ((f (fib k)))
;                 (if (even? f) (cons f (next (+ k 1)))
;                     (next (+ k 1))))))
;     (next 0))


(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
    (cond 
        ((null? sequence) '())
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 0 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
    (if (> low high) '()
        (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
    (cond 
        ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
    (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
    (accumulate cons '() (map square (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements seq)
    (accumulate * 1 (map square (filter odd? seq))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; (define (salary-of-highest-paid-programmer records)
;     (accumulate max 0 (map salary (filter programmer? records))))

;;programmer? 프로그래머의 인사기록인지 판단
;;salary 봉급을 고르는 프로시저



;;겹친 매핑

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond
        ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))
(define (prime? n)
    (= n (smallest-divisor n)))

; (enumerate-interval 1 n)

; (accumulate
;     append
;     '()
;     (map (lambda (i) 
;         (map (lambda (j) (list i j))
;             (enumerate-interval 1 (- i 1))))
;         (enumerate-interval 1 n)))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map 
        make-pair-sum 
        (filter 
            prime-sum? 
            (flatmap 
                (lambda (i) 
                    (map 
                        (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(define (permutations s)
    (if (null? s)
        (list '())
        (flatmap
            (lambda (x)
                (map 
                    (lambda (p) (cons x p))
                    (permutations (remove x s))))
            s)))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence))

(permutations (list 1 2 3))