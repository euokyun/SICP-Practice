#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2) 
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum) (car datum)
        (error "bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum) (cdr datum)
        (error "bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if (not (null? proc)) (apply proc (map contents args))
                (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

; (define (deriv exp var)
;     (cond
;         ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp) (make-sum 
;                         (deriv (addend exp) var)
;                         (deriv (augend exp) var)))
;         ((product? exp) 
;             (make-sum
;                 (make-product 
;                     (multiplier exp)
;                     (deriv (multiplicand exp) var))
;                 (make-product 
;                     (deriv (multiplier exp) var)
;                     (multiplicand exp))))
;     ; 추가되는 규칙들
;         (else (error "unknown expression type -- DERIV" exp))))

(define operator car)
(define operands cdr)

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define *op-table* (make-hash))
(define (put op type proc)
    (hash-set! *op-table* (list op type) proc))
(define (get op type)
    (hash-ref *op-table* (list op type) '()))

; a deriv에서 사용하는 프로시저들을 데이터 중심 프로그래밍 기법으로 일반화된 연산으로 구현하였다.
; number?나 variable?을 데이터 중심 방식으로 다루지 못하는 이유는 이 두 프로시저는 한가지 데이터 타입만을 처리하기 때문이다.

; b
(define (install-sum-package)
    (define augend car)
    (define addend cadr)
    (define (deriv-sum exp var) 
        (make-sum 
            (deriv (addend exp) var)
            (deriv (augend exp) var)))
    (put 'deriv '+ deriv-sum)
    'done)
(define (install-product-package)
    (define (multiplier x) (car x))
    (define (multiplicand x) (cadr x))
    (define (deriv-product exp var)
        (make-sum
            (make-product 
                (multiplier exp)
                (deriv (multiplicand exp) var))
            (make-product 
                (deriv (multiplier exp) var)
                (multiplicand exp))))
    (put 'deriv '* deriv-product)
    'done)
(install-sum-package)
(install-product-package)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; c
(define (exponentiation? x) (and (pair? x) (eq? (car x) '^)))
(define (make-exponentiation base expo)
    (cond
        ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? expo 0) 1)
        ((=number? expo 1) base)
        ((and (number? base) (number? expo)) (expt base expo))
        (else (list '^ base expo))))
(define (install-exponent-package)
    (define base car)
    (define exponent cadr)
    (define (deriv-exponent exp var)
        (make-product
            (exponent exp)
            (make-exponentiation (base exp) (- (exponent exp) 1))))
    (put 'deriv '^ deriv-exponent)
    'done)
(install-exponent-package)
(deriv '(^ x 3) 'x) ; (* 3 (^ x 2))

; d
(define (deriv-d exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))
(define (install-deriv-package)
    (define augend car)
    (define addend cadr)
    (define (multiplier x) (car x))
    (define (multiplicand x) (cadr x))
    (define base car)
    (define exponent cadr)
    (define (deriv-sum exp var) 
        (make-sum 
            (deriv-d (addend exp) var)
            (deriv-d (augend exp) var)))
    (define (deriv-product exp var)
        (make-sum
            (make-product 
                (multiplier exp)
                (deriv-d (multiplicand exp) var))
            (make-product 
                (deriv-d (multiplier exp) var)
                (multiplicand exp))))
    (define (deriv-exponent exp var)
        (make-product
            (exponent exp)
            (make-exponentiation (base exp) (- (exponent exp) 1))))
    (put '+ 'deriv deriv-sum)
    (put '* 'deriv deriv-product)
    (put '^ 'deriv deriv-exponent)
    'done)
(install-deriv-package)
(deriv-d '(+ x 3) 'x); 1
(deriv-d '(* x y) 'x); y
(deriv-d '(* (* x y) (+ x 3)) 'x); (+ (* x y) (* y (+ x 3)))
(deriv-d '(^ x 3) 'x); (* 3 (^ x 2))
; put에 순서를 바꿔 적용하는 정도의 변화가 필요하다.