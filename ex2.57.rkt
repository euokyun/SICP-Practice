#lang sicp

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp)
            (make-sum 
                (deriv (addend exp) var)
                (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
                (make-product 
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product 
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        ((exponentiation? exp)
            (make-product
                (exponent exp)
                (make-exponentiation (base exp) (- (exponent exp) 1))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '^)))
(define (make-exponentiation e1 e2)
    (cond
        ((=number? e1 0) 0)
        ((=number? e1 1) 1)
        ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
        (else (list '^ e1 e2))))
(define (base x)
    (if (exponentiation? x) (cadr x) x))
(define (exponent x)
    (if (exponentiation? x) (caddr x) 1))



(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define augend cadr)
(define multiplier cadr)

(define (make-sum a1 a2)
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2) 
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (addend x) 
   (if (null? (cdddr x)) (caddr x)
        (cons '+ (cddr x))))
(define (multiplicand x)
    (if (null? (cdddr x)) (caddr x)
        (cons '* (cddr x))))

(equal? (deriv '(* (* x y) (+ x 3)) 'x) (deriv '(* x y (+ x 3)) 'x))