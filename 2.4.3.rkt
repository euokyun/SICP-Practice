#lang sicp
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type) 
  (hash-ref *op-table* (list op type) '()))

(define (square x) (* x x))
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum) (car datum)
        (error "bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum) (cdr datum)
        (error "bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (install-rectangular-package)
    ; 갇힌 프로시저
    (define real-part car)
    (define imag-part cdr)
    (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (define (angle z) (atan (imag-part z) (real-part z)))
    (define make-from-real-imag cons)
    (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
    ; 이 꾸러미Package의 인터페이스
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
        (lambda (x y) (tag (make-from-mag-ang x y))))
    'done
)

(define (install-polar-package)
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define magnitude car)
    (define angle cdr)
    (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
    (define make-from-mag-ang cons)
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
        (lambda (x y) (tag (make-from-mag-ang x y))))
    'done
)
(define (generic-common-datatype)
    (define (apply-generic op . args)
        (let ((type-tags (map type-tag args)))
            (let ((proc (get op type-tags)))
                (if proc (apply proc (map contents args))
                    (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-real-ang r a) ((get 'make-from-real-ang 'polar) r a))
'done
)

; message passing---
(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond
            ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude)
                (sqrt (+ (square x) (square y))))
            ((eq? op 'angle) (atan y x))
            (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
    dispatch)
; much easier
(define (apply-generic op arg) (arg op))