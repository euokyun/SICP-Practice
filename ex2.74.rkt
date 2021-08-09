#lang sicp
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *op-table* (make-hash))
(define (put op type proc)
    (hash-set! *op-table* (list op type) proc))
(define (get op type)
    (hash-ref *op-table* (list op type) '()))

; a
(define (get-record division empname)
    ((get 'get-record division) empname))
; 각 부서 파일에서는 get-record 프로시저를 put 해야 한다.

; b
(define (get-salary record) (get 'get-salary 'record) record)
; 레코드는 봉급 정보라는것을 알 수 있도록 타입 태그가 붙어 있어야 한다.

; c
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

(define (find-employee-record empname . divisions) 
    (apply-generic 'get-record divisions) empname)

; d
; put에 데이터 구조에 맞는 프로시저를 넣는다.