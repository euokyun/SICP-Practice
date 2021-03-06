#lang sicp
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
(define (square x) (* x x))
(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))
;coercion
(define *coercion-table* (make-hash))
(define (put-coercion op type proc) (hash-set! *coercion-table* (list op type) proc))
(define (get-coercion op type) (hash-ref *coercion-table* (list op type) '()))

(define (attach-tag type-tag contents)
    (cond
        ; ((number? contents) contents)
        ; ((symbol? contents) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
    (cond
        ; ((integer? datum) 'scheme-integer)
        ; ((number? datum) 'scheme-number)
        ; ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (error "bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (cond
        ; ((number? datum) datum)
        ; ((symbol? datum) datum)
        ((pair? datum) (cdr datum))
        (error "bad tagged datum -- CONTENTS" datum)))

; 2.82
(define (map-proc procs args)
    (cond
        ((or (null? procs) (null? args)) '())
        (else (cons ((car procs) (car args)) (map-proc (cdr procs) (cdr args))))))
(define (any proc lists)
    (cond
        ((null? lists) #f)
        ((null? proc) #f)
        ((pair? lists) (if (proc (car lists)) #t (any proc (cdr lists))))
        (else (proc lists))))
(define (apply-generic op . args)
    (define (apply-generic-1 op args)
        (let ((type-tags (map type-tag args)))
            (define (apply-coercion args)
                (define (apply-coercion-1 t)
                    (let ((coercions (map (lambda (x) 
                                                (if (eq? (type-tag x) (car t)) 
                                                    (lambda (x) x) 
                                                    (get-coercion (type-tag x) (car t))))
                                        args)))
                        (cond
                            ((null? t) (error "No coercion for these types -- APPLY-COERCION" (list op type-tags)))
                            ((any (lambda (x) (null? x)) coercions) (apply-coercion-1 (cdr t)))
                            (else ; (display "mapproc") (display (map-proc coercions args)) (newline) 
                                (map-proc coercions args)))))
                (apply-coercion-1 type-tags))
            (let ((proc (get op type-tags)))
                (if (not (null? proc)) (apply proc (map contents args))
                    ; (apply-coercion args type-tags)
                    (if (= (length args) 1) (error "No coercion for these types -- APPLY-GENERIC" (list op type-tags))
                        (apply-generic-1 op (apply-coercion args)))))))
    (apply-generic-1 op args))
            
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
    (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular (lambda (x y) (tag (make-from-mag-ang x y))))
    ;2.80
    (put '=zero? '(rectangular) (lambda (z) (and (= (real-part z) 0) (= (imag-part z) 0))))
    'done)

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
    (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar (lambda (x y) (tag (make-from-mag-ang x y))))
    ;2.80
    (put '=zero? '(polar) (lambda (z) (and (= (magnitude z) 0) (= (angle z) 0))))
    'done)

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
    ; (put 'make 'scheme-number (lambda (x) (tag x)))
    (put 'make 'scheme-number (lambda (x) (tag x)))
    ; 2.79
    (put 'equ? '(scheme-number scheme-number) (lambda (n1 n2) (= n1 n2)))
    ; 2.80
    (put '=zero? '(scheme-number) (lambda (n) (= 0 n)))
    ; 2.81
    (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
    ; 2.83
    (put-coercion 'scheme-number 'complex (lambda (x) (tag (make-complex-from-real-imag x 0))))
    (put 'raise 'scheme-number (lambda (x) ((get-coercion 'scheme-number 'complex) (contents x))))
    'done)

(define (install-rational-package)
    (define numer car)
    (define denom cdr)  
    (define (make-rat n d) 
        (display n) (display ", ") (display d) (newline)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat
            (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat
            (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat
            (* (numer x) (numer y))
            (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat
            (* (numer x) (denom y)) 
            (* (denom x) (numer y))))

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
    ; 2.79
    (put 'equ? '(rational rational) (lambda (rat1 rat2) (and (= (numer rat1) (numer rat2)) (= (denom rat1) (denom rat2)))))
    ; 2.80
    (put '=zero? '(rational) (lambda (n) (= 0 (numer n))))
    ; 2.83
    (put-coercion 'rational 'scheme-number (lambda (x)  (display "r->n :") (display x) (newline)
        (make-scheme-number (/ (car x) (cdr x)))))
    (put 'raise 'rational (lambda (x) ((get-coercion 'rational 'scheme-number) (contents x))))
    'done)

(define (install-complex-package)
    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang x y) ((get 'make-from-mag-ang 'polar) x y))

    (define (add-complex z1 z2)
        (make-from-real-imag 
            (+ (real-part z1) (real-part z2))
            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag 
            (- (real-part z1) (real-part z2))
            (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-real-imag 
            (* (magnitude z1) (magnitude z2))
            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-real-imag 
            (/ (magnitude z1) (magnitude z2))
            (- (angle z1) (angle z2))))
    
    (define (tag z) (attach-tag 'complex z))

    (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
    ; 2.77 added
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    ; 2.79
    (put 'equ? '(complex complex) (lambda (z1 z2) (and (= (magnitude z1) (magnitude z2)) (= (angle z1) (angle z2)))))
    ; 2.80
    (define (=zero? z) (apply-generic '=zero? z))
    (put '=zero? '(complex) =zero?)
    ; 2.5.2
    (define (add-complex-to-schemenum z x)
        (make-complex-from-real-imag (+ (real-part z) x) (imag-part z)))
    (put 'add '(complex scheme-number) add-complex-to-schemenum)
    'done)

; 2.83 add integer type
(define (install-scheme-integer-package)
    (define (tag x) (attach-tag 'scheme-integer x))
    (put 'add '(scheme-integer scheme-integer) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-integer scheme-integer) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-integer scheme-integer) (lambda (x y) (tag (* x y))))
    ; (put 'div '(scheme-integer scheme-integer) (lambda (x y) (tag (quotient x y))))
    ; (put 'make 'scheme-integer (lambda (x) (tag x)))
    (put 'make 'scheme-integer (lambda (x) (tag x)))
    ; 2.79
    (put 'equ? '(scheme-integer scheme-integer) (lambda (n1 n2) (= n1 n2)))
    ; 2.80
    (put '=zero? '(scheme-integer) (lambda (n) (= 0 n)))
    ; 2.81
    (put 'exp '(scheme-integer scheme-integer) (lambda (x y) (tag (expt x y))))
    ; 2.83
    (put-coercion 'scheme-integer 'rational (lambda (x) (make-rational x 1)))
    (put 'raise 'scheme-integer (lambda (x) ((get-coercion 'scheme-integer 'rational) (contents x))))
    'done)

(install-scheme-integer-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-rational-package)
(define (make-scheme-integer n) ((get 'make 'scheme-integer) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
(define (equ? n1 n2) (apply-generic 'equ? n1 n2))
(define (=zero? n) (apply-generic '=zero? n))



(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)
; 2.81
(define (exp x y) (apply-generic 'exp x y))
; ; 2.83
; (put-coercion 'scheme-integer 'rational (lambda (x) (make-rational (contents x) 1)))
; (put 'raise 'scheme-integer (lambda (x) ((get-coercion 'scheme-integer 'rational) x)))
; (put-coercion 'scheme-number 'complex (lambda (x) (make-complex-from-real-imag x 0)))
; (put 'raise 'scheme-number (lambda (x) ((get-coercion 'scheme-number 'complex) x)))
; (put-coercion 'rational 'complex (lambda (x) (make-complex-from-real-imag x 0)))
; (put 'raise 'rational (lambda (x) ((get-coercion 'rational 'complex) x)))

(define cpx1 (make-complex-from-real-imag 2 1))
(define cpx2 (make-complex-from-real-imag 1 2))

(define (raise v) ((get 'raise (type-tag v)) v))
(make-scheme-integer 3) ; 3
(raise (make-scheme-integer 3)) ; (rational 3 . 1)
(raise (raise (make-scheme-integer 3))) ; (scheme-number . 3)
(raise (raise (raise (make-scheme-integer 3)))) ; (complex rectangular 3 . 0)
; (type-tag (raise (raise (make-scheme-integer 3)))) ; (complex rectangular (rational 3 . 1) . 0)
; (type-tag (/ 3 1))