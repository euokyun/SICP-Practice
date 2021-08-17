#lang sicp
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))
; from 2.86
(define *coercion-table* (make-hash))
(define (put-coercion op type proc) (hash-set! *coercion-table* (list op type) proc))
(define (get-coercion op type) (hash-ref *coercion-table* (list op type) '()))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (> n 1) (compose f (repeated f (- n 1))) f))
(define (attach-tag type-tag contents)
    (cond
        ; ((number? contents) contents)
        ; ((symbol? contents) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
    (cond
        ((integer? datum) 'scheme-integer)
        ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
    (cond
        ((number? datum) datum)
        ((symbol? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "bad tagged datum -- CONTENTS" datum))))
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
(define (drop v)
    (if (or (boolean? v) (not (element-of-set? (type-tag v) number-tower)) (eq? (type-tag v) (car number-tower))) v
        (let ((subtyped (project v)))
            (if (equal? v (raise subtyped)) (drop subtyped) 
                v))))
(define (list-equal? m)
    (cond
        ((null? m) #t)
        ((null? (cdr m)) #t)
        ((equal? (car m) (cadr m)) (list-equal? (cdr m)))
        (else #f)))
(define (apply-generic op . args)
    (define apply-coercion raise-to-top)
    (define (apply-generic-1 op args)
        (let ((type-tags (map type-tag args)))
            (let ((proc (get op type-tags)))
                (if (not (null? proc)) (drop (apply proc (map contents args)))
                    (apply-generic-1 op (apply-coercion args))))))
        (apply-generic-1 op args))
(define (install-rectangular-package)
    (define real-part car)
    (define imag-part cdr)
    (define (square x) (mul x x))
    (define (magnitude z) (make-scheme-number (sqrt-number (add (square (real-part z)) (square (imag-part z))))))
    (define (angle z) (arctangent (imag-part z) (real-part z)))
    (define (make-from-real-imag x y) (cons x y))
    (define (make-from-mag-ang r a)  (cons (mul r (cosine a)) (mul r (sine a))))
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular (lambda (x y) (tag (make-from-mag-ang x y))))
    (put '=zero? '(rectangular) (lambda (z) (and (= (real-part z) 0) (= (imag-part z) 0))))
    'done)

(define (install-polar-package)
    ; 2.86
    (define (square x) (mul x x))
    (define (real-part z) (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z) (mul (magnitude z) (sine (angle z))))
    (define magnitude car)
    (define angle cdr)
    (define (make-from-real-imag x y) (cons (sqrt-number (add (square x) (square y))) (arctangent y x)))
    (define make-from-mag-ang cons)
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar (lambda (x y) (tag (make-from-mag-ang x y))))
    (put '=zero? '(polar) (lambda (z) (and (= (magnitude z) 0) (= (angle z) 0))))
    'done)

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number) (lambda (n1 n2) (= (contents n1) (contents n2))))
    (put '=zero? '(scheme-number) (lambda (n) (= 0 n)))
    (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
    (put-coercion 'scheme-number 'complex (lambda (x) (tag (make-complex-from-real-imag x 0))))
    (put 'raise 'scheme-number (lambda (x) ((get-coercion 'scheme-number 'complex) (contents x))))
    (put 'project 'scheme-number (lambda (x) (make-rational (contents x) 1)))
    (put 'sqrt-number '(scheme-number) (lambda (x) (sqrt (contents x))))
    'done)

(define (install-rational-package)
    (define numer car)
    (define denom cdr)  
    (define (make-rat n d) 
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
    (put 'equ? '(rational rational) (lambda (rat1 rat2) (and (= (numer rat1) (numer rat2)) (= (denom rat1) (denom rat2)))))
    (put '=zero? '(rational) (lambda (n) (= 0 (numer n))))
    (put-coercion 'rational 'scheme-number (lambda (x) (make-scheme-number (/ (car x) (cdr x)))))
    (put 'raise 'rational (lambda (x) ((get-coercion 'rational 'scheme-number) (contents x))))
    (put 'project 'rational (lambda (x) (make-scheme-integer (numer (contents x)))))
    'done)

(define (install-complex-package)
    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang x y) ((get 'make-from-mag-ang 'polar) x y))
    (define (add-complex z1 z2)
        (make-from-real-imag 
            (add (real-part z1) (real-part z2))
            (add (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag 
            (sub (real-part z1) (real-part z2))
            (sub (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-real-imag 
            (mul (magnitude z1) (magnitude z2))
            (add (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-real-imag 
            (/ (magnitude z1) (magnitude z2))
            (sub (angle z1) (angle z2))))
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'equ? '(complex complex) (lambda (z1 z2) (and (= (magnitude z1) (magnitude z2)) (= (angle z1) (angle z2)))))
    (put '=zero? '(complex) (lambda (z) (apply-generic '=zero? z)))
    (define (add-complex-to-schemenum z x) (make-complex-from-real-imag (+ (real-part z) x) (imag-part z)))
    (put 'add '(complex scheme-number) add-complex-to-schemenum)
    (put 'project 'complex (lambda (x) (make-scheme-number (real-part x))))
    'done)

(define (install-scheme-integer-package)
    (define (tag x) (attach-tag 'scheme-integer x))
    (put 'add '(scheme-integer scheme-integer) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-integer scheme-integer) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-integer scheme-integer) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-integer scheme-integer) (lambda (x y) (tag (quotient x y))))
    (put 'make 'scheme-integer (lambda (x) (tag x)))
    (put 'equ? '(scheme-integer scheme-integer) (lambda (n1 n2) (= n1 n2)))
    (put '=zero? '(scheme-integer) (lambda (n) (= 0 n)))
    (put 'exp '(scheme-integer scheme-integer) (lambda (x y) (tag (expt x y))))
    (put-coercion 'scheme-integer 'rational (lambda (x) (make-rational x 1)))
    (put 'raise 'scheme-integer (lambda (x) ((get-coercion 'scheme-integer 'rational) (contents x))))
    'done)

(install-scheme-integer-package)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-rational-package)
(define (make-scheme-integer n) ((get 'make 'scheme-integer) n))
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
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
(define (exp x y) (apply-generic 'exp x y))
(define (raise v) ((get 'raise (type-tag v)) v))
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)
(define (element-of-set? x set) (any (lambda (e) (equal? e x)) set))

(define (list-index x set)
    (define (list-index-1 x s index)
        (cond
            ((null? s) #f)
            ((equal? x (car s)) index)
            (else (list-index-1 x (cdr s) (+ index 1)))))
    (list-index-1 x set 0))

(define number-tower (list 'scheme-integer 'rational 'scheme-number 'complex))
(define (project v) (let ((subtype (get 'project (type-tag v)))) (if (procedure? subtype) (subtype v) '())))
(define (2.86-complex-package)
    (define (tag z) (attach-tag 'complex z))
    (put 'sine '(complex) (lambda (x) (make-scheme-number (sin (real-part x)))))
    (put 'cosine '(complex) (lambda (x) (make-scheme-number (cos (real-part x)))))
    (put 'arctangent '(complex complex) (lambda (x y) (make-scheme-number (atan (real-part x) (real-part y)))))
    'done)

(2.86-complex-package)
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctangent x y) (apply-generic 'arctangent x y))
(define (raise-to-top elems) 
    (let ((levelmap (map (lambda (x) (list-index (type-tag x) number-tower)) elems))) ; 인자가 하나면 에러남.
        (if (any (lambda (x) (eq? x #f)) levelmap) (error "error: not in type tree -- RAISE-TO-TOP" elems)
            (if (list-equal? levelmap) (map raise elems)
                (let ((maxlevel (apply max levelmap)))
                    (apply map 
                        (lambda (e l) 
                            (cond
                                ((= l maxlevel) e)
                                ((< l maxlevel) ((repeated raise (- maxlevel l)) e))))
                        (list elems levelmap)))))))
(define (sqrt-number x) (apply-generic 'sqrt-number x))
; end 2.86

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (variable? x) (symbol? x))

(define (install-polynomial-package)
    ; 변수variable 하나와 마디 리스트term-list 하나를 묶어서 다항식polynomial을 만듦
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p)) ; 다항식에서 변수를 찾아냄
    (define (term-list p) (cdr p)) ; 다항식에서 마디 리스트를 찾아냄 

    ; adjoin-term,  ... coeff procedure
    (define (adjoin-term term term-list) 
        (if (=zero? (coeff term)) term-list
            (cons term term-list)))
    (define (the-empty-termlist) '()) ; generate empty term list
    (define (empty-termlist? term-list) (null? term-list))
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (make-term order coeff) (list order coeff)) ; 마디를 만들어 내는 프로시저
    (define (order term) (car term)) ; 차수
    (define (coeff term) (cadr term)) ; 계수

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2)) 
            (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
            
    (define (add-terms L1 L2)
        (cond
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else (let ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond
                    ((> (order t1) (order t2)) 
                        (adjoin-term 
                            t1 
                            (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2)) 
                        (adjoin-term 
                            t2 
                            (add-terms L1 (rest-terms L2))))
                    (else (adjoin-term 
                            (make-term (order t1) (add (coeff t1) (coeff t2))) ; 일반화된 프로시저인 add를 사용함
                            (add-terms (rest-terms L1) (rest-terms L2)))))))))

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1) (the-empty-termlist)
            (add-terms 
                (mul-term-by-all-terms (first-term L1) L2)
                (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L) (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term 
                    (make-term 
                        (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    ; 2.87
    (define (=zero? z)
        (define (=zero?-1 z)
            (cond
                ((null? z) #t)
                ((equ? (coeff (first-term z)) 0) (=zero?-1 (rest-terms z)))
                ; ((and (equ? (order (first-term z)) 0) (equ? (coeff (first-term z)) 0)) (=zero?-1 (rest-terms z)))
                (else #f)))
        (if (empty-termlist? (term-list z)) #t
            (=zero?-1 (term-list z))))
    (put '=zero? '(polynomial) =zero?)
    'done)
(install-polynomial-package)
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))
; (define (add p1 p2) ((get 'add '(polynomial polynomial)) p1 p2))
; (define (mul p1 p2) ((get 'mul '(polynomial polynomial)) p1 p2))

; (define (make-term order coeff) (list order coeff))
; (define (variable p) (car p))
; (define (term-list p) (cdr p)) ; 다항식에서 마디 리스트를 찾아냄 
; (define (first-term term-list) (car term-list))
; (define (order term) (car term)) ; 차수
; (define (coeff term) (cadr term)) ; 계수
; (define (rest-terms term-list) (cdr term-list))
; (define (empty-termlist? term-list) (null? term-list))

; x^100+2x^2+1
; (define term1 '(x ((100 1) (2 2) (0 1)))) ; (x ((100 1) (2 2) (0 1)))
; (define term2 '(x ((5 1) (4 2) (2 3) (1 -2) (0 -5)))) ; (x ((5 1) (4 2) (2 3) (1 -2) (0 -5)))
; (define term3 '(y ((100 1) (2 2) (0 1)))) ; (y ((100 1) (2 2) (0 1)))
; (define term4 (list 'y (make-term 2 term1))) ; (y ((2 (x ((100 1) (2 2) (0 1)))) (2 2) (0 1)))
(define poly1 (make-polynomial 'x '((100 1) (2 2) (0 1))))
(define poly2 (make-polynomial 'x '()))
(define poly3 (make-polynomial 'x '((0 0))))
poly1 ; (polynomial x (100 1) (2 2) (0 1))
poly2 ; (polynomial x)
poly3 ; (polynomial x (0 0))
; (term-list (contents poly1)) ; ((100 1) (2 2) (0 1))
; (term-list (contents poly2)) ; ()
; (term-list (contents poly3)) ; ((0 0))
; (empty-termlist? (term-list (contents poly1))) ; #f
; (empty-termlist? (term-list (contents poly2))) ; #t
; (empty-termlist? (term-list (contents poly3))) ; #f
(=zero? poly1) ; #f
(=zero? poly2) ; #t
(=zero? poly3) ; #t
(=zero? (make-polynomial 'x '((100 0) (10 0) (1 0) (0 0))))