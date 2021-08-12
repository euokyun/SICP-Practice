#lang sicp
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
; (define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (> n 1) (compose f (repeated f (- n 1))) f))

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
    ; (display "type: ") (display (car datum)) (display ", ") (display (cdr datum)) (newline)
    (cond
        ((integer? datum) 'scheme-integer)
        ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
; (display "contents: ") (display (car datum)) (display ", ") (display (cdr datum)) (newline)

    (cond
        ((number? datum) datum)
        ; ((symbol? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "bad tagged datum -- CONTENTS" datum))))

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

; 2.85
(define (drop v)
    (if (or (boolean? v) (eq? (type-tag v) (car number-tower))) v
        (let ((subtyped (project v)))
; (display "subtyped : ") (display subtyped) (newline)
; (display "v : ") (display v) (newline)
; (display "(raise subtyped) : ") (display (raise subtyped)) (newline)
; (display "eq : ") (display (equal? v (raise subtyped))) (newline)
            (if (equal? v (raise subtyped)) (drop subtyped) 
                v))))
            ; (cond
            ;     ((equal? v (raise subtyped)) (drop subtyped))
            ;     (else v)
            ; ))))
; (define (apply-generic op . args)
;     (define (apply-generic-1 op args)
;         (let ((type-tags (map type-tag args)))
;             (define apply-coercion raise-to-top)
;             (let ((proc (get op type-tags)))
;                 (if (not (null? proc)) (drop (apply proc (map contents args)))
;                 ; (if (not (null? proc)) (apply proc (map contents args))
;                     (if (= (length args) 1) (error "No coercion for these types -- APPLY-GENERIC" (list op type-tags))
;                         (apply-generic-1 op (apply-coercion args)))))))
;     (apply-generic-1 op args))
; 2.86
(define (list-equal? m)
; (display "list-equal?: ") (display m) (newline)
    (cond
        ((null? m) #t)
        ((null? (cdr m)) #t)
        ((equal? (car m) (cadr m)) (list-equal? (cdr m)))
        (else #f)))

(define (apply-generic op . args)
    ; (display "apply-generic: ") (display op) (display ", ") (display args) (newline)
    (define (apply-generic-1 op args)
        (let ((type-tags (map type-tag args)))
            (define apply-coercion raise-to-top)
            (let ((proc (get op type-tags)))
                ; (display "proc: ")(display proc) (newline)
                (if (not (null? proc)) (drop (apply proc (map contents args)))
                    (apply-generic-1 op (apply-coercion args))))))
        (apply-generic-1 op args))
            
(define (install-rectangular-package)
    ; 갇힌 프로시저
    (define real-part car)
    (define imag-part cdr)
    ; (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    ; 2.86
    (define (square x) (mul x x))
    (define (magnitude z) (make-scheme-number (sqrt-number (add (square (real-part z)) (square (imag-part z))))))
    (define (angle z) ;(display "angle: ") (display z) (newline) 
        (arctangent (imag-part z) (real-part z)))
    (define (make-from-real-imag x y) (cons x y))
    (define (make-from-mag-ang r a)  (cons (mul r (cosine a)) (mul r (sine a))))
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
    ; (define (real-part z) (* (magnitude z) (cos (angle z))))
    ; (define (imag-part z) (* (magnitude z) (sin (angle z))))
    ; 2.86
    (define (square x) (mul x x))
    (define (real-part z) (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z) (mul (magnitude z) (sine (angle z))))
    (define magnitude car)
    (define angle cdr)
    ; 2.86
    ; (define (make-from-real-imag x y) (cons (sqrt (+ (square (contents x)) (square (contents y)))) (atan (contents y) (contents x))))
    ; (define (make-from-mag-ang r a) (cons (contents r) (contents a)))
    (define (make-from-real-imag x y) (cons (sqrt-number (add (square x) (square y))) (arctangent y x)))
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
    (put 'equ? '(scheme-number scheme-number) (lambda (n1 n2) (display "equ?num") (newline) (= (contents n1) (contents n2))))
    ; 2.80
    (put '=zero? '(scheme-number) (lambda (n) (= 0 n)))
    ; 2.81
    (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
    ; 2.83
    (put-coercion 'scheme-number 'complex (lambda (x) (tag (make-complex-from-real-imag x 0))))
    (put 'raise 'scheme-number (lambda (x) ; (display "num->com : ") (display x) (newline) 
        ((get-coercion 'scheme-number 'complex) (contents x))))
    ; 2.85
    (define (project x) (make-rational (contents x) 1))
    (put 'project 'scheme-number project)
    ; 2.86
    (put 'sqrt-number '(scheme-number) (lambda (x) (sqrt (contents x))))
    'done)

(define (install-rational-package)
    (define numer car)
    (define denom cdr)  
    (define (make-rat n d) 
        ; (display n) (display ", ") (display d) (newline)
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
    (put 'equ? '(rational rational) (lambda (rat1 rat2) ; (display "equ?rat ") (display rat1) (display ", ") (display rat2) (newline) 
        (and (= (numer rat1) (numer rat2)) (= (denom rat1) (denom rat2)))))
    ; 2.80
    (put '=zero? '(rational) (lambda (n) (= 0 (numer n))))
    ; 2.83
    (put-coercion 'rational 'scheme-number (lambda (x) ; (display "r->n :") (display x) (newline)
        (make-scheme-number (/ (car x) (cdr x)))))
    (put 'raise 'rational (lambda (x) ((get-coercion 'rational 'scheme-number) (contents x))))
    ; 2.85
    (define (project x) (make-scheme-integer (numer (contents x))))
    (put 'project 'rational project)
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
        ; (display "add-complex-to-schemenum") (display z) (display x) (newline)
        (make-complex-from-real-imag (+ (real-part z) x) (imag-part z)))
    (put 'add '(complex scheme-number) add-complex-to-schemenum)
    ; 2.85
    (define (project x) (make-scheme-number (real-part x)))
    (put 'project 'complex project)
    'done)

; 2.83 add integer type
(define (install-scheme-integer-package)
    (define (tag x) (attach-tag 'scheme-integer x))
    (put 'add '(scheme-integer scheme-integer) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-integer scheme-integer) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-integer scheme-integer) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-integer scheme-integer) (lambda (x y) (tag (quotient x y))))
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
; 2.81
(define (exp x y) (apply-generic 'exp x y))

(define rat1 (make-rational 5 2))

(define cpx-1 (make-complex-from-real-imag 2 -1))
(define cpx1 (make-complex-from-real-imag 2 1))
(define cpx2 (make-complex-from-real-imag 1 2))
; 2.83
(define (raise v) ((get 'raise (type-tag v)) v))

; 2.84
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (element-of-set? x set) 
    (any (lambda (e) (equal? e x)) set))

(define (list-index x set)
    (define (list-index-1 x s index)
        (cond
            ((null? s) #f)
            ((equal? x (car s)) index)
            (else (list-index-1 x (cdr s) (+ index 1)))))
    (list-index-1 x set 0))

(define number-tower (list 'scheme-integer 'rational 'scheme-number 'complex))
; 2.85
; return droped type or null if it has no project procedure 
(define (project v) (let ((subtype (get 'project (type-tag v)))) (if (procedure? subtype) (subtype v) '())))

; 2.86
(define (2.86-complex-package)
    (define (tag z) (attach-tag 'complex z))
    ; for integer, rational
    (define (sine x) (make-scheme-number (sin (real-part x))))
    (define (cosine x) (make-scheme-number (cos (real-part x))))
    (define (arctangent x y) (make-scheme-number (atan (real-part x) (real-part y))))
    (put 'sine '(complex) sine)
    (put 'cosine '(complex) cosine)
    (put 'arctangent '(complex complex) arctangent)
    'done
)
(2.86-complex-package)
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctangent x y) (apply-generic 'arctangent x y))
; (sine (add cpx1 cpx-1)) ; (rational -3408335435861847.0 . 4503599627370496.0)
; (cosine (add cpx1 cpx-1)) ; (rational -5887498334708929.0 . 9007199254740992.0)
; (arctangent cpx1 cpx-1) ;(rational 884279719003555.0 . 1125899906842624.0)

; 문제 : raise-to-top이 자신의 요소에 한정해서 raise함.
; 모든 것이 실패했을때 최종적으로 가장 높은 타입까지 올리는 방법을 만들자.
; 2.86
(define (raise-to-top elems) 
    ; (display "elems: ")(display elems) (newline)
    (let ((levelmap (map (lambda (x) (list-index (type-tag x) number-tower)) elems))) ; 인자가 하나면 에러남.
        ; (display "levelmap: ") (display levelmap) (newline)
        (if (any (lambda (x) (eq? x #f)) levelmap) (error "error: not in type tree -- RAISE-TO-TOP" elems)
            (if (list-equal? levelmap) (map raise elems)
                (let ((maxlevel (apply max levelmap)))
                    ; (display "raise lvl: ") (newline)
                    (apply map 
                        (lambda (e l) 
                            (cond
                                ((= l maxlevel) e)
                                ((< l maxlevel) ((repeated raise (- maxlevel l)) e))))
                        (list elems levelmap)))))))

(define (apply-generic-test op . args)
    ; (display op) (display ", ") (display args) (display ", ") (newline)
    (define (apply-generic-1 op args)
        (let ((type-tags (map type-tag args)))
            (define apply-coercion raise-to-top)
            (let ((proc (get op type-tags)))
            ; (display "proc ")(display proc)(newline)
                (if (not (null? proc)) (drop (apply proc (map contents args)))
                    (apply-generic-1 op (apply-coercion args))))))
        (apply-generic-1 op args))
(define (sqrt-number x) (apply-generic 'sqrt-number x))


; (make-complex-from-real-imag (add cpx1 cpx-1) (add cpx1 cpx-1))
; (make-complex-from-mag-ang (add cpx1 cpx-1) (add cpx1 cpx-1))
; (magnitude (make-complex-from-real-imag rat1 rat1)) ; 3.5355339059327378
; (angle (make-complex-from-real-imag rat1 rat1)) ; (rational 884279719003555.0 . 1125899906842624.0)
; (atan 2.5 2.5) ;0.7853981633974483
; (drop #f)
; (make-complex-from-real-imag rat1 rat1)
; (arctangent 2.5 2.5)

(equ? (angle (make-complex-from-real-imag rat1 rat1)) (arctangent rat1 rat1)) ; #t
(magnitude (make-complex-from-real-imag rat1 rat1)) ; (rational 124395540479019.0 . 35184372088832.0)
(real-part (make-complex-from-mag-ang rat1 rat1)) ; (rational -2255018805279639.0 . 1125899906842624.0)