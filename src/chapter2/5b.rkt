#lang racket

;; it is "inconvenient" way to implement selectors via a logarithm

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car n)
    (inexact->exact ((get-member 2) n)))

(define (cdr n)
    (inexact->exact ((get-member 3) n)))

(define (get-member basement)
    (define (get-exp n) 
        (inexact->exact (floor (log n basement))))
    (lambda (n) 
        (nearest-int (log (gcd n (expt basement (get-exp n))) 
                          basement))))

(define (nearest-int x)
    (floor (+ x 0.5)))

; a kind of tests
(define couple1 (cons 500 104))
(define couple2 (cons 27 374))
(define couple3 (cons 853 90))

; expected: 500, received: 500
;           104            104
(car couple1)
(cdr couple1)

; expected: 27, received: 27
;           374           374
(car couple2)
(cdr couple2)

; expected: 853, received: 853
;           90             90
(car couple3)
(cdr couple3)


