#lang racket

;; its "inconvenient" way to implement selectors via a logarithm

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (nearest-int x)
    (floor (+ x 0.5)))

(define (get-member basement)
    (define (get-exp n) 
        (inexact->exact (floor (log n basement))))
    (lambda (n) 
        (nearest-int (log (gcd n (expt basement (get-exp n))) 
                          basement))))

;(define (car n)
;    (define exp (inexact->exact (floor (log n 2))))
;    (nearest-int (log (gcd n (expt 2 exp)) 2)))

;(define (cdr n)
;    (define exp (inexact->exact (floor (log n 3))))
;    (floor (+ (log (gcd n (expt 3 exp)) 3) 0.5)))

(define (car n)
    (inexact->exact ((get-member 2) n)))

(define (cdr n)
    (inexact->exact ((get-member 3) n)))

(define couple (cons 500 104))
(car couple)
(cdr couple)


