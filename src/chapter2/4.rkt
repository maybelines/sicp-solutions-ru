#lang racket

(define (cons x y) 
    (lambda (m) (m x y)))

(define (car z) 
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

; a kind of tests
; expected: 12, received: 12
(car (cons 12 16))
; expected: 107, received: 107
(cdr (cons 38 107))