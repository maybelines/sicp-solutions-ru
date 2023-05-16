#lang racket

; its easy generalization of double procedure from 1.41
(define (compose f g)
    (lambda (x) (f (g x))))

(define (inc x)
    (+ x 1))

(define (square x)
    (* x x))

; expected: 49, received: 49
((compose square inc) 6)