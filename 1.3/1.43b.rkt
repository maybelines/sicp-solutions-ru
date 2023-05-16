#lang racket

; generates recursive process
; another approach: create lambda only in the finish
(define (repeated f n)
    (define (collect-f count arg)
        (if (= count 1)
            (f arg)
            (f (collect-f (- count 1) arg))))
    (lambda (x) (collect-f n x)))  

(define (square x)
    (* x x))

; expected: 625, received: 625
((repeated square 2) 5) 
