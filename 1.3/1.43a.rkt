#lang racket

; generates recursive process
; build f sequence lambda by lambda
(define (repeated f n)
    (lambda (x) 
            (if (= n 1) 
                (f x)
                (f ((repeated f (- n 1)) x)))))

(define (square x)
    (* x x))

; expected: 625, received: 625
((repeated square 2) 5) 

