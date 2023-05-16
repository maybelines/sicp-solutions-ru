#lang racket

; generates iterative process
(define (repeated f n)
    (define (iter count cur)
        (if (= count 0)
            cur
            (iter (- count 1) 
                  (lambda (x) (f (cur x))))))
    (iter n (lambda (x) x)))

(define (square x)
    (* x x))

; expected: 625, received: 625
((repeated square 2) 5) 
