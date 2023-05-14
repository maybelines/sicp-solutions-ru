#lang racket

(define tolerance 0.00001)

; a little changes in fixed-point:
; add arg "state" which stores number of procedure calls
; add subprocedure "print-intermediate-result" which outputs only
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) 
            (< (abs (- v1 v2)) tolerance)) 
    (define (print-intermediate-result count res)
        (display "intermediate try ")
        (display count)
        (display " --- ")
        (display res)
        (newline))
    (define (try state guess) 
        (print-intermediate-result state guess)
        (let ((next (f guess))) 
            (if (close-enough? guess next) 
                next 
                (try (+ state 1) next))))
    (try 1 first-guess))

(define (average x y)
    (/ (+ x y) 2))

(define (find-fixed-point)
    (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1))

(define (find-fixed-point-average-damping)
    (fixed-point (lambda (x) 
                         (average x (/ (log 1000) (log x)))) 
                 1.1))

; 37 steps to answer
(find-fixed-point)
; 13 steps to answer
(find-fixed-point-average-damping)
; conclusion: average damping is good technic 
; for acceleration of the search for a fixed point 