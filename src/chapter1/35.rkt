#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) 
        (< (abs (- v1 v2)) tolerance)) 
    (define (try guess) 
        (let ((next (f guess))) 
            (if (close-enough? guess next) 
                next 
                (try next)))) 
    (try first-guess))

(define (find-golden-ratio)
    (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 
                 1))

; returns #t 
(< (- 1.6180339887 (find-golden-ratio)) 0.00001)