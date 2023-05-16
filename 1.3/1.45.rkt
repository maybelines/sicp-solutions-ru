#lang racket

(define tolerance 0.00001)

; procedure from 1.43
(define (repeated f n)
    (define (iter count cur)
        (if (= count 0)
            cur
            (iter (- count 1) 
                  (lambda (x) (f (cur x))))))
    (iter n (lambda (x) x)))

; procedure from 1.40
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) 
        (< (abs (- v1 v2)) tolerance)) 
    (define (try guess) 
        (let ((next (f guess))) 
             (if (close-enough? guess next) 
                  next (try next)))) 
    (try first-guess))

; procedure from average-damping subsection
(define (average-damp f) 
    (lambda (x) (average x (f x))))

(define (average x y)
    (/ (+ x y) 2))

(define quest-times 5)

(define (find-n-deg-root x n)
    (fixed-point ((repeated average-damp quest-times) 
                  (lambda (y) 
                          (/ x (expt y (- n 1)))))
                 1.0))

(find-n-deg-root 16 4)