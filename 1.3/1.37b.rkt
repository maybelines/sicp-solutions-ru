#lang racket

(define phi-to-approx (/ 1 1.6180339887498948482))
(define accuracy 0.0001)

; generates iterative process 
(define (cont-frac n d k)
    (define (iter count res)
        (if (= count 0)
            res
            (iter (- count 1) (/ (n count) (+ (d count) res)))))
    (iter k 0))

(define (find-k-value)
    (define (iter k)
        (if (< (abs (- (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k) 
                       phi-to-approx))
               accuracy) 
            k
            (iter (+ k 1))))
    (iter 1))
    
; returns k = 10
(find-k-value)
; returns approx value for 1/phi
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) (find-k-value))