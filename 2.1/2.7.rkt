#lang racket

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (upper-bound interval)
    (car interval))

(define (lower-bound interval)
    (cdr interval))