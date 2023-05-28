#lang racket

(define (sub-interval interval1 interval2)
    (make-interval (- (lower-bound interval1) (lower-bound interval2))
                   (- (upper-bound interval1) (upper-bound interval2))))

; ----------abstraction-barrier------------

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))
