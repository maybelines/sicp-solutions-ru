#lang racket

(define (print-interval i) 
    (display (center i)) 
    (display ", ") 
    (display (percent i)) 
    (display "%")
    (newline))

; ----------abstraction-barrier------------

(define (make-center-percent c p)
    (let ((w (* c (/ p 100))))
        (make-center-width c w)))

(define (percent i)
    (* 100 (/ (width i) (center i))))

; ----------abstraction-barrier------------

(define (make-center-width c w) 
    (make-interval (- c w) (+ c w)))

(define (center i) 
    (/ (+ (lower-bound i) 
          (upper-bound i)) 
       2.0))

(define (width i) 
    (/ (- (upper-bound i) 
          (lower-bound i)) 
       2.0))

; ----------abstraction-barrier------------

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

; ----------abstraction-barrier------------\

; a kind of test
(define x (make-center-percent 100 10))
(define y (make-center-percent 50 12.5))

; expected: 100.0, 10.0%, received: 100.0, 10.0%
(print-interval x)

; expected: 50.0, 12.5%, received: 50.0, 12.5%
(print-interval y)


