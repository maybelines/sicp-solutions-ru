#lang racket

(define (midpoint-segment seg)
    (average-point (start-segment seg) (end-segment seg)))

; ----------abstraction-barrier------------

(define (make-segment pnt1 pnt2)
    (cons pnt1 pnt2))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (print-point p) 
    (newline) 
    (display "(") 
    (display (x-point p)) 
    (display ", ") 
    (display (y-point p)) 
    (display ")"))

(define (average-point pnt1 pnt2)
    (define (average x y)
        (/ (+ x y) 2.0))
    (make-point 
        (average (x-point pnt1) (x-point pnt2))
        (average (y-point pnt1) (y-point pnt2))))

; ----------abstraction-barrier------------

(define (make-point x y)
    (cons x y))

(define (x-point pnt)
    (car pnt))

(define (y-point pnt)
    (cdr pnt))

; ----------abstraction-barrier------------

; a kind of tests
; expected: (0, 2.5), received: (0, 2.5)
(print-point
    (midpoint-segment  
        (make-segment (make-point 0 0)
                      (make-point 0 5))))

; expected: (6.0, 4.0), received: (6.0, 4.0)
(print-point
    (midpoint-segment 
        (make-segment (make-point 2 3)
                      (make-point 10 5))))

; expected: (6.0, 4.0), received: (6.0, 4.0)
(print-point
    (midpoint-segment 
        (make-segment (make-point 10 5)
                      (make-point 2 3))))

; expected: (0, 0), received: (0, 0)
(print-point
    (midpoint-segment 
        (make-segment (make-point 0 0)
                      (make-point 0 0))))