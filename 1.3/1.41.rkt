#lang racket
(require racket/trace)

(define (double f)
    (lambda (x) (f (f x))))

(define (inc x)
    (+ x 1))

; this expression returns 21, why?
(((double (double double)) inc) 5)

; note that it also returns 21:
((double (double (double (double inc)))) 5)

; let f = double
; 1. (double f) -> (f (f _)) 
; 2. (double (f (f _))) ->  ((f (f (f (f _)))) -> [f = double] ->
; -> ((double (double (double (double _)))))

; if we substitute inc instead _ :
; ((double (double (double (double inc)  )))) -> 
; ((double (double (double (inc (inc _)) )))) ->
; ((double (double (inc (inc (inc (inc _)))) )))) -> ...
; finally we get 16 = 2^4 inc procedures because 
; at each step we increase number of inc twice


