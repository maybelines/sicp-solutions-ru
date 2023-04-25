#lang racket

(define (square n)
    (* n n))

(define (smallest-divisor n) 
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n) 
          ((divides? test-divisor n) test-divisor) 
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) 
    (= (remainder b a) 0))

;; 199 is prime number
(smallest-divisor 199)
;; 1999 is prime too
(smallest-divisor 1999)
;; 19999 is composite number
(smallest-divisor 19999)