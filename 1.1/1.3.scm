#lang racket

(define (square a) (* a a))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (sum-of-squares-max-couple a b c)
    (cond ((and (>= a b) (>= a c)) 
                (if (> b c) 
                    (sum-of-squares a b) 
                    (sum-of-squares a c)))
          ((and (>= b a) (>= b c)) 
                (if (> a c) 
                    (sum-of-squares b a) 
                    (sum-of-squares b c)))
          (else 
                (if (> a b) 
                    (sum-of-squares c a) 
                    (sum-of-squares c b)))))
