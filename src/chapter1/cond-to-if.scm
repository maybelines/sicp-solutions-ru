#lang racket

(define (p) (p)) 

(define (new-if true? true-case false-case)
    (cond (true? true-case)
          (else false-case)))  

(new-if #f (p) 0)

