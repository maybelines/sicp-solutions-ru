#lang racket
  
 (define (simpson-integral f a b n) 
   (define (sum term a next b) 
     (if (> a b) 
       0 
       (+ (term a) (sum term (next a) next b)))) 
   (define (term x) 
     (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h))))) 
   (define (next x) 
     (+ x (* 2 h))) 
   (define h (/ (- b a) n)) 
   (* (/ h 3) (sum term a next (- b (* 2 h))))) 
  
  
 (define (cube x) 
   (* x x x)) 
  
  
 (display (simpson-integral cube 0 1 100)) (newline) 
 (display (simpson-integral cube 0 1 1000)) (newline) 