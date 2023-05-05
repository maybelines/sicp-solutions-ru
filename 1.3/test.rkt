#lang racket

;; we had to modify the condition because in other way 
;; we had wrong answer or ugly kind of argument
(define (sum term a next b)
    (if (or (> a b) (= a b)) 
        0 
        (+ (term a) (sum term (next a) next b))))

;; get-triple is correct because n is always an even integer
(define (simpson-integral f a b n) 
    (define h (/ (- b a) n)) 
    (define (get-triple x)
        (+ (f x) 
           (* 4 (f (+ x h))) 
           (f (+ x (* 2 h)))))
    (define (step x)
        (+ x (* 2 h)))
    (* (/ h 3) (sum get-triple a step b)))
    
(define (cube x) 
    (* x x x)) 

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)