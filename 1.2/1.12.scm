#lang racket

;; rewrite triangle to diagonal form
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; ...

(define (pascal-triangle line elem)
    (cond ((or (< elem 0) 
               (> elem line))
           #f)  
          ((or (= elem 0) 
               (= elem line)) 
           1)
          (else (+ (pascal-triangle (- line 1) elem) 
                   (pascal-triangle (- line 1) (- elem 1))))))


;; something like tests
;; expected: #t, received: #t
(= (pascal-triangle 4 0) 1)
(= (pascal-triangle 4 1) 4)
(= (pascal-triangle 4 2) 6)
(= (pascal-triangle 4 3) 4)
(= (pascal-triangle 4 4) 1)
