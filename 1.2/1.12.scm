#lang racket

;; rewrite pascal's triangle into a diagonal shape
;; now for any three elements that enter into the figure below
;;  ___
;; |_ _|
;;   |_|
;;
;; hold true: the sum of the two upper elements is equal to the lower one, 
;; for example:
;; 1
;; 1 1
;; 1 2̲ ̲1̲
;; 1|3̲ 3̲|1
;; 1 4|6̲|4 1
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
