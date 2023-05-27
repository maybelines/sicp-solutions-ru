#lang racket

;; it is usual way to implement selectors 
;; using that gcd(2, 3) = 1 

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car n)
    (if (= (remainder n 2) 0)
        (+ 1 (car (/ n 2)))
        0))

(define (cdr n)
   (if (= (remainder n 3) 0)
        (+ 1 (cdr (/ n 3)))
        0))

; a kind of tests
(define couple1 (cons 500 104))
(define couple2 (cons 27 374))
(define couple3 (cons 853 90))

; expected: 500, received: 500
;           104            104
(car couple1)
(cdr couple1)

; expected: 27, received: 27
;           374           374
(car couple2)
(cdr couple2)

; expected: 853, received: 853
;           90             90
(car couple3)
(cdr couple3)


