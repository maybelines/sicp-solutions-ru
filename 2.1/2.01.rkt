#lang racket

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) 
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) 
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) 
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y)) 
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y)) 
       (* (numer y) (denom x))))

(define (print-rat-safely x)
    (if (false? x)
        (shows-the-denominator-is-zero-text)
        (print-rat x)))

(define (print-rat x)
    (display (numer x)) 
    (display "/")  
    (display (denom x))
    (newline))

; ----------abstraction-barrier------------
; upgrade of make-rat procedure for negative numbers and zero
(define (make-rat n d)
    (let ((g (abs (gcd n d)))
          (abs-n (abs n))
          (abs-d (abs d)))
    (cond ((< (* n d) 0)
           (cons (- 0 (/ abs-n g)) 
                 (/ abs-d g)))
          ((> (* n d) 0)
           (cons (/ abs-n g) 
                 (/ abs-d g)))
          (else (if (= n 0) 
                    (cons 0 1)
                    #f)))))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define (shows-the-denominator-is-zero-text)
    (display "the denominator must not be a zero")
    (newline))
    
; ----------abstraction-barrier------------
; something like tests
; expected: 5/16, received: 5/16
(print-rat-safely (make-rat 25 80))

; expected: 1/2, received: 1/2
(print-rat-safely (make-rat -15 -30))

; expected: "the denominator must not be a zero", 
; received: "the denominator must not be a zero"
(print-rat-safely (make-rat 15 0))

; expected: -1/2, received: -1/2
(print-rat-safely (make-rat 1 -2))

; expected: 0/1, received: 0/1
(print-rat-safely (make-rat 0 14273845634563245))
