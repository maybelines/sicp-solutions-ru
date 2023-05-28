#lang racket

(define (print-interval i) 
    (display (center i)) 
    (display ", ") 
    (display (percent i)) 
    (display "%")
    (newline))

; ----------abstraction-barrier------------

(define (make-center-percent c p)
    (let ((w (* c (/ p 100))))
        (make-center-width c w)))

(define (percent i)
    (* 100 (/ (width i) (center i))))

; ----------abstraction-barrier------------

(define (make-center-width c w) 
    (make-interval (- c w) (+ c w)))

(define (center i) 
    (/ (+ (lower-bound i) 
          (upper-bound i)) 
       2.0))

(define (width i) 
    (/ (- (upper-bound i) 
          (lower-bound i)) 
       2.0))

; ----------abstraction-barrier------------

; counts according to (R1 * R2)/(R1 + R2)
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2) 
                  (add-interval r1 r2)))

; counts according to 1/(1/R1 + 1/R2)
(define (par2 r1 r2)
    (let ((one (make-interval 1 1))) 
        (div-interval one 
                      (add-interval (div-interval one r1) 
                                    (div-interval one r2)))))

; ----------abstraction-barrier------------

(define (div-interval x y)
        (if (is-end-of-interval-zero? y)
            (raise-arguments-error 'div-interval
                                   "one of the ending points of interval is zero")
            (mul-interval x (inverse-by-mul-interval y))))

; ----------abstraction-barrier------------

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y))) 
          (p2 (* (lower-bound x) (upper-bound y))) 
          (p3 (* (upper-bound x) (lower-bound y))) 
          (p4 (* (upper-bound x) (upper-bound y)))) 
        (make-interval (min p1 p2 p3 p4) 
                       (max p1 p2 p3 p4))))

(define (is-end-of-interval-zero? x)
    (if (or (= (lower-bound x) 0) 
            (= (upper-bound x) 0))
        #t
        #f))

(define (inverse-by-mul-interval x)
    (make-interval (/ 1.0 (upper-bound x)) 
                   (/ 1.0 (lower-bound x))))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y))))

; ----------abstraction-barrier------------

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

; ----------abstraction-barrier------------

(define A (make-center-percent 1000 0.1))
(define B (make-center-percent 1500 0.05))

(print-interval (div-interval A A))
(print-interval (div-interval A B))
(print-interval (par1 A B))
(print-interval (par2 A B))