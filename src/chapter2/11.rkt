#lang racket

(define (mul-interval a b)
    (let ((a1 (lower-bound a)) 
          (a2 (upper-bound a)) 
          (b1 (lower-bound b)) 
          (b2 (upper-bound b)))
        (cond ((and (= (+ (sign a1) 
                          (sign b1)) 
                       -2) 
                    (= (+ (sign a2) 
                          (sign b2)) 
                       0))
              ; the case when {sign(a1, a2), sign(b1, b2)} = {(-1, -1), (-1, 1)} 
               (make-interval (* a1 b2) (* a1 b1)))
              ((and (= (+ (sign a1) 
                          (sign b1)) 
                       0) 
                    (= (+ (sign a2) 
                          (sign b2)) 
                       0))
              ; the case when {sign(a1, a2), sign(b1, b2)} = {(-1, -1), (1, 1)}
               (make-interval (* a1 b2) (* a2 b1)))
              ((and (= (sign a1) 
                       (sign b1)
                       (sign a2) 
                       (sign b2)))
              ; the case when (sign a1) = (sign a2) = (sign b1) = (sign b2)
               (make-interval (* a1 b1) (* a2 b2)))
              ((and (= (sign a1) 1) 
                    (= (sign a2) 1)  
                    (= (sign b1) -1) 
                    (= (sign b2) 1))
              ; contain only one case, unfortunately it is not contained in the inertia group
               (make-interval (* a2 b1) (* a2 b2)))
              ((and (= (sign a1) -1) 
                    (= (sign a2) 1)  
                    (= (sign b1) 1) 
                    (= (sign b2) 1))
              ; contain only one case, unfortunately it is not contained in the inertia group
               (make-interval (* a1 b2) (* a2 b2)))
              ((and (= (sign a1) -1) 
                    (= (sign a2) 1)  
                    (= (sign b1) -1) 
                    (= (sign b2) 1))
              ; the worst case when requires more than two multiplications, actually four
               (make-interval (min (* a1 b2) 
                                   (* a2 b1)) 
                              (max (* a1 b1) 
                                   (* a2 b2)))))))

(define (print-interval x) 
    (newline) 
    (display "(") 
    (display (lower-bound x)) 
    (display ", ") 
    (display (upper-bound x)) 
    (display ")"))

; ----------abstraction-barrier------------

(define (make-interval a b) 
    (if (< a b)
        (cons a b)
        (cons b a)))

(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

(define (sign x)
    (if (< x 0) 
        -1
        1))

; -----------------------------------------
; a kind of tests
(define a (make-interval -1000 1))
(define b (make-interval -1000 2))
(define a1 (make-interval -10 10))
(define b1 (make-interval -10 100))
(define a2 (make-interval -1 1))
(define b2 (make-interval -2 1))

;; (-1000, 1) * (-1000, 2) = (x1 * y2, x1 * y1) = (-2000, 1000000)
;; (-10, 10) * (-10, 100) =  (x1 * y2, x2 * y2) = (-1000, 1000)
;; (-1, 1) * (-2, 1) =       (x2 * y1, x1 * y1) = (-2, 2)
(print-interval (mul-interval a b))
(print-interval (mul-interval a1 b1))
(print-interval (mul-interval a2 b2))

(define x (make-interval -10 -2))
(define y (make-interval -100 -3))
(define z (make-interval 2 10))
(define p (make-interval 1 2))
(define q (make-interval 2 3))

;; (-10, -2) * (-100, -3) = (x2 * y2, x1 * y1) = (6, 1000)
;; (-10, -2) * (2, 10) =    (x1 * y2, x2 * y1) = (-100, -4)
;; (1, 2) * (2, 3) =        (x1 * y1, x2 * y2) = (2, 6)
(print-interval (mul-interval x y))
(print-interval (mul-interval x z))
(print-interval (mul-interval p q))