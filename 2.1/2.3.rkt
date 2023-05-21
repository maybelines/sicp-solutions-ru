#lang racket

(define (square-of-rectangle rec)
    (define (compute-square x y)
        (* x y))
    (compute-square (length-of-segment (first-side rec)) 
                    (length-of-segment (second-side rec))))

(define (perimeter-of-rectangle rec)
    (define (compute-perimeter x y)
        (* 2 (+ x y)))
    (compute-perimeter (length-of-segment (first-side rec)) 
                       (length-of-segment (second-side rec))))

; ----------the third level of abstraction----------
(define (are-these-correct-sides? seg1 seg2)
    (define a (start-segment seg1))
    (define b (end-segment seg1))
    (define c (start-segment seg2))
    (define d (end-segment seg2))
    (define (close-enough? x y) 
        (if (< (- x y) 0.01)
            #t
            #f))
    (define (square x)
        (* x x))
    (define (pifagoras-check-for-points x y z)
        (close-enough? (square (length-of-segment (make-segment x z)))
           (+ (square (length-of-segment (make-segment x y)))
              (square (length-of-segment (make-segment y z))))))
    (cond ((is-equal? a c)
           (pifagoras-check-for-points b a d))
          ((is-equal? a d) 
           (pifagoras-check-for-points b a c))
          ((is-equal? b c) 
           (pifagoras-check-for-points a b d))
          ((is-equal? b d) 
           (pifagoras-check-for-points a b c))
          (else #f)))

; using the "segment" entity to create rectangles
; means that seg1 and seg2 are perpendicular segments
(define (make-rectangle seg1 seg2)
    (if (are-these-correct-sides? seg1 seg2)
        (cons seg1 seg2)
        #f))

(define (first-side rec)
    (car rec))

(define (second-side rec)
    (cdr rec))

; ----------something in the middle level of abstraction----------
(define (length-of-segment seg)
    (define (square x)
        (* x x))
    (define (euclidian-distance x1 y1 x2 y2)
        (sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))
    (euclidian-distance (x-point (start-segment seg)) 
                        (y-point (start-segment seg))
                        (x-point (end-segment seg))
                        (y-point (end-segment seg))))

; ----------the second level of abstraction----------
; using the "point" entity to create segments
;(define (create-segment point1 point2)
;    (if (> (y-point point1) (y-point point2))
;        (cons point2 point1)
;        (cons point1 point2)))

(define (make-segment point1 point2)
    (if (and (= (x-point point1) (x-point point2)) 
             (= (y-point point1) (y-point point2)))
        #f
        (cons point1 point2)))

(define (start-segment x)
    (car x))

(define (end-segment x)
    (cdr x))

(define (print-point p) 
    (display "(") 
    (display (x-point p)) 
    (display ", ") 
    (display (y-point p)) 
    (display ")")
    (newline))

(define (shows-text-not-a-point)
    (display "not a point")
    (newline))

(define (print-point-safely p) 
   (if (pair? p)
       (print-point p)
       (shows-text-not-a-point)))

(define (is-equal? point1 point2)
    (and (= (x-point point1) (x-point point2))
         (= (y-point point1) (y-point point2)))) 

; ----------the first level of abstraction----------
; using integers to create "points"
(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

; a kind of tests
;(−3;−1), (−3;3), (5;−1)
(define rect (make-rectangle 
                (make-segment 
                    (make-point -3 -1)
                    (make-point -3 3))
                (make-segment
                    (make-point -3 -1)
                    (make-point 5 -1))))

; expected: 24, received: 24
(perimeter-of-rectangle rect)
; expected: 32, received: 32
(square-of-rectangle rect)
    
