#lang racket

;; midpoint-segment returns point in the middle of the segment
; the question of what level of abstraction this procedure occupies remains open
; by one side its the third level of abstraction because its using "segment" entity
; by another its somewhere else because its actively using "point" entity (at least returns "point")
(define (midpoint-segment seg)
    (define (average x y)
        (/ (+ x y) 2.0))
    (if (false? seg)
        #f
        (make-point (average (x-point (start-segment seg))
                            (x-point (end-segment seg)))
                    (average (y-point (start-segment seg))
                            (y-point (end-segment seg))))))

; ----------the second level of abstraction----------
; using the "point" entity to create segments
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
   (if (false? p)
       (shows-text-not-a-point)
       (print-point p)))

; ----------the first level of abstraction----------
; using integers to create "points"
(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

; a kind of tests
; expected: (0, 2.5), received: (0, 2.5)
(print-point-safely
 (midpoint-segment 
  (make-segment (make-point 0 0)
                (make-point 0 5))))

; expected: (6.0, 4.0), received: (6.0, 4.0)
(print-point-safely
 (midpoint-segment 
  (make-segment (make-point 2 3)
                (make-point 10 5))))

; expected: (6.0, 4.0), received: (6.0, 4.0)
(print-point-safely
 (midpoint-segment 
  (make-segment (make-point 10 5)
                (make-point 2 3))))

; expected: "not a point", received: "not a point"
(print-point-safely
 (midpoint-segment 
  (make-segment (make-point 0 0)
                (make-point 0 0))))