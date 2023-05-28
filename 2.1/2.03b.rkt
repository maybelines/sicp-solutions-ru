#lang racket
;; the second kind of implementation constructors and selectors rectangles
;; the make-rectangle procedure takes basement (implements as segment) and  
;; standalone vertice (implements as point) as input

;; all of changes this version locates in only two procedures:
;; the make-rectangle procedure and the is-correct-rectangle-vertices? 

(define (square-of-rectangle rect)
    (* (width-of-rectagle rect)
       (height-of-rectagle rect)))

(define (perimeter-of-rectangle rect)
    (* 2 (+ (width-of-rectagle rect)
            (height-of-rectagle rect))))

; ----------abstraction-barrier------------

;; this procedure has changed
(define (make-rectangle segment pnt)
    (if (is-correct-rectangle-vertices? segment pnt)
        (cons segment pnt)
        (raise-arguments-error 'make-rectangle
                               "these segment and point cannot form rectangle")))

(define (width-of-rectagle rect)
    (let ((basement (car rect)))
        (length-of-segment basement)))

(define (height-of-rectagle rect)
    (let ((point (cdr rect)) 
          (basement (car rect)))
        (point-to-segment-distance point basement)))

; ----------abstraction-barrier------------

; and this procedure has changed
(define (is-correct-rectangle-vertices? seg pnt)
    (let ((distance1 (euclid-distance (start-segment seg) (end-segment seg)))
          (distance2 (euclid-distance (end-segment seg) pnt))
          (distance3 (euclid-distance (start-segment seg) pnt)))
        (is-pythagoras-equation-fulfilled? distance1
                                           distance2
                                           distance3)))

(define (get-segment pnt1 pnt2 pnt3)
    (abstract-double-argmin euclid-distance 
                            pnt1 
                            pnt2 
                            pnt3 
                            (lambda (x y z) (make-segment x y))))


(define (get-standalone-point pnt1 pnt2 pnt3)
    (abstract-double-argmin euclid-distance 
                            pnt1 
                            pnt2 
                            pnt3 
                            (lambda (x y z) z)))

(define (length-of-segment seg)
    (euclid-distance (start-segment seg)
                     (end-segment seg)))

(define (point-to-segment-distance pnt seg)
    (min (euclid-distance pnt (start-segment seg))
         (euclid-distance pnt (end-segment seg))))

; ----------abstraction-barrier------------

(define (make-segment pnt1 pnt2)
    (cons pnt1 pnt2))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (euclid-distance pnt1 pnt2)
    (sqrt   
        (+ (square (- (x-point pnt1) 
                      (x-point pnt2))) 
           (square (- (y-point pnt1) 
                      (y-point pnt2))))))

; ----------abstraction-barrier------------

(define (make-point x y)
    (cons x y))

(define (x-point pnt)
    (car pnt))

(define (y-point pnt)
    (cdr pnt))

; ----------abstraction-barrier------------
; implements a kind of language primitives

(define (square x)
    (* x x))

(define (is-pythagoras-equation-fulfilled? x y z)
    (define tolerance 0.001)    
    (define (is-close-enough? x y)
        (< (- x y) tolerance))
    (let ((max-side (max x y z)))
        ; checks if a^2 + b^2 + c^2 is almost equal 2*c^2 
        (is-close-enough? (* 2 (square max-side))
                          (+ (square x)
                             (square y)
                             (square z)))))

(define (abstract-double-argmin f x1 x2 x3 g)
    (cond ((and (< (f x1 x2) (f x1 x3)) (< (f x1 x2) (f x2 x3))) 
           (g x1 x2 x3))
          ((and (< (f x2 x3) (f x1 x2)) (< (f x2 x3) (f x1 x3))) 
           (g x2 x3 x1))   
          ((and (< (f x1 x3) (f x1 x2)) (< (f x1 x3) (f x2 x3))) 
           (g x1 x3 x2))))

; ----------abstraction-barrier------------

; a kind of tests
(define x (make-point -1 -3))
(define y (make-point 5 1))
(define z (make-point 5 -2))
(define yz (make-segment y z))

; expected: "these segment and point cannot form rectangle"
; received: "these segment and point cannot form rectangle"
(with-handlers ([exn:fail?
                    (lambda (exn) ((error-display-handler) (exn-message exn) exn))])
    (make-rectangle yz x))

(define a (make-point -1 -3))
(define c (make-point 5 1))
(define d (make-point 5 -3))
(define cd (make-segment c d))
(define abcd (make-rectangle cd a))

; expected: 20.0, received: 20.0
(perimeter-of-rectangle abcd)
; expected: 24.0, received: 24.0
(square-of-rectangle abcd)

(define x1 (make-point 0 0))
(define x2 (make-point 1 3))
(define x3 (make-point 6 -2))
(define base (make-segment x1 x2))
(define rect (make-rectangle base x3))

; expected: 18.973665961, received: 18.973665961010276
(perimeter-of-rectangle rect)
; expected: 20, received: 20.000000000000004
(square-of-rectangle rect)