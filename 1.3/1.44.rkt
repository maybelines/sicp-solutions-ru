#lang racket

(define dx 0.00001)

(define (smooth f)
    (lambda (x) (average (f (- x dx))
                   (f x)
                   (f (+ x dx)))))

(define (average x1 x2 x3)
    (/ (+ x1 x2 x3) 3))

(define (repeated f n)
    (define (iter count cur)
        (if (= count 0)
            cur
            (iter (- count 1) 
                  (lambda (x) (f (cur x))))))
    (iter n (lambda (x) x)))

(define (n-smooth f n)
    ((repeated smooth n) f))

(define (cube x)
    (* x x x))

; a kind of tests
; expected: #t, received: #t
(= ((smooth cube) 1)
   ((n-smooth cube 1) 1))

; expected: #t, received: #t
(= ((n-smooth cube 2) 1) 
   ((smooth (smooth cube)) 1))

; expected: #t, received: #t
(= ((n-smooth cube 3) 5)
   ((smooth (smooth (smooth cube))) 5))
