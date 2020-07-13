; Chapter 1 exercises

; 1.1
10 ; 10

(+ 5 3 4) ; 12

(- 9 1) ; 8

(/ 6 2) ; 3

(+ (* 2 4) (- 4 6)) ; 6

(define a 3)  ;

(define b (+ a 1)) ;

(+ a b (* a b)) ; 19

(= a b) ; #f

(if (and (> b a) (< b (* a b)))
  b
  a) ; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16

(+ 2 (if (> b a) b a)) ; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

; 1.2
(/ (+ 5 4 (- 2 3(+ 6 (/ 4 5))))
   (* 3 (- 6 2) (- 2 7))) ; -1/50

;1.3
(define (sos x y) (+ (* x x) (* y y)))
(define (sgs x y z)
  (cond ((and (or (> x z) (> x y)) (> y z)) (sos x y))
        ((and (> x y) (> z y)) (sos x z))
        (else (sos y z))))

(sgs 2 4 1) ; 20
(sgs 5 8 2) ; 89

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 -1) ; 2

; 1.5

; return 0 in both cases


; 1.6
; Infinite recursion due to evaluation of sqrt-iter as one of the arguments due
; to Applicative evaluation rather than normal expand and reduce evaluation

; 1.7

; with very small numbers the problem is that the number may be smaller than the test precision?
; For large numbers the number of iterations may effectively leave you in an infinite loop?

(define (average x y)
  (/ (+ x y ) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
  (exact->inexact (sqrt-iter 1 0 x)))

(sqrt 4)

; the new method works well for small numbers but not for large

; 1.8

(define (square x) (* x x))

(define (improve-qb guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (qbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
    guess
    (qbrt-iter (improve-qb guess x) guess x)))

(define (qbrt x)
  (exact->inexact (qbrt-iter 1 0 x)))

(qbrt 8)
