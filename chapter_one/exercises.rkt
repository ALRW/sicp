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



