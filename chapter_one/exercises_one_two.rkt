(define (square x) (* x x))
; 1.9

;This process is recursive e.g. for (+ 1 2)
; (inc (+ 0 2))
; (inc 2)
; 3
(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

; This is an iterative process .eg. for (+ 1 2)
; (+ 0 3)
; 3
(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

; 1.10 Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ;1024
(A 2 4) ; 65536
(A 3 3) ; 65536

;(define (f n) (A 0 n)) => 2n
;(define (g n) (A 1 n)) => 2^n
;(define (h n) (A 2 n)) => 2^h(n-1)

; 1.11
; recursive
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;iterative
(define (f2 n)
  (f2-iter 2 1 0 n))

(define (f2-iter a b c cnt)
  (if (= cnt 0)
    c
    (f2-iter (+ a (* 2 b) (* 3 c))
             a
             b
             (- cnt 1))))

; 1.12

; where i is not zero indexed
(define (pascals row index)
  (if (or (= row 1)
          (= row 2)
          (= index 1)
          (= 0 (- row index)))
    1
    (+ (pascals (dec row) (dec index))
       (pascals (dec row) index))))

;1.13
;I'll come back to this when I'm better at Math :face_palm:

;1.14
; Θ(n) that is space and steps both grow linearly with n

;1.15
(define (cube x) (* x x x))
(define (p x ) (- (* 3 x) (* 4 (cube x))))
(define (sine angle step)
  (display step) (display ": ") (display angle) (newline)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0) (+ step 1)))))

(sine 12.15 1) ; 6
;𝚯(log(a))

;1.16
;𝚯(log(n)) steps and 𝚯(1) space
;requires (square) defined earlier

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter base n product)
  (cond ((= n 0) product)
        ((even? n) (expt-iter (square base) (/ n 2) product))
        (else (expt-iter base (- n 1) (* base product)))))

;1.17 
;𝚯(log(b)) steps and 𝚯(log(b)) space
(define (double a)
  (+ a a))

(define (halve a)
  (if (even? a)
    (/ a 2)
    0))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-multi a (halve b))))
        (else (+ a (fast-multi a (- b 1))))))

;1.18
;𝚯(log(b)) steps and 𝚯(1) space
(define (fast-mult a b)
  (mult-iter a b 0))

(define (mult-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mult-iter (double a) (halve b) p))
        (else (mult-iter a (- b 1) (+ p a)))))

;1.19
; !learn maths
; p' = p^2 + q^2
; q' = 2qp + q^2
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 q p) (square q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))

;1.20
; 18 remainder operations are performed for normal order: exponential growth
; 4 in the applicative order: logarithmic growth

;1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

;1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n prime-test)
  (start-prime-test n prime-test (runtime)))

(define (start-prime-test n prime-test start-time)
  (if (prime-test n)
      (report-prime n (- (runtime)
                       start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end prime-test)
  (timed-prime-test start prime-test)
  (cond ((= 0 (remainder start 2))
         (search-for-primes (+ start 1) end prime-test))
        ((> start end) (display "Finished"))
        (else (search-for-primes (+ start 2) end prime-test))))

(search-for-primes 10000 10010 prime?); 5ms
(search-for-primes 100000 100010 prime?); 15ms
(search-for-primes 1000000 1000100 prime?); 45ms
(search-for-primes 10000000 10000100 prime?); 144ms
; All of which seems to support the sqrt(10) growth in # of steps in the process

;1.23
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(search-for-primes 10000 10010 prime?); 4ms
(search-for-primes 100000 100010 prime?); 12ms
(search-for-primes 1000000 1000100 prime?); 35ms
(search-for-primes 10000000 10000100 prime?); 112ms
;this isn't quite a speedup of 200% because for each test we've added an extra
;function call and additional if statement to resolve

;1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (fp-5? n)
  (fast-prime? n 5))

(search-for-primes 10000 10010 fp-5?); 12ms
(search-for-primes 100000 100010 fp-5?); 11ms
(search-for-primes 1000000 1000100 fp-5?); 13ms
(search-for-primes 10000000 10000100 fp-5?); 16ms
