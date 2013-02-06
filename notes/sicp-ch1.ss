
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (avg x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.00001))

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.00001))

(sqrt-iter 1.0 10000)
(sqrt-iter 1.0 0.0000000001)
(sqrt-iter 1.0 0.0000000000000001)
(sqrt-iter 1.0 100000000)

(define (3sqrt guess x)
  (if (good-enough? guess x)
      guess
      (3sqrt (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.00001))

(3sqrt 1.0 0.0000000000001)
(3sqrt 1.0 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter result counter)
    (if (> counter n)
        result
        (fact-iter (* result counter) (+ counter 1))))
  (fact-iter 1 1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (f2 n)
  (f2-iter 2 1 0 n))

(define (f2-iter a b c count)
  (if (= count 0)
      c
      (f2-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(f2 0)
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pascal-triangle n m)
  (if (or (= m 1) (= m n))
      1
      (+ (pascal-triangle (- n 1) (- m 1))
         (pascal-triangle (- n 1) m))))

(pascal-triangle 5 1)
(pascal-triangle 5 5)
(pascal-triangle 5 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (p x count)
  (print count)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle count)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0) count) (+ count 1))))


(define (p x)
  ;(print x)
  ;(print "   ")
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(sine 12.15)

(sine 123.340)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
;  (print a)
;  (print "-")
;  (print b)
;  (print " ")
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))


(fast-expt 2 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my* a b)
  (cond ((= b 0) 0)
        ((even? b) (my* (double a) (halve b)))
        (else (+ a (my* a (- b 1))))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(my* 3 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my* a b)
  (my*-iter 0 a b))

(define (my*-iter s a b)
  (cond ((= b 0) s)
        ((even? b) (my*-iter s (double a) (halve b)))
        (else (my*-iter (+ s a) a (- b 1)))))

(my* 3 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test)
  (cond ((> (* test test) n) n)
        ((divides? n test) test)
        (else (find-divisor n (+ 1 test)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 19999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (cond ((= count 0) (newline))
        ((timed-prime-test start) (search-for-primes (next-odd start) (- count 1)))
        (else (search-for-primes (next-odd start) count))))

(define (next-odd n)
  (if (= (remainder n 2) 0)
      (+ 1 n)
      (+ 2 n)))


(search-for-primes 10000000000000 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test)
  (cond ((> (* test test) n) n)
        ((divides? n test) test)
        (else (find-divisor n (next test)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-time (- (current-inexact-milliseconds) start-time))
      #f))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)
