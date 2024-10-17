; Problem 0
(define (const c) (lambda (x) c))

(define forever-21 (const 21))
(forever-21 5) ; -> 21
(forever-21 10) ; -> 21
((const 21) 10) ; -> 21

(define (flip f) (lambda (x y) (f y x)))

(define f (flip -))
(f 4 10) ; -> 6
((flip f) 4 10) ; -> -6

(define (compose f g) (lambda (x) (f (g x))))
(define f (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ; ((x^2)+1)
(f 3) ; -> 10

; Problem 1
(define (id x) x)

(define (repeat n f)
  (if (zero? n) id (compose f (repeat (- n 1) f))))

(define f (repeat 5 (lambda (x) (+ x 1))))
(f 10) ; -> 15
((repeat 0 (lambda (x) (+ x 1))) 10) ; -> ?

; Problem 2
(define (twist k f g)
  (if (zero? k)
      id
      (compose f (compose g (twist (- k 2) f g)))))

(define (++ x) (+ x 1))
(define (sq x) (* x x))
(define foo (twist 4 ++ sq))
(define bar (twist 2 ++ sq))
(foo 2) ; -> 26
(bar 2) ; -> 5

; Problem 3
(define (accumulate op nv a b term next)
  (if (<= a b)
      (op (term a) (accumulate op nv (next a) b term next))
      nv))

(define (!! n) (accumulate * 1 (if (odd? n) 1 2) n id (lambda (x) (+ x 2))))
(!! 5) ; -> 15    ; =1*3*5
(!! 10) ; -> 3840 ; =2*4*6*8*10

; Problem 4
(define (! n) (accumulate * 1 1 n id ++))

(define (nchk n k) (/ (! n) (* (! k) (! (- n k)))))

(nchk 5 3) ; -> 10
(nchk 15 0) ; -> 1

(define (nchkacc n k) (accumulate * 1 1 k (lambda (x) (/ (+ (- n x) 1) x)) ++))

(nchkacc 5 3) ; -> 10
(nchkacc 15 0) ; -> 1

; Problem 5
(define (2^acc n) (accumulate * 1 1 n (const 2) ++))

(2^acc 3) ; -> 8
(2^acc 10) ; -> 1024

(define (2^ n) (accumulate + 0 0 n (lambda (x) (nchk n x)) ++))

(2^ 3) ; -> 8
(2^ 10) ; -> 1024

; Problem 6
(define (my-and x y) (and x y))
(define (my-or x y) (or x y))

(define (all? p? a b) (accumulate my-and #t a b p? ++))
(define (any? p? a b) (accumulate my-or #f a b p? ++))

(all? even? 0 10)
(all? number? 0 10)
(any? even? 0 10)
(any? string? 0 10)

; Problem 7
(define (sum-powers k n) (accumulate + 0 1 n id (lambda (x) (* x k))))

(sum-powers 2 10) ; -> 15 ; 1+2+4+8

; Problem 8
(define (divisible? n k)
  (zero? (remainder n k)))

(define (divisors-sum n)
  (+ n (accumulate + 0 2 (quotient n 2) (lambda (x) (if (divisible? n x) x 0)) ++) 1))

(divisors-sum 12) ; -> 28 ;1+2+3+4+6+12=28

; Problem 9
(define (count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) ++))

(count even? 1 10) ; -> 5
(count odd? 1 9) ; -> 5

; Problem 10
(define (prime? n)
  (not (or (= n 1) (any? (lambda (x) (divisible? n x)) 2 (- n 1)))))

(prime? 1) ; -> #f ;(!)
(prime? 2) ; -> #t
(prime? 101) ; -> #t

; Problem 11
(define (repeat-acc n f)
  (accumulate compose id 1 n (const f) ++))

(define f (repeat-acc 5 (lambda (x) (+ x 1))))
(f 10) ; -> 15
((repeat-acc 0 (lambda (x) (+ x 1))) 10) ; -> ?

; Problem 12
(define (twist k f g)
  (accumulate compose id 1 (/ k 2) (const (compose f g)) ++))

(define foo (twist 4 ++ sq))
(define bar (twist 2 ++ sq))
(foo 2) ; -> 26
(bar 2) ; -> 5

; Problem 13
(define (max-arg f x y)
  (if (> (f x) (f y)) x y))

(define (argmax f a b)
  (accumulate (lambda (x y) (if (> (f x) (f y)) x y)) a a b id ++)) ; a, защото интервалът е непразен и няма значение

(argmax (lambda (x) (remainder x 7)) 40 45) ; -> 41 ; а не (f 41), т.е. 6

; Problem 14
(define (digits-count n) (if (zero? n) 1 (accumulate + 0 1 n (const 1) (lambda (x) (* x 10)))))

(digits-count 1000) ; -> 4
(digits-count 1) ; -> 1
(digits-count 0) ; -> 1

; Problem *
(define (d/dx f) (lambda (x) (/ (- (f (+ x 1/10000)) (f x)) 1/10000)))
(((repeat 2 d/dx) (lambda (x) (expt x 3))) 2)