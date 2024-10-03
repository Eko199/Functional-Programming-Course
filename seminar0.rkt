;Problem 0

;(3 + 5)/2 + sqrt(4^3 - 7*2^2)
;(5 + 1/4 + (2 - (3 - (6 + 1/5)))) / 3(6 - 2)(2 - 7)
;(15 + 21 + (3 / 15) + (7 - (2 * 2))) / 16

(+ (/ (+ 3 5) 2) (sqrt (- (expt 4 3) (* 7 (expt 2 2)))))
(/ (+ 5 (/ 1 4) (- 2 (- 3 (+ 6 (/ 1 5))))) (* 3 (- 6 2) (- 2 7)))
(/ (+ 15 21 (/ 3 15) (- 7 (* 2 2))) 16)

;Problem 1
(define (f1 x y) (or (and (>= y 0) (<= (+ (* x x) (* y y)) 4)) (and (<= y 0) (>= y -2) (>= x -1) (<= x 1))))
(f1 1 1)
(f1 0 -1)
(f1 2 2)

(define (f2 x y) (and (>= x -1) (<= x 2) (<= (floor x) y) (>= (ceiling x) y)))
(f2 -0.5 -0.5)
(f2 2 2)
(f2 -0.5 0.5)
(f2 -2 -2)

;Problem 2
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 5)

;Problem 3
(define (fib n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(fib 6)

;Problem 4
(define (sum-interval a b) (if (> (ceiling a) b) 0 (+ (ceiling a) (sum-interval (+ a 1) b))))
(sum-interval 3.5 9.1)

;Problem 5
(define (count-digits n) (if (< n 10) 1 (+ 1 (count-digits (quotient n 10)))))
(count-digits 1000)

;Problem 6
(define (reverse-digits n) (if (< n 10) n (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1))) (reverse-digits (quotient n 10)))))
(reverse-digits 123)
(reverse-digits 100)

;Problem 7
(define (palindrome? n) (= n (reverse-digits n)))
(palindrome? 12321)
(palindrome? 11)
(palindrome? 0)
(palindrome? 10)