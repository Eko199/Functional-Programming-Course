;Problem 1
(define (sum-interval a b)
  (define (for res i)
    (if (> i b) res (for (+ res i) (+ i 1))))
  (for 0 (ceiling a)))

(sum-interval 1.2 7.7)

;Problem 2
(define (count-digit d n)
  (define (loop res n)
    (if (= n 0)
        res
        (loop (if (= (remainder n 10) d) (+ res 1) res) (quotient n 10))))
  (if (= n d 0)
      1
      (loop 0 n)))

(count-digit 2 12324)
(count-digit 0 0)

;Problem 3
(define (reverse-digits n)
  (define (loop res n)
    (if (zero? n)
        res
        (loop (+ (* res 10) (remainder n 10)) (quotient n 10))))
  (loop 0 n))

(reverse-digits 1234)
(reverse-digits 0)

;Problem 4
(define (divisible? n d)
  (zero? (remainder n d)))

(define (divisors-sum n)
  (define (loop sum i)
    (if (> i (/ n 2))
        sum
        (loop
         (if (divisible? n i) (+ sum i) sum)
         (+ i 1))))
  (loop (+ n 1) 2))

(divisors-sum 12)

;Problem 5
(define (perfect? n)
  (= (- (divisors-sum n) n) n))

(perfect? 28)
(perfect? 33550336)

;Problem 6
(define (prime? n)
  (define (loop i)
    (cond
      ((> i (sqrt n)) #t)
      ((divisible? n i) #f)
      (else (loop (+ i 1)))))
  (if (< n 2) #f (loop 2)))

(prime? 2)
(prime? 1)
(prime? 10)
(prime? 59)

;Problem 7
(define (increasing? n)
  (cond
    ((< n 10) #t)
    ((< (remainder n 10) (remainder (quotient n 10) 10)) #f)
    (else (increasing? (quotient n 10)))))

(increasing? 1234)
(increasing? 19234)

;Problem 8
(define (toBinary n)
  (define (loop res n i)
    (if (zero? n)
        res
        (loop
         (+ res (* (remainder n 2) (expt 10 i)))
         (quotient n 2)
         (+ i 1))))
  (loop 0 n 0))

(toBinary 8)
(toBinary 42)

;Problem 9
(define (toDecimal n)
  (define (loop res n i)
    (if (zero? n)
        res
        (loop
         (+ res (* (remainder n 10) (expt 2 i)))
         (quotient n 10)
         (+ i 1))))
  (loop 0 n 0))

(toDecimal 101010)
(toDecimal (toBinary 1234))

;Problem 10
(define (sq x) (* x x))

(define (^ x n)
  (cond
    ((zero? n) 1)
    ((odd? n) (* x (sq (^ x (quotient n 2)))))
    (else (sq (^ x (quotient n 2))))))

(^ 2 3)
(^ 0 6)
(^ 1 4)
(^ 10 6)