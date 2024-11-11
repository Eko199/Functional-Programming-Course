;; често използвани функции
(define (id x) x)
(define (1+ x) (+ x 1))
(define (compose f g) (lambda (x) (f (g x))))

;; натрупване от по-висок ред
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; стандартни генератори на списъци
(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

;; фунции от по-висок ред за списъци
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

; Problem 1
(define (num-to-list-rev n)
  (if (<= n 9)
      (list n)
      (cons (remainder n 10) (num-to-list-rev (quotient n 10)))))

(define (rev-list-to-num lst)
  (if (null? lst)
      0
      (+ (car lst) (* 10 (rev-list-to-num (cdr lst))))))

; Multiply all elements by position
(define (multiply-pos lst curr)
    (if (null? lst)
        lst
        (cons (* (car lst) curr) (multiply-pos (cdr lst) (1+ curr)))))

(define (multiply-by-position n)
  (rev-list-to-num (foldr append '()
                          (map (lambda (x) (num-to-list-rev x))
                               (multiply-pos (num-to-list-rev n) 1)))))

(multiply-by-position 123) ; 343
(multiply-by-position 507) ; 1507
(multiply-by-position 987) ; 27167
(multiply-by-position 1249) ; 4689
(multiply-by-position 9000) ; 36000
(multiply-by-position 0) ; 0

; Problem 2
(define (square x) (* x x))

(define (max-res f l)
  (apply max (map f l)))

(define (max-row-res fl l)
  (apply max (map (lambda (g) (max-res g l)) fl)))

(define (minmax fm l)
  (apply min (map (lambda (frow) (max-row-res frow l)) fm)))

(minmax (list (list square exp) (list cos 1+)) '(-1 0 1)) ; 2.0
(minmax (list (list id exp) (list cos square)) '(-1 0 1)) ; 1.0
(minmax (list (list id 1+) (list cos square)) '(5 0 2)) ; 6.0
(minmax (list (list id exp) (list (lambda (x) (1+ (1+ x))) (lambda (x) (1+ (1+ x))))) '(1 0 1)) ; e

; Problem 3
(define root car)
(define left-tree cadr)
(define right-tree caddr)

(define (even-sum l)
  (foldr + 0 (map (lambda (x) (if (even? x) x 0)) l)))

(define (better-first-than s1 l1 s2 l2)
  (or (> s1 s2) (and (= s1 s2) (>= l1 l2))))

(define (best-path l1 l2 l3)
  (define sum1 (even-sum l1))
  (define len1 (length l1))
  (define sum2 (even-sum l2))
  (define len2 (length l2))
  (define sum3 (even-sum l3))
  (define len3 (length l3))
  (cond
    ((and (better-first-than sum1 len1 sum2 len2) (better-first-than sum1 len1 sum3 len3)) l1)
    ((better-first-than sum2 len2 sum3 len3) l2)
    (else l3)))

(define (maximum-even-nodes-sum bt)
  (if (null? bt)
      '()
      (cons (root bt) (best-path
                       '()
                       (maximum-even-nodes-sum (left-tree bt))
                       (maximum-even-nodes-sum (right-tree bt))))))

(maximum-even-nodes-sum '()) ; ()
(maximum-even-nodes-sum '(4 () ())) ; (4)
(maximum-even-nodes-sum '(1 (4 () ()) (6 (3 () ()) (-4 () ())))) ; (1 6 3)
(maximum-even-nodes-sum '(1 (4 () ()) (6 (-2 () ()) (-4 () ())))) ; (1 6)
