#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
          (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

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

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (map-stream f . streams)
  (cons-stream (apply            f (map head streams))
               (apply map-stream f (map tail streams))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
                            (filter-stream p? (tail s))))

;Problem 1
(define (myAnd x y) (and x y))
(define (++ x) (+ x 1))

(define (member x l)
  (and (not (null? l)) (or (equal? x (car l)) (member x (cdr l)))))

(define (isNPerm n f)
  (define (range x) (if (>= x n) '() (cons (f x) (range (+ x 1)))))
  (define flist (range 0))
  (accumulate myAnd #t 0 (- n 1) (lambda (x) (member x flist)) ++))

(isNPerm 3 (lambda (x) (remainder (- 3 x) 3))) ; -> True
(isNPerm 10 (lambda (x) (quotient x 2))) ;→ False
(isNPerm 10 (lambda (x) (remainder (+ x 2) 10))) ;→ True

(define (snoc l x) (append l (list x)))

(define (maxCycle n f)
  (define (cycle curr x len)
    (define fx (f x))
    (if (member fx curr)
        (cons curr len)
        (cycle (snoc curr fx) fx (++ len))))
  (car (accumulate (lambda (x y)
                (if (< (cdr x) (cdr y)) y x))
              (cons '() 0)
              0
              (- n 1)
              (lambda (x) (cycle (list x) x 1))
              ++)))

(maxCycle 3 (lambda (x) (remainder (- 3 x) 3))) ;→ [1, 2]
(maxCycle 10 (lambda (x) (remainder (+ x 2) 10))) ;→ [0, 2, 4, 6, 8]
(maxCycle 10 (lambda (x) (remainder (+ x 3) 10))) ;→ [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]

;Problem 2
(define (take-stream n s)
  (if (or (zero? n) (null? s))
      '()
      (cons (head s) (take-stream (- n 1) (tail s)))))

(define (movingAverage l n)
  (cons-stream (/ (apply + (take-stream n l)) n) (movingAverage (tail l) n)))

(define test
  (cons-stream 1076 (cons-stream 1356 (cons-stream 1918
   (cons-stream 6252 (cons-stream 6766 (cons-stream 5525 test)))))))

(take-stream 4 (movingAverage test 3))

(define (allAverages l)
  (define naturalsFrom2 (cons-stream 2 (map-stream ++ naturalsFrom2)))
  (map-stream (lambda (n) (movingAverage l n)) naturalsFrom2))

(take-stream 3 (map-stream (lambda (s) (take-stream 4 s)) (allAverages test)))
