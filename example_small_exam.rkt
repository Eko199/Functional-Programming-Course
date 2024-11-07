#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (++ x) (+ 1 x))

(define (path1 i)
  (cond [(= i 1) (cons -7 2)]
        [(= i 2) (cons 3 -3)]
        [(= i 3) (cons 1 5)]))

; Problem 1
(define (destination f n)
  (accumulate-i (lambda (x y) (cons (+ (car x) (car y)) (+ (cdr x) (cdr y))))
              (cons 0 0) 1 n f ++))

(destination path1 3) ; -> '(-3 . 4)

; Problem 2
(define (dist x y)
  (sqrt (+ (expt (- (car x) (car y)) 2)  (expt (- (cdr x) (cdr y)) 2))))

(define (path-length f n)
  (accumulate-i + 0 1 n (lambda (x)
                        (dist (destination f (- x 1)) (destination f x))) ++))

(path-length path1 3) ; -> ~16.62

; Problem 3
(define (clamp x b)
  (max (min x b) (- b)))

(define (bound-pair x bounds)
  (cons (clamp (car x) (car bounds)) (clamp (cdr x) (cdr bounds))))

(define (destination* f n bounds)
  (accumulate-i (lambda (x y) (bound-pair (cons (+ (car x) (car y)) (+ (cdr x) (cdr y))) bounds))
              (cons 0 0) 1 n f ++))

(destination* path1 3 (cons 5 4)) ; -> '(-1 . 4) ; а не '(-3 . 3)!

; Bonus
(define (path-length* f n bounds)
  (accumulate-i + 0 1 n (lambda (x)
                        (dist (destination* f (- x 1) bounds) (destination* f x bounds))) ++))

(path-length* path1 3 (cons 5 4)) ; -> ~14.72