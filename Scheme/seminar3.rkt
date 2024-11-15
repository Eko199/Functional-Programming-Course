#lang racket

; Problem 1
(define (take n lst)
  (if (or (null? lst) (zero? n)) `()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (null? lst) (zero? n)) lst
      (drop (- n 1) (cdr lst))))

(take 3 '(1 2 3 4 5)) ; -> '(1 2 3)
(take 10 '(1 2 3 4 5)) ;s -> '(1 2 3 4 5)
(drop 3 '(1 2 3 4 5)) ; -> '(4 5)
(drop 10 '(1 2 3 4 5)) ; -> '()

; Problem 2
(define (all? p? lst)
  (if (null? lst) #t
      (and (p? (car lst)) (all? p? (cdr lst)))))

(define (any? p? lst)
  (if (null? lst) #f
      (or (p? (car lst)) (any? p? (cdr lst)))))

(all? even? '(1 2 3 4 5)) ; -> #f
(any? even? '(1 2 3 4 5)) ; -> #t
(any? (lambda (x) (> x 10)) '(4 2 6 3 1)) ; -> #f

; Problem 3
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))

(zip '(1 2 3 4) '(#t #f #f)) ; -> '((1 . #t) (2 . #f) (3 . #f))

; Problem 4
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (f (car lst1) (car lst2)) (zipWith f (cdr lst1) (cdr lst2)))))

(zipWith + '(1 2 3 4) '(7 10 12)) ; -> '(8 12 15)

; Problem 5
(define (sorted? lst)
  (if (or (null? lst) (null? (cdr lst))) #t
      (and (<= (car lst) (cadr lst)) (sorted? (cdr lst)))))

(sorted? '(1 1 2 2 3)) ; -> #t
(sorted? '(1 1 2 0 3)) ; -> #f

; Problem 6
;(define (new-sorted? lst)
 ; (if (null? (drop 1 lst)) #t
  ;    (all? (lambda (x) (>= x (car lst))) (drop 1 lst))))

(define (new-sorted? lst)
  (all? (lambda (x) x) (zipWith <= lst (drop 1 lst))))

(new-sorted? '(1 1 2 2 3)) ; -> #t
(new-sorted? '(1 1 2 0 3)) ; -> #f