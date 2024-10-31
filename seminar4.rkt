#lang racket
; Problem 1
(define (union x lst)
  (cond
    ((null? lst) (list x))
    ((equal? x (car lst)) lst)
    (else (cons (car lst) (union x (cdr lst))))))

(define (uniques lst)
  (foldl union '() lst))

(uniques '(1 2 2 "iei" 1 3 "iei" 'oops)) ; -> '(1 2 "iei" 3 'oops) ; подредбата в резултата няма значение

; Problem 2
(define (insert val lst)
  (cond
    ((null? lst) (list val))
    ((<= val (car lst)) (cons val lst))
    (else (cons (car lst) (insert val (cdr lst))))))

(insert 5 '(1 4 10)) ; -> '(1 4 5 10)
(insert 12 '(1 4 10)) ; -> '(1 4 10 12)

; Problem 3
(define (insertion-sort lst)
  (foldr insert '() lst))

(insertion-sort '(4 3 6 2 1 8 10)) ; -> '(1 2 3 4 6 8 10)

; Problem 4
(define (interval-length i)
  (- (cdr i) (car i)))


(define (arg-max f nv lst)
  (cond
    ((null? lst) nv)
    ((null? (cdr lst)) (car lst))
    (else (let ((max (arg-max f nv (cdr lst))))
      (if (< (f (car lst)) (f max))
          max
          (car lst))))))

(define (longest-interval il)
  (arg-max interval-length '() il))

(define (insert-i i lst)
  (cond
    ((null? lst) (list i))
    ((<= (car i) (car (car lst))) (cons i lst))
    (else (cons (car lst) (insert-i i (cdr lst))))))

(define (insertion-sort-i il)
  (foldr insert-i '() il))

(define (longest-interval-subsets il)
  (define longest (longest-interval il))
    (insertion-sort-i (filter
                       (lambda (i) (and (>= (car i) (car longest)) (<= (cdr i) (cdr longest))))
                       il)))

(longest-interval-subsets
  '((24 . 25) (90 . 110) (0 . 100) (10 . 109) (1 . 3) (-4 . 2)))
; -> ((0 . 100) (1 . 3) (24 . 25))

; Problem 5
(define (add-group f lst x)
  (define fx (f x))
  (cond
    ((null? lst) (list (list fx (list x))))
    ((equal? (car (car lst)) fx) (cons (list (car (car lst)) (append (cadr (car lst)) (list x))) (cdr lst)))
    (else (cons (car lst) (add-group f (cdr lst) x)))))

;(add-group even? '() 1)
;(add-group even? (add-group even? '() 1) 2)
;(add-group even? (add-group even? '() 1) 3)

(define (group-by f lst)
  (foldl (lambda (x l) (add-group f l x)) '() lst))

(group-by even? '(1 2 3 4 5)) ; -> ((#f (1 3 5))
                                 ; (#t (2 4))) ; подредбата няма значение
(group-by length '((1 2 3) (4) (5 6 7))) ; -> '((1 ((4)))
                                           ;   (3 ((1 2 3) (5 6 7))))

; Problem 6
(define (compose f g) (lambda (x) (f (g x))))

(define (compose* . fns)
  (foldr compose (lambda (x) x) fns))

(define (sq x) (* x x))
(define (1+ x) (+ x 1))
(define f (compose* sq 1+ (lambda (x) (* x 2)) 1+))
; това е екв. на:  (sq
                      ;(1+
                      ;   ((lambda (x) (* x 2))
                      ;                        (1+ x))))
(f 5) ; -> 169

; Problem 7
;(define (zipWith f lst1 lst2)
;  (if (or (null? lst1) (null? lst2)) '()
;      (cons (f (car lst1) (car lst2)) (zipWith f (cdr lst1) (cdr lst2)))))

;(define (foldl-first f lst)
  ;(if (null? lst) lst (foldl f (car lst) (cdr lst))))
(define (any? p? lst)
  (if (null? lst) #f
      (or (p? (car lst)) (any? p? (cdr lst)))))

(define (zipWith* f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      `()
      (cons  (apply f (map car lsts)) (apply zipWith* f (map cdr lsts)))))

(zipWith* list '(1 2 3) '(a b) '(7 8 9 10)) ; -> '((1 a 7) (2 b 8))
(zipWith* + '(1 2 3)) ; -> '(1 2 3) ; все пак броят списъци е произволен
(zipWith* void) ; -> '() ; why does this make my head hurt