#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (car lst)
                    (take (- n 1) (cdr lst)))]))

(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (cdr lst))]))

(define (all? p? lst)
  (or (null? lst)
      (and (p? (car lst))
           (all? p? (cdr lst)))))

(define (complement p?)
  (lambda (x) (not (p? x))))
(define (any? p? lst)
  (not (all? (complement p?) lst)))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (++ x) (+ x 1))

; Problem 1
(define (average lst n)
  (/ (foldr + 0 (take n lst)) n))

;(average '(3 2 7 0 -1 1) 3) ; -> 4

(define (moving-average lst n)
  (if (< (length lst) n)
      '()
      (cons (average lst n) (moving-average (cdr lst) n))))

(moving-average '(3 2 7 0 -1 1) 3) ; -> '(4 3 2 0) ; точно в този ред
(moving-average '(3 2 7 0 -1) 2)

; Problem 1 - not obvious recursion
(define (moving-average-1 lst n)
  (define len (length lst))
  (if (< len n)
      '()
      (accumulate append '() 0 (- len n) (lambda (x) (list (average (take n (drop x lst)) n))) ++)))

(moving-average-1 '(3 2 7 0 -1 1) 3) ; -> '(4 3 2 0) ; точно в този ред
(moving-average-1 '(3 2 7 0 -1) 2)

; Problem 2
(define (sublists lst)
  (if (null? lst)
      (list lst)
      (append (accumulate append '() 1 (length lst) (lambda (x) (list (take x lst))) ++) (sublists (cdr lst)))))

(define (sublist? needle haystack)
  (any? (lambda (l) (equal? needle l)) (sublists haystack)))

(sublist? '(2 4) '(1 2 3 4)) ; -> #f
(sublist? '(2 4) '(1 2 4)) ; -> #t
(sublist? '() '(1)) ; -> #t

; Problem 3

(define (majors? a b)
  (and
   (= (length a) (length b))
   (all? (lambda (x) (<= (car x) (cdr x))) (zip a b))))

(majors? '(1 2 3) '(4 3 7)) ; -> #t
(majors? '(1 2 3) '(1 2 3)) ; -> #t
(majors? '(1 2 3) '(5 8 2)) ; -> #f
(majors? '(1 2 3) '(5 8 2 3)) ; -> #f
(majors? '() '()) ; -> #t

; Problem 4

(define (any-majors? a b)
  (any? (lambda (x) (majors? a x)) (sublists b)))

(any-majors? '(1 2 3) '(0 4 3 7 0)) ; -> #t
(any-majors? '(1 2 3) '(0 4 3 7 8 8 8)) ; -> #t
(any-majors? '(1 2 3) '(0 5 8 2)) ; -> #f
(any-majors? '(1 2 3) '(0 0)) ; -> #f
(any-majors? '() '(1 2 3)) ; -> #t