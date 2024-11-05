#lang racket
; Problem 1
(define (accumulate op nv a b f step)
  (if (> a b) nv
      (op (f a) (accumulate op nv (step a) b f step))))

(define (accumulate-f op nv a b f step p?)
  (if (> a b) nv
      (op (if (p? a) (f a) nv) (accumulate-f op nv (step a) b f step p?))))

(define (divisible? x y)
  (= 0 (remainder x y)))

(define (id x) x)
(define (++ x) (+ x 1))

(define (divisor-sum x)
  (accumulate-f + 0 1 (- x 1) id ++ (lambda (y) (divisible? x y))))

(define (done? x)
  (= (divisor-sum x) (+ x 2)))

(divisor-sum 3)
(done? 20) ; → #t
(done? 28) ; → #f

(define (dist x y) (abs (- x y)))

(define (sum-almost-done a b)
  (define (min-dist-i x) (min (dist x a) (dist x b)))
  (define (min-dist x)
    (define (loop curr minDist)
      (if (> curr b) minDist
        (loop (++ curr) (if (and (done? curr) (< (dist curr x))) (dist curr x) minDist))))
    (loop a (min-dist-i x)))
  (define (almost-done? x)
    (< (min-dist x) (min-dist-i x)))
  (accumulate-f + 0 a b id ++ almost-done?))

(sum-almost-done 5 24) ; → 153 ; сумата на числата от 13 до 21

; Problem 2
(define (not-number? x) (not (number? x)))

(define (3rd-option op n stek)
  (if (or
       (zero? n)
       (null? stek)
       (null? (cdr stek))
       (not-number? (car stek))
       (not-number? (cadr stek)))
      stek
      (3rd-option op (- n 1) (cons (op (car stek) (cadr stek)) (cddr stek)))))

(define (run-machine lst)
  (define (run lst stek)
    (if (null? lst) stek
        (let ((op (car lst)))
        (run (cdr lst)
             (cond
               ((or (number? op) (symbol? op))
                (cons op stek))
               ((procedure? op)
                (map (lambda (x) (if (number? x) (op x) x)) stek))
               ((and (pair? op) (procedure? (car op)) (number? (cdr op)))
                (3rd-option (car op) (cdr op) stek))
               (else stek))))))
  (run lst '()))

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       ; → (6 5 4 3 a 2 x 1)
(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) ; → (45 a 2 x 1)

; Problem 3
(define (majorate-limited? lst1 lst2 n)
  (or (and (null? lst1) (zero? n))
      (and (not (null? lst1)) (not (null? lst2)) (not (zero? n)) (<= (car lst1) (car lst2)) (majorate-limited? (cdr lst1) (cdr lst2) (- n 1)))))

;(majorate-limited? '(1 2) '(2 3 4) 2)
;(majorate-limited? '(1 2) '(2 3 4) 0)

(define (new-or a b) (or a b))

(define (majorate-sub? lst1 lst2)
  (and (not (null? lst2))
       (or
        (accumulate new-or #f 1 (length lst2) (lambda (x) (majorate-limited? lst1 lst2 x)) ++)
        (majorate-sub? lst1 (cdr lst2)))))

;(majorate-sub? '(1 2) '(0 2 3 4))
;(majorate-sub? '(1 2) '(0 2 1 4))
;(majorate-sub? '(4 2 7) '(2 5 4 3 9 12))

(define (is-major? lst)
  (or (null? lst) (null? (cdr lst))
      (and (majorate-sub? (car lst) (cadr lst)) (is-major? (cdr lst)))))

(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))); → #t
(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))); → #f

(define (take lst n)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;(take `(1 2 3 4) 2)

(define (sublists lst)
  (if (null? lst) (list lst)
      (append (accumulate cons '() 1 (length lst) (lambda (x) (take lst x)) ++)
              (sublists (cdr lst)))))

;(sublists '(1 2 3))

(define (get-biggest-cdr lst)
  (cond
    ((null? lst) lst)
    ((null? (cdr lst)) (car lst))
    (else (get-biggest-cdr
           (if (<= (cdar lst) (cdadr lst))
               (cdr lst)
               (cons (car lst) (cddr lst)))))))

(define (find-longest-major lst)
  (define majors (filter is-major? (sublists lst)))
  (car (get-biggest-cdr (map cons majors (map length majors)))))

(find-longest-major '((10 10) (1 3) (4 2 7) (2 5 4 3 9 12) (10 10)))