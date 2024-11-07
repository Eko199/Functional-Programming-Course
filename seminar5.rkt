#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

; Problem 3

(define (height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (height (left-tree t)) (height (right-tree t))))))

(height empty-tree) ; -> 0
(height test-tree) ; -> 4

; Problem 4
(define (get-level n t)
  (cond
    ((or (zero? n) (empty-tree? t)) '())
    ((= n 1) (list (root-tree t)))
    (else (append (get-level (- n 1) (left-tree t)) (get-level (- n 1) (right-tree t))))))

(get-level 2 test-tree)

; Problem 5

(define (find-longest-path t)
  (if (empty-tree? t)
      '()
      (cons (root-tree t)
            (let
                ((left-path (find-longest-path (left-tree t)))
                 (right-path (find-longest-path (right-tree t))))
              (if (<= (length right-path) (length left-path))
                  left-path
                  right-path)))))

(find-longest-path test-tree) ; -> '(10 3 4 1) ; или '(10 3 4 2), няма значение

; Problem 6

(define (tree-map f t)
  (if (empty-tree? t)
      t
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

(find-longest-path (tree-map (lambda (x) (+ x 1)) test-tree))

; Problem 7

(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t)) (list (root-tree t)) (tree->list (right-tree t)))))

(tree->list test-tree)

; Problem 8

(define (bst-insert val t)
  (cond
    ((empty-tree? t) (make-tree val empty-tree empty-tree))
    ((< val (root-tree t)) (make-tree (root-tree t) (bst-insert val (left-tree t)) (right-tree t)))
    (else (make-tree (root-tree t) (left-tree t) (bst-insert val (right-tree t))))))

(tree->list (bst-insert 4 (bst-insert 3 (bst-insert 2 (bst-insert 2 (bst-insert 1 empty-tree))))))

; Problem 9

(define (list-to-bst lst)
  (if (null? lst)
      empty-tree
      (bst-insert (car lst) (list-to-bst (cdr lst)))))

(define (three-sort lst)
  (tree->list (list-to-bst lst)))

(three-sort '(6 3 22 9 9 0 3 21))

; Problem 10

(define (tree-apply f t)
  (apply f (tree->list t)))

(define (valid-bst? t)
  (or (empty-tree? t)
      (and (valid-bst? (left-tree t))
           (valid-bst? (right-tree t))
           (or (empty-tree? (left-tree t)) (<= (tree-apply max (left-tree t)) (root-tree t)))
           (or (empty-tree? (right-tree t)) (<= (root-tree t) (tree-apply min (right-tree t)))))))

(valid-bst? (make-tree 1
                       empty-tree
                       (make-leaf 3)))

(valid-bst? (make-tree 2
                       (make-tree 1
                                  empty-tree
                                  (make-leaf 3))
                       empty-tree))

; Problem 11
(define (empty-or-leaf? t)
  (or (empty-tree? t) (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))))

(define (prune t)
  (if (empty-or-leaf? t)
      '()
      (make-tree (root-tree t) (prune (left-tree t)) (prune (right-tree t)))))

(prune test-tree)

; Problem 12

(define (bloom t)
  (cond
    ((empty-tree? t) t)
    ((empty-or-leaf? t) (make-tree (root-tree t) (make-leaf (root-tree t)) (make-leaf (root-tree t))))
    (else (make-tree (root-tree t) (bloom (left-tree t)) (bloom (right-tree t))))))

(bloom test-tree)

; Problem 13
(define (avg t)
  (if (empty-or-leaf? t)
      t
      (make-tree (/ (+ (tree-apply max t) (tree-apply min t)) 2)
                 (avg (left-tree t))
                 (avg (right-tree t)))))

(avg test-tree)

; Problem 14
(define (rev-to-decimal bin)
  (if (null? bin)
      0
      (+ (car bin) (* 2 (rev-to-decimal (cdr bin))))))

(define (same-as-code t)
  (define (loop sub-t code)
    (if (empty-tree? sub-t)
        '()
        (append
         (if (= (root-tree sub-t) (rev-to-decimal code))
             (list (root-tree sub-t))
             '())
         (loop (left-tree sub-t) (cons 0 code))
         (loop (right-tree sub-t) (cons 1 code)))))
  (loop t '(1)))

(same-as-code test-tree) ; -> '(3)