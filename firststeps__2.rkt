#lang pl

#|

This file contains basic Racket operations and explanations of how to run the program.
Note, pay attention to the spaces in the code.

By bmitay4

|#

#|
Lists
Empty list - null or ()'
Check if we reach to the end of the list - null?
cons - pair of values
|#
;; The following four values are the same
(cons 1 (cons 2 null))
(cons 1 (cons 2 '()))
(list 1 2)
'(1 2)

;; Check if the value is a list
(list? '(1 2)) ; return #t
(list? (cons 1 (cons 2 '()))) ; return #t

;; pair is refered for an object which hold two values
(pair? 1) ; return #f
(pair? (list 1 4)) ; return #t
(pair? (cons 1 (cons 2 '()))) ; return #t
(pair? '(4 6)) ; return #t
(pair? '(1)) ; return #t, because its like (cons 1 '())

;; first - return the first value in list
;; rest - return the list (second value in the pair) without the first value

;; Create a function which return the length of a given list
(: list-length : (Listof Any) -> Natural)
(define (list-length list)
  (if (null? list) 0
      (+ 1 (list-length(rest list)))))
(test (list-length '(1 2 3)) => 3)
(test (list-length (list 4 2)) => 2)
(test (list-length null) => 0)
(test (list-length '()) => 0)