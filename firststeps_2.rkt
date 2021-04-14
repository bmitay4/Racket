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


#|
Regular Recursion 
|#
(: factorial : Natural -> Natural)
(define (factorial value)
  (if (zero? value) 1
      (* value (factorial (- value 1)))))

(test (factorial 5) => 120) 
#|
Tail Recursion
|#
(: helper : Natural Natural -> Natural)
(define (helper n acc)
  (if (zero? n) acc
      (helper (- n 1) (* n acc))))

(: fact : Natural -> Natural)
(define (fact num)
  (helper num 1))
(test (fact 5) => 120)

;; Now we'll build fibonacci function using tail and regular recursion
(: fibo-reg : Integer -> Natural)
(define (fibo-reg value)
  (cond
    [(eq? value 0) 1]
    [(eq? value 1) 1]
    [(>= value 2) (+ (fibo-reg (- value 1 ))(fibo-reg (- value 2)))]
    [else (error 'fibo-reg "Expected for positive integer! got ~s" value)]))

(: fib-tail : Natural -> Natural)
(define (fib-tail n)
 (: fib-tail-helper : Natural Natural Natural -> Natural)
  (define (fib-tail-helper count f1 f2)
    (if (eq? count n) (+ f1 f2)
        (fib-tail-helper (+ count 1) f2 (+ f1 f2))))
(cond
  [(= n 0) 1]
  [(= n 1) 1]
  [else (fib-tail-helper 2 1 1)]))

;; Calculate sum of items(numbers) in a list, using regular recursion
(: sum : (Listof Number) -> Number)
(define (sum my_list)
  (if (null? my_list) 0
      (+ (first my_list) (sum (rest my_list)))))
(test (sum (list 1 2 3 4 5)) => 15)

;; Calculate sum of items(numbers) in a list, using tail recursion
(: sum-tail : (Listof Number) -> Number)
(define (sum-tail lis)
  (: sum-tail-help : (Listof Number) Number -> Number)
  (define (sum-tail-help lishel acc)
    (if (null? lishel) acc
        (sum-tail-help(rest lishel) (+ acc (first lishel))))) 
  (if (null? lis) 0
      (sum-tail-help lis 0)))