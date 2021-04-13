#lang pl

#|

This file contains basic Racket operations and explanations of how to run the program.
Note, pay attention to the spaces in the code.

By bmitay4

|#

;; Right-angled triangle, finding the permit with the help of Pythagoras
(sqrt (+ ( * 3 3)( * 4 4))) ; Answer is 5

;; Simple expression
(+ 2 ( * 3 4)( - ( + 1 2) 3)) ; Answer is 14

;; Conditions
;; Single 'if' condition
(if ( < 2 3) 10 20) ; If true, we'll get 10, otherwise return 20. In this case we'll get 10
(if ( > 10 8 11) 0 -5) ;Several values in one if block. Answer is -5, because 10 is greater than 3, but 3 is lower than 11 (kind of sequence)

;; Multi 'if-else' condition
(cond
  [(eq? 'a 'b) 0 ]
  [(eq? 'a 'c) 1 ]
  [else 2]) ; Sure 'a and 'b aren't equal, as well as 'a and 'c, therefore we'll get the else, which is 2

(cond
  [(and #t #f) 0 ]
  [(or #t #f) 2 ]
  [else 3 ]) ; #t represent true and #f is false. NOTE! only #f is false in Racket

;; Const and functions
(define PI 3.14) ; Const number, no type needed (int, float etc)

(: Not : Number -> Boolean )
(define (Not a)
  (cond
    [a #f]
    [else #t])) ; Example for function, checks if value a is the opposite

;; Function which check if the given number is in range 0<=x<=5
(: f : Number -> Number )
(define (f num)
  (if (and (>= num 0 )(<= num 5)) 1 0 ))
(test (f 0) => 1) ; A way to test our code, using the word 'test'. we set the value we're expecting to receive
(test (f 7) => 0)

#| Union of returns types, using the U char
For example: if value == 0 return false, else return value + 1
Note that we can use 'if' condition to return 2 seperate answers
|#
(: func : Number -> (U Number Boolean) )
   (define (func value)
     (if (eq? value 0) #f (+ value 1)))

(test (func 0) => #f)
(test (func 4) => 5)
(test (func 100) => 101)

#| Using error word
if 0<=x5 return 2
   5<=x<10 return 4
   x>=10 return 7
   else return error (if negetive number)
|#
(: fm : Number -> Number)
(define (fm x)
  (cond
    [(and (>= x 0) (< x 5)) 2]
    [(and (>= x 5)(< x 10)) 4]
    [( >= x 10 ) 7]
    [else (error 'fm "The function is not define in ~s" x)]))

(test (fm 0) => 2)
(test (fm 5) => 4)
(test (fm 10) => 7)
(test (fm -1) =error> "The function is not define in -1")