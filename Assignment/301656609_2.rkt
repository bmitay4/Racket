#lang pl 02

#|

DATE:    May 2021
AUTHOR:  I. Ben Moshe

Note: All my course files and their solution can be found at GitHub, at: github.com/bmitay4/Racket

|#

#|

Question 1.a


|#


#|

Question 1.b


|#


#|

Question 2

We were asked to write a function that receives a list, and returns the sum of the squares of the list members.
For this purpose, we built the auxiliary function square which receives a number as input and returns its square.

sources: https://docs.racket-lang.org/reference/pairs.html?q=map#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
|#

;; Function 'square' receives a number as an input and return its square number (power 2)
(: square : Number -> Number)
(define (square n)
  (if (eq? n 0) 0 (* n n)))

#| Function 'sum-of-squares' receives a list of numbers as an input and return the sum of the squares of all of the numbers in the given list
   I've used the foldl and map together, as well the square function mention above.
   While map performs the square function on the values ​​of the given list, and also returns the list after the changes made by square, foldl receives this list,
   and unite the numbers in the list, next to the initial value 0 (which does not change the result)
|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares ls)
  (foldl + 0 (map square ls)))

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 3 10 2)) => 113)
(test (sum-of-squares '(2 8)) => 68)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1.5 2.25 6.5)) => 49.5625)
(test (sum-of-squares '(10 10 10 10)) => 400)

#|

Question 3.a

In this question I have struggled greatly to understand the roles of its internal functions, and possibly also because I have not practiced enough tail recursion.
However, I found a solution on the net, and after making some minor changes it was found suitable. (See attached link)
The function 'createPolynomial' gets a list of numbers.
The nested 'poly' function receives a list of numbers, and three additional values: a given number (coefficient), a power value that is raised in each round by 1,
and the accumulator that will hold the final value for us at each stage (so this is a tail function)

source: https://stackoverflow.com/questions/55810809/create-polynomial-function
|#

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL) accum                                                               ;If we reach an empty list, we will return the accumulator value
        (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power ))) ) )) ;Otherwise, run a tail recursion (note we increase the accumulator value each time) with the rest of the list, and increase the power value by 1 each time
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0)) polyX)


(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>(+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5(expt 0 3))))
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(define p123 (createPolynomial '(1 2 3)))
(test (p123 4) => (+ (* 1 (expt 4 0)) (* 2 (expt 4 1)) (* 3 (expt 4 2))))
(define p789 (createPolynomial '(7 8 9)))
(test (p789 2) => (+ (* 7 (expt 2 0)) (* 8 (expt 2 1)) (* 9 (expt 2 2))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)


#|

Question 3.b.i

Grammar:
      <PLANG> ::= {{poly <AEs> }{<AEs> }}
      <AEs>   ::= <AE> | <AE> <AEs>
      <AE>    ::= <num>      
                  {+ <> <AE> }
                  {- <AE> <AE> }
                  {* <AE> <AE> }
                  {/ <AE> <A> }

Some of the syntax is taken from the classroom examples.

|#


#|

Question 3.b.ii

|#

(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code 
      [(list (cons 'poly '()) (list x ...)) (error 'parse "at least one coefficient is required in ~s" code)] ; In case it is an empty list
      [(list (cons 'poly hea) '()) (error 'parse "at least one point is required in ~s" code)] ; If there is a list, but the next organ does not exist (which is the power base. there can also be a list of organs)
      [(list (cons 'poly ls) (list x ...)) (Poly (map parse-sexpr ls) (map parse-sexpr x))]
      [else (error 'parse "bad syntax in ~s" code)]))) ; We can write anything instead of 'else' 

(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")  =error> "parse: at least one point is required in ((poly 1 2) ())")


#|

Question 3.b.iii

|#
