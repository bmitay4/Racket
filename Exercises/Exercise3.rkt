#lang pl

#|

This file contains basic Racket operations and explanations of how to run the program.
Note, pay attention to the spaces in the code.

By bmitay4

|#

;; The key word 'Let' allowing us to set a const value by name for a local block
(let ([x 5][z 3][y 7])
  (* x y z))

;; 'match' is looking for pattern and returning its value from a bunch of patterns
(match (list 1 2 3)
  [x x])

;;Example of let combine match. Note that the 'let' is local
(let ([foo 'x])
(match foo
  ['x "yes"] ; return "yes"
  [else "no"])) ; Note that 'else' word isn't uniqe in match, we can call it whatever we want

(match '(1 2 3)
  [(list x y z) (+ x y z)]) ;match works also on list elements. return 6

;; Calculate the sum of numbers list using match
(: sum : (Listof Number) -> Number)
(define (sum lst)
  (match lst
    ['() 0]
    [(cons h t)(+ h (sum t))])) ;NOTE! h is first and t is rest. using cons allready allow this
(test (sum (list 1 2 3 4)) => 10)

;; using .. for repeat
(match '((1 2) (3 4) (5 6) (7 8))
  [(list(list x y)...)(append x y)])

#| We can use _ in match that it will always fit, but will not have a linked value
(match value
  [_ result-expr])

We can check the type of the value before linking to it (number, symbol, string)
(match value
  [(number:n) result-expr])
By that, only if n is type of number it will be linked to it, and return its expression

We also can use 'and' 'or' word to check fitness for multi/some patterns
(match value
[(and pat1 pat2) result-expr]) . Thus, if pat1 and pat2 are exist, it will return its expression

(match value
[(or pat1 pat2) result-expr]) . Thus, if ONE OF THE pat1 and pat2 are exist, it will return its expression
|#
