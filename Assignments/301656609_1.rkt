#lang pl

#|

DATE:    April 2021
AUTHOR:  I. Ben Moshe

Note: All my course files and their solution are in GitHub, at: github.com/bmitay4/Racket

|#

#|

Question 1.1

The append5 function receives 5 characters (Char Char..) and returns their concatenation in the order they are entered into the function.
We learned in the first practice that the word 'define' can be used to define a particular value for a particular variable (eg (define PI 3.14)), so the
main direction for the solution in this way was clear.
However, I have encountered difficulty in how to get the string back from the function, since there is no familiar word known from other languages ('return').
But we also learned in the first practice that conditions 'if' or 'cond' are useful and can be used to return values ​​from a function.

source: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string%29%29

|#

(: append5 : Char Char Char Char Char -> String)
(define (append5 a b c d e)
  (define str (string a b c d e))
  (if (eq? a e) str str))


(test (append5 #\a #\b #\c #\d #\e) => "abcde")
(test (append5 #\l #\a #\e #\v #\a) => "laeva")
(test (append5 #\z #\h #\r #\h #\z) => "zhrhz")


#|

Question 1.2

The permute3 function receives 3 characters as input, and returns their various permutations in a list.
This task was complex for me, as I was looking for creative ways to create the different permutations, however I finally chose the
simple way, building each of the permutations using 'define' and 'string' keywords and then inserting them back into a list.

As in the previous exercise, the way to return the output was using 'if' conditions, and for that I generally chose to check if the first character is 'a'

|#

(: permute3 : Char Char Char -> (Listof String))
(define (permute3 a b c)
  (define str_a (string a b c))
  (define str_b (string a c b))
  (define str_c (string b a c))
  (define str_d (string b c a))
  (define str_e (string c b a))
  (define str_f (string c a b))
  (define ls (list str_a str_b str_c str_d str_e str_f))
  (if (eq? a #\a) ls ls))


(test (permute3 #\a #\b #\c) =>'("abc" "acb" "bac" "bca" "cba" "cab"))
(test (permute3 #\d #\f #\g) =>'("dfg" "dgf" "fdg" "fgd" "gfd" "gdf"))
(test (permute3 #\g #\i #\j) =>'("gij" "gji" "igj" "ijg" "jig" "jgi"))

#|

Question 2.a

In this exercise, we were required to build a recursive function, which receives a list of lists, and returns as output the
number of internal lists that contain exactly 3 elements.
For this purpose, I built an external function that receives a list as input, and returns the number of members in it, to help build the solution.

The structure of the 'count-3lists' function:
Receives a list of lists, and checks the following conditions:
If the list is empty, return 0.
If the number of members in the given list is 3, return 1 and perform the function again with the rest of the list.
Otherwise, continue to perform the function on a list permit.
In this recursive way, we were able to count the number of lists that contain exactly 3 organs. (Of any type)

|#

(: list-length : (Listof Any) -> Number)
(define (list-length list)
  (if (null? list) 0
      (+ 1 (list-length(rest list)))))

(: count-3lists : (Listof (Listof Any)) -> Number)
(define (count-3lists ls)
  (cond
    [(null? ls) 0]
    [(= (list-length (first ls)) 3) (+ 1 (count-3lists(rest ls)))]
    [else (count-3lists(rest ls))]))


(test (count-3lists '((1 2 3) (4 5 6) (7 8) (1 2 3 4) ("a" "b" "c"))) => 3)
(test (count-3lists '((5 2 5) (4 4) (7 8 9) ("a" "b" "c" "z"))) => 2)
(test (count-3lists '((3 "t" g) (3 0) (0 0 0) ("h" 0 "m" 3))) => 2)
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)

#|

Question 2.b

This task is similar to the previous section, however we were required to solve the task using tail recursion, as we learned in the pratice lesson.
Therefore, I built the 'helper' function that gets two values: list and number, which will be used as accumulator.

Each time the function locates a list with exactly 3 members, the accumulator increases by 1, and the function again calls the helper with the rest
of the list and the accumulator with its new value.

Inspiration for the solution was obtained by participating in the practice lesson, by T.Suad

|#

(: helper : Number (Listof (Listof Any)) -> Number)
(define (helper acc ls)
  (cond
    [(null? ls) acc]
    [(= (list-length (first ls)) 3) (helper (+ acc 1) (rest ls))]
    [else (helper (+ acc 0) (rest ls))]))

(: count-3lists-tail : (Listof (Listof Any)) -> Number)
(define (count-3lists-tail ls)
  (helper 0 ls))


(test (count-3lists-tail '((1 2 3) (4 5 6) (7 8) (1 2 3 4) ("a" "b" "c"))) => 3)
(test (count-3lists-tail '((5 2 5) (4 4) (7 8 9) ("a" "b" "c" "z"))) => 2)
(test (count-3lists-tail '((3 "t" "g") (3 0) (0 0 0) ("h" 0 "m" 3))) => 2)

#|

Question 2.c

This is the exercise on which I had the most difficulty of all.
Due to lack of experience in Racket language, I did not know how to make proper use of the various conditions and operators, nor did I
have the sufficient knowledge to exercise the function using 'match'.

I declared the function 'count-3listsRec' which receives a list of lists and returns a number - the number of lists in length 3.
Similar to the previous section, I checked if the given list is empty, and used the function of Racket 'andmap', which allows you to go over the elements in the list
and check if a certain condition is met.
This condition was necessary to check the nested lists and estimate their size to give proper output.

Source: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._andmap%29%29

|#

(: count-3listsRec : (Listof (Listof Any)) -> Number)
(define (count-3listsRec ls)
  (cond
    [(null? ls) 0]
    [(= (list-length (first ls)) 3)
     (if (andmap list? (first ls)) (+ 1 (+ (count-3listsRec (first ls)) (count-3listsRec (rest ls))))
         (+ 1 (count-3listsRec(rest ls))))]
    [(and (or (> (list-length (first ls)) 3) (< (list-length (first ls)) 3)) (andmap list? (first ls)))
     (+ (count-3listsRec (first ls)) (count-3listsRec (rest ls)))]
    [else (count-3listsRec(rest ls))]))

(test (count-3listsRec '(()(4 5 6))) => 1)
(test (count-3listsRec '(() () ())) => 0)
(test (count-3listsRec '(((1 2 3) (4 5 6) (7 8 9)) (7 8) (1 2 3 4) ("a" "b" "c"))) => 5)
(test (count-3listsRec '((5 2 5) ((4 4) (7 8 9)) ("b" "c" "z"))) => 3)
(test (count-3listsRec '((3 "t" "g") ("h" 0 "m" 3))) => 1)

#|

Question 3

This question was the most interesting, and to solve it I closely followed the lectures and practice lessons to fully understand the define-type in Racket.
Our define-type KeyStack contains 2 fields, one for EmptyKS which indicates that the cartridge is empty, and the other for the Push operation that allows entries
to be entered into the stack.

The main difference in this exercise, which also required a better understanding of Racket, was the shift to using 'cases' versus 'match'.
We have learned that the use of 'cases' is necessary for every symbol / type we build, which is not part of the Racket types, And through it
we can check a number of situations / conditions for our type and act accordingly.

|#

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

(: search-stack : Symbol KeyStack -> (U String #f) )
(define (search-stack sym ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push s st k) (if (eq? sym s) st (search-stack sym k))]))

(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'd "A" (EmptyKS))))) => "B") 
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)

(: pop-stack : KeyStack -> ( U #f KeyStack) )
(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push s st k) k]))

(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))  
(test (pop-stack (EmptyKS)) => #f)

#|

Question 4

Comments were written in the places specified

|#

(: is-odd? : Natural -> Boolean)
;; A function statement called 'is-odd?' which receives as a single input a natural number, and returns as output a Boolean answer, true or false
;; Is-odd function? Checks if the given number is negative is positive. To do this, the function initially asks zero ?, i.e. whether the number is 0
;; If the number is not 0, the function calls the function is-even? with the given natural number minus 1, which completes the test.
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))


(: is-even? : Natural -> Boolean)
;; A function statement called 'is-even?' which receives as a single input a natural number, and returns as output a Boolean answer, true or false
;; Similar to the previous function, this one also asks if the given number is 0. If the answer is yes, the function will return a false.
;; Otherwise, the function will take the value of the given number X, subtract 1 from it, and send it back to 'is-odd?'
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))


(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file…

;; The "every?" function gets a function and a list of elements of type 'A', and returns Boolean answer, true or false. 
;; 'pred' functions gets an element of type 'A' and returns true or false.
;; Returns true if all pass the pred.

(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; "all-even" gets list of Nautral numbers and returns true if all the elemtns are even, by sending the list and the function "is even" to "every?"
;; which verify if all the elemnets are even.

(define (all-even? lst)
  (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
                                                         Boolean))

;; The function "every2?" gets 2 inner function (that get type 'A' or 'B' and return #f or #t) and 2 lists of type 'A' and 'B', and
;; returns true if all the elements of list1 meets the requirements of pred1 and the same for list2 and pred2, and false if not.
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))