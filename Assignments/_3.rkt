#lang pl

#|
DATE:    May 2021
AUTHOR:  I. Ben Moshe

Note: All my course files and their solution can be found at my GitHub, at: github.com/bmitay4/Racket
|#

;;PART A

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL> }
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ |  <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [IdS    Symbol]
  [WithS  Symbol SOL SOL])


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.
#|
Solution Explanation:
This was the easiest part for me in the task, since I wrote most of the code from the class lesson for WAE, FLANG, so
it was easy to make the adjustments to SOL.

As mentioned, most of the code is in my Git, a link is at the top of the file
|#
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns)] 
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named-expr) body)
        (WithS name(parse-sexprS named-expr) (parse-sexprS body))]
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs)(Inter (parse-sexprS lhs)(parse-sexprS rhs))]
    [(list 'union lhs rhs)(Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

  
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")
(test (parseS "{some-else S {3}}")
      =error> "parse-sexprS: bad syntax in")
(test (parseS "S") => (IdS 'S))
(test (parseS "{with {S {intersect {1 2 3} {4 2 3}}}
                              {scalar-mult 3 S}}") 
      =>
      (WithS 'S
             (Inter (Set '(1 2 3)) (Set '(4 2 3)))
             (Smult 3 (IdS 'S))))

(test (parseS "{scalar-mult 5 {6 10 9}}") => (Smult 5 (Set '(6 10 9))))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 
#|
General solution:
(1) If the list is empty, return false
(2) If the organ is the same as the given organ return true.
(3) Otherwise, proceed to the rest of the list and repeat step 1 again

We asked to check if two conditions are met, whether the list is empty and whether the given number is contained within the SET.
For this purpose we went through the whole SET and sought equality
Therefore, in Racket we will use cond when we want to use if-elseif-else methods
|#
(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond
    [(null? l) #f]
    [(eq? n (first l)) #t]
    [else (ismember? n (rest l))]))

(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? 5 '()) => #f)
(test (ismember? 6 '(4 5 7)) => #f)
(test (ismember? 0 '(10 100)) => #f)
(test (ismember? 7 '(3 3.3 7)) => #t)


#|
General solution:
(1) If the list is empty, return it.
(2) Check using the ismember? function, if the given value is contains in the rest of the list.
    If so, we will return the rest of the list, meaning, removing all duplicates of the numbers at the top of the list.
(3) Otherwise, return the list and repeat step 1 for the rest of the list(Without the value just examined)

We were asked to remove duplicates from a given list so that one single member of each value would appear.
To do this, we used a function we just built, to check if a particular value is in a given list.
During the mission I was apparently stuck in an endless loop, and DrRacket informed me that I was running out of memory.

The problem was at the end of the function, when I set that if the given value does not exist within the list, then we'll return remove-duplicate with the given list
thus entering an infinite circle.
This occurred because I just wanted to return the list without performing the process again for the given value.

Error code line was: [else (remove-duplicates l)]
|#
(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond
    [(null? l) l]
    [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
    [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '(2 1 3 4 2)) => '(1 3 4 2))
(test (remove-duplicates '(1 1 1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(10 100 1000)) => '(10 100 1000))


#|
Solution Explanation:
In the code we have already got the built-in sort function, as well as the < symbol so we expect to sort a given list from the smallest value to the largest.
However, a SET is defined to be a collection of values, without duplicates, meaning that a value in a SET cannot appear in it more than once.
Therefore, it is easy to understand that we had to use the function we just built, remove-duplicates, to go through the sorted list, look for duplicates and remove them.
|#
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))

(test (create-sorted-set '(3 4 5 3)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '(7 4 3 2 2 1)) => '(1 2 3 4 7))
(test (create-sorted-set '(8 8 8 3 1)) => '(1 3 8))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(103 102 101 101 2)) => '(2 101 102 103))


#|
Solution Explanation:
We were asked to write a function that would perform SET unification, and as mentioned, a SET is defined to be a collection of values,
without duplicates, meaning that a value in a SET cannot appear in it more than once.

Therefore, in order to perform a union between groups, we will of course have to activate on their union the function that removes duplicates.
So I chose to use create-sorted-set, which also sorts the consolidated list.
Of course we could also write: remove-duplicates(append A B). Then we would get a consolidated but not sorted list.

Source: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29
|#
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(1 2 3) '(3 4 5)) => '(1 2 3 4 5))
(test (set-union '(7 8 9) '(12 19)) => '(7 8 9 12 19))
(test (set-union '(3 2 1) '(1 2)) => '(1 2 3))
(test (set-union '(3) '()) => '(3))

#|
Solution Explanation:
We were asked to build a function that gets two SETS, and intersec between the two, meaning that it would return only a
list of values ​​that are in the two lists.
Most of the function already existed, including mem-filter, which checked whether a given value existed in list A.

So, we used filter built-in funtion, which gets a procedure for filtering and listing, and the procedure is applied to each element
of the list from first to last.

===IGNORE THIS=== (The code was written in this way before being updated by the lecturer)
Note, we were not required to sort the list after the intersection.
Therefore the order of the values ​​to be returned, will be determined according to their appearance in list B.
===IGNORE THIS===

Source: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
|#
(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(1 1 2) '(1)) => '(1))
(test (set-intersection '(4 1 1 2) '(4 1)) => '(1 4))
(test (set-intersection '(10 100 4 5) '(4 100)) => '(4 100))
(test (set-intersection '(7 4 0) '(0 4)) => '(0 4))


#|
Solution Explanation:
We were asked to write a function that receives a scalar and a SET, and doubles all the values ​​in the given scalar.
The direction was to use the map function on the given group, and my dilemma was how to implement the procedures,
whether using external function.

I finally decided to exercise through inner function mult-op, after I failed to exercise it through lambda

Source: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
|#
(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (: mult-op : Number -> Number)
  (define (mult-op value)
    (* n value))
  (map mult-op (create-sorted-set l)))

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 5 '(5 10 15)) => '(25 50 75))
(test (set-smult 0.5 '(8 4 2)) => '(1.0 2.0 4.0))
(test (set-smult 10 '(1 2 3)) => '(10 20 30))
(test (set-smult 0 '(3 4 5)) => '(0 0 0)) ;; This test is invalid. We assume the scalar is >=1


;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`set' is a { <NumList> }, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      set[v/x]              = set 
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

#|
Solution Explanation:
This also was an easy part, since I wrote most of the code from the class lesson for WAE, FLANG, so
it was easy to make the adjustments to SOL substS.
|#
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
            (substS named-expr from to)
            (if (eq? named-expr from)
                bound-body
                (substS bound-body from to)))]))

(test (substS (Inter (WithS 'x (Smult 4 (Set '(1 5 15))) (Union (IdS 'x) (Set '(2 3)))) (Set '(1 6))) 'x (Set '(4 20)))
      => (Inter (WithS 'x (Smult 4 (Set '(1 5 15))) (Union (Set '(4 20)) (Set '(2 3)))) (Set '(1 6))))
(test (substS (Inter (WithS 'x (Set '(1 7)) (Union (IdS 'x) (Set '(2 3)))) (Set '(1 6))) 'x (Set '(9)))
      => (Inter (WithS 'x (Set '(1 7)) (Union (Set '(9)) (Set '(2 3)))) (Set '(1 6))))
(test (substS (Union (WithS 'x (Set '(7 7 7)) (Union (IdS 'x) (Set '(1 5 6)))) (Set '(32 54))) 'x (Set '(1 109)))
      => (Union (WithS 'x (Set '(7 7 7)) (Union (Set '(1 109)) (Set '(1 5 6)))) (Set '(32 54))))


;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    eval({ N1 N2 ... Nl })  = sort( create-set({ N1 N2 ... Nl }))
                               ;; where create-set removes all duplications from
                                  the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) = { K*N1 K*N2 ... K*Nl }
                               ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2}) = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})     = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
    eval(id)                = error!
|#



;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable.

#|
Solution Explanation:
This also was an easy part, since I wrote most of the code from the class lesson for WAE, FLANG, so
it was easy to make the adjustments to SOL eval.

I encountered a certain difficulty in the part of (WithS name named body).
In time I realized that since the function expects to get SOL, then it is not possible in the last part of the eval for With to do: eval (with),
but (Set (eval named))
|#
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named body)
     (eval (substS body
                   name
                   (Set (eval named))))]
    [(IdS name) (error 'eval "free identifier ~s" name)]))

(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{union {1 1 1 1} {8 3 11}}") => '(1 3 8 11))
(test (run "{intersect {8 3 1 11} {8 3 11}}") => '(3 8 11))
(test (run "{intersect {1 2 1} {3 4 6}}") => '())
(test (run "{scalar-mult 3 {1 2 1}}") => '(3 6))
(test (run "{scalar-mult 10 {0 40 12}}") => '(0 120 400))
(test (run "{* 5 5}") =error> "parse-sexprS: bad syntax in")
(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}") => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B} {4 5 7 9 8 8 8}}}") =error> "eval: free identifier")


;;PART B

#|
This task is mostly based on code snippets we wrote for WAE, as mentioned I have in Git in the link at the top of the file.
We copied them here to use them for the function freeInstanceList we were asked to build.

Our Grammer:
<WAE>:: = 1 <num>      
           |2 {+ <WAE> <WAE> }
           |3 {- <WAE> <WAE> }
           |4 {* <WAE> <WAE> }
           |5 {/ <WAE> <A> }
           |6 {with { <id> <WAE> } <WAE> }
           |7 <id>

Where <num> stands for any Racket number, and <id> stands for any Racket symbol
|#

;;Defining the AST -- WAE
(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [With Symbol WAE WAE]
  [Id Symbol])

;;parse-sexpr
(: parse-sexpr : Sexpr -> WAE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sxp
       [(list 'with (list (symbol: name) named-expr) body)
        (With name(parse-sexpr named-expr) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad with syntax in ~s" sxp)])]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))] ;; We just ask if we holding a list, which the first value is '+, and has two more values (3 in total)
    [(list '- l r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
    [(list '/ l r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad sytax in ~s" sxp)]))   ;; We can write anything instead of 'else' 

;;subst
(: subst : WAE Symbol WAE -> WAE)  
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(With name named body)
     (With name
           (subst named from to)
           (if (eq? name from)
               body
               (subst body from to)))]
    [(Id name) (if (eq? name from) to expr)]))

;;parse
(: parse : String -> WAE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


#|
Solution Explanation:
To implement the freeInstanceList function, we were inspired by part A of the task, and rebuilt the functions responsible for checking
 whether a value is in the list and whether there are duplicates in the list to suit the new situation.

The goal is simple, look for symbols. Therefore in the cases phase, the function will return a value only if we are looking at a id variant.
Of course in any case we will call the function remove-dup to remove duplicates.
|#
(: ismembersym? : Symbol (Listof Symbol)  -> Boolean)
(define (ismembersym? n l)
  (cond
    [(null? l) #f]
    [(eq? n (first l)) #t]
    [else (ismembersym? n (rest l))]))

(: remove-dup : (Listof Symbol)  -> (Listof Symbol))
(define (remove-dup l)
  (cond
    [(null? l) l]
    [(ismembersym? (first l) (rest l)) (remove-dup (rest l))]
    [else (cons (first l) (remove-dup (rest l)))]))

(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
  [(Num n) '()]
  [(Add l r) (remove-dup(append (freeInstanceList l) (freeInstanceList r)))]
  [(Sub l r) (remove-dup(append (freeInstanceList l) (freeInstanceList r)))]
  [(Mul l r) (remove-dup(append (freeInstanceList l) (freeInstanceList r)))]
  [(Div l r) (remove-dup(append (freeInstanceList l) (freeInstanceList r)))]
  [(Id name) (list name) ]
  [(With name named body)
   (remove-dup(append (freeInstanceList (subst body name (Num 0)))
           (freeInstanceList named)))]))

(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{/ x {+ z {- y x}}}")) => '(z y x))
(test (freeInstanceList (parse "{* x {+ y {- y z}}}")) => '(x y z))
(test (freeInstanceList (Num 0)) => '())
(test (freeInstanceList (Id 'z)) => '(z))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(x z))
