#lang pl

#|
DATE:    June 2021
AUTHOR:  I. Ben Moshe

Note: All my course files and their solution can be found at my GitHub, at: github.com/bmitay4/Racket
|#

;;PART A
#| Please complete the missing rules below

The solution was clear and easy. Since the <SOL> contained 'True', then it is natural that it
should also contain 'False', because we will also work with Boolean values.

<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> }
        |  <id>
        |  { with {<id> <SOL>  <id> <SOL>} <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment
        |  True
        |  False
        | { if <SOL> then <SOL> else <SOL> }
        | { equal? <SOL> <SOL> }

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
#|
 In order to fill in the gaps in the Equal variant, it was clear that we would need to compare two particular values,
 and because we are dealing with <SOL>, it was clear that the variants would get two <SOL> values
|#
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  ;;    [With  Symbol Symbol SOL SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL]
  [Bool Boolean]
  [If SOL SOL SOL]
  [Equal SOL SOL])


;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))

(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))
  
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))
  
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.

#|
 In order to fill in the gaps in 'parse-sexpr', 'e had to look at the tests and variants of <SOL> and the code was written by itself.
 However, I did spent some time on "if cond 'then true-cond 'else false-cond" parse.
 I was trying to figure out how to correctly write the parse of this section. And after further review of the tests they
 received, the way was clearer.
|#

(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    ['True (Bool true)] 
    ['False (Bool false)] 
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name1) named1 (symbol: name2) named2) body)
        (CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1) (parse-sexpr named2))] ;;; There is no With constructor. Replace it with existing constructors...
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name1) (symbol: name2)) body)
        (if (equal? name1 name2)
            (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr)  ;; cannot use the same param name twice
            (Fun name1 name2 (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'if cond 'then true-cond 'else false-cond)
     (If (parse-sexpr cond)
         (parse-sexpr true-cond)
         (parse-sexpr false-cond))] 
    [(list 'equal? arg1 arg2) (Equal (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  

;;; Tests for parse
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{1 x}")  =error> "parse-sexpr: bad syntax in (1 x)")
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'c
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Set '())))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} S1 {union {1 2 3} {4 2 3}}}
                          {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}}") =error> "parse-sexpr: bad `with' syntax in (with (S (intersect (1 2 3) (4 2 3))))")
(test (parse "True") => (Bool true))
(test (parse "False") => (Bool false))
(test (parse "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") =>
      (If (Equal (Set '(1 2 3)) (Set '(1 2))) (Set '(1 2 3)) (Set '(1 2))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-dynamic {fun {x y} {union x S}}
                               {if {equal? S {scalar-mult 3 S}}
                                   then S
                                   else {4 5 7 6 9 8 8 8}}
                               {}}}")
      => (CallS (Fun 'S 'c
                     (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S)))
                            (If (Equal (Id 'S) (Smult 3 (Id 'S)))
                                (Id 'S)
                                (Set '(4 5 6 7 8 9)))
                            (Set '())))
                (Inter (Set '(1 2 3)) (Set '(2 3 4)))
                (Set '())))


;; PART B
;;-----------------------------------------------------
;; Evaluation 
#|

This was one of the easier parts of the task, since the code relied on a similar code we did in class so it was just a matter
of looking at my GitHub and entering the missing information.
In addition, when understanding the environments model, the eval part is very clear.
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below
    eval({ N1 N2 ... Nl })      = sort( create-set({ N1 N2 ... Nl })) ;; where create-set removes all duplications from
                                                                         the sequence (list) and sort is a sorting procedure

    eval({scalar-mult K E})     = { K*N1 K*N2 ... K*Nl }              ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2})     = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})         = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({fun {x1 x2} E},env)   = <{fun {x1 x2} E}, env>

    eval({call-static E-op E1 E2},env)
                                = eval(Ef, extend(x2,eval(E2,env) (extend (x1, eval(E1, env), envf))
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise

    eval({call-dynamic E-op E1 E2},env)
                                = eval(Ef, extend(x2,eval(E2,env) (extend (x1, eval(E1,env),env))
                                                      if eval(E-op, env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise

    eval(True,env)              = true
    eval(False,env)             = false

    eval({if E1 then E2 else E3},env)
                                = eval(E3, env)       if eval(E1,env) = false
                                = eval(E2, env)       otherwise

    eval({equal? E1 E2},env)    = true                if eval(E1,env) is equal in content to eval(E2,env)
                                = false               otherwise

|#

;; Types for environments, values, and a lookup function
;; Because the requirement was also to have a Boolean value, it was clear what was missing in the BoolV variant

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean]) 

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

#|
   The SetV-> set function receives VAL as an input and return Set if input is from SetV form.
   otherwise returns error otherwise

   The smult-set receives number n and a VAL, and return a new VAL, when each member in the set is multiplied
   by the value n, similar to the action we did in task 2.

   The set-op function takes an op, and two VALS, just like we did in class,
   and returns a VAL which is the reuslt of op on {val1,val2}
|#
(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))
  
(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (SetV (map mult-op (SetV->set s))))


(: set-op : (SET SET -> SET) VAL VAL -> VAL )
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))


;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable.

#|
   Completing the eval function was a bit tricky for me, especially the part of "[(If cond true-cond false-cond)]".
   It took me a while to figure out how to properly build this part, since I got an error in the tests at each run.
   As written in the assignment, we had to implement "If" as it is implemented in Racket, and after a number of attempts
   I realized that it is correct to use eqv? On the returned part from eval cond env and decide what to return to the user accordingly.
|#

(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV (create-sorted-set S))]
    [(Smult n set) (smult-set n (eval set env))]
    [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
    [(Union l r) (set-op set-union (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env) 
                        (Extend bound-id1 (eval arg-expr1 env)
                                f-env)))]
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env) 
                        (Extend bound-id1 (eval arg-expr1 env)
                                env)))]
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))]
    [(Bool b) (BoolV b)]
    [(If cond true-cond false-cond)
     (let ([cval (eval cond env)])
       (cases cval
         [(BoolV b)
          (if (eqv? b #t) (eval true-cond env) (eval false-cond env))];; b is a Boolean value
    [else (error 'eval "`Bool' expects a boolean, got: ~s" cval)]))]
    [(Equal l r) (if (equal? (eval l env) (eval r env))
                     (BoolV #t)
                     (BoolV #f))]))


#|
   That was the most challenging part of the task for me. I understood the overall idea, and it is very similar to the
   global environment we created in the last lesson with the mathematical operations (+, -, *, /), but as mentioned I had a
   hard time realizing it in this task.
   In this part I was helped by my wife who explained to me how to build the environment properly.
|#

(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
          (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                  (Extend 'cons (FunV 'f 's (Fun 'foo 'spare-param (CallS (Id 'foo) (Id 'f) (Id 's))) (EmptyEnv))
                          (EmptyEnv)))))


#|

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}} S1 {}}
                   {with {S {intersect {call-static first p {}}
                          {call-static second p {}}}  S1 {}}
                {call-static {fun {x y} {union x S}}
                                  {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
                                                => '(2 3 6 9))


parse solution

(CallS
 (Fun
  'p
  'S1
  (CallS
   (Fun 'S 'S1 (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) (Smult 3 (Id 'S)) (Set '(4 5 6 7 8 9))))
   (Inter (CallS (Id 'first) (Id 'p) (Set '())) (CallS (Id 'second) (Id 'p) (Set '())))
   (Set '())))
 (CallS (Id 'cons) (Set '(1 2 3)) (Set '(2 3 4)))
 (Set '())



(test (run "{with {p {call-static cons {1 2 3} {4 2 3}} c{}}
                 {with {foo {fun {x y} {intersect x y}} c {}}
                                 {call-static p foo {}}}}")
                                                 => '(2 3))
(test (run "{with {p {call-static cons {1 2 3} {4 2 3}} c{}}
                 {with {foo {fun {x y} {intersect x y}} c {}}
                                 {call-static p foo {}}}}")
                                                => '(2 3))

|#

(: run : String -> (U SET VAL Boolean))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(SetV S) S]
      [(BoolV b) b]
      [else result]))) ;;Should it return error, bad evaluation. TODO.


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))


(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
               {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                    S1 {}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}
                     S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
                   S1 {}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}
                     S1 {}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}") =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "True") => #t)
(test (run "False") => #f)
(test (run "5") =error> "parse-sexpr: bad syntax in 5")
(test (run "{with {S {intersect {1 2 3} {4 2 3}} S1 {}}}") =error> "parse-sexpr: bad `with' syntax in (with (S (intersect (1 2 3) (4 2 3)) S1 ()))")
(test (run "{if {union {0 1 2} {0 2 1}} then {2 1} else {2 1}}") =error> "`Bool' expects a boolean, got: #(struct:SetV (0 1 2)")
(test (run "{if {equal? {0 1 2} {0 2 1}} then {2 1} else {2 1}}") => '(1 2)) ;;Change the order, but still same result
(test (run "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") => '(1 2))
(test (run "{equal? {union {1 2 3} {4 2 3}} {1 2 3 4}}") => #t)
(test (run "{equal? {union {7 8 10} {7 8 11}} {7 8 10 1011}}") => #f)
(test (run "{union {equal? {4} {4}} {4 2 3}}") =error> "SetV->set: expects a set, got: #(struct:BoolV #t)")


;; PART C
#|

1. Now we have 4 types in SOL. 3 of them can only be understood from looking at the run function, which it can return SET VAL or Boolean,
   and another type is a function (procedure).

2. In 'With' we used statistically because we do not want to mix between the environments of E2 and E1 (as shown in the part of the eval)
   Of course, at 'call-static' we called the function statically as opposed to 'call-dynamic' where we called the function dynamically.
   The important choice for us focused on how we want our environment and the values ​​to be fed into it. If in the static part we were
   to put in an environment that is dynamic, i.e. one that can be changed during program execution, we might or may not get the value
   we wanted to get.
   

3. We used call-static in in this section.
   It was simple in my opinion to look at some of the Evaluation rules, and understand that we would like to
   expand the environment statically, so for example when we call first then we get the value we wanted to get,
   and not accidentally get value from another extended environment dynamically.
   
4. In the specific example below there will be no difference whether we switch to call-dynamic in some places.

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                                               S1 {}}
                  {with {S {intersect {call-static first p {}}
                         {call-static second p {}}}
                                            S1 {}}
                  {call-static {fun {x y} {union x S}}
                           {scalar-mult 3 S}
                          {4 5 7 6 9 8 8 8}}}}")
                                 => '(2 3 6 9))
|#
