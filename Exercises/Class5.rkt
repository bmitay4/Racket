#lang pl

;;We'll contiunie establish our AE language
;;Since we added the 'with' option we will change the name of our language to WAE
#|
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

#|

Remainder of Sexpr Defenition:
Basis: Any Number/Symbol is an Sexpr
General: Any List of Sexpr is an Sexpr

|#
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
    
(: parse : String -> WAE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(test (parse "4") => (Num 4))
(test (parse "{+ 3 4}") => (Add (Num 3) (Num 4)))
(test (parse "{+ 3 { - 5 4}}") => (Add (Num 3)
                                       (Sub (Num 5)
                                            (Num 4))))
(test (parse "{+ 2 3 4 5}") =error> "bad sytax")

#|
eval(<num>) = <num> (e.g. eval("5") = 5
eval("{+ E1 E2}") = eval(E1) + eval(E2)
eval("{- E1 E2}") = eval(E1) - eval(E2)
eval("{* E1 E2}") = eval(E1) * eval(E2)
eval("{/ E1 E2}") = eval(E1) / eval(E2)

(: eval : WAE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n] 
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    ))

(: run : String -> Number)
(define (run code)
  (eval(parse code)))

(test (eval(Num 4)) => 4)
(test (eval(Add(Num 3) (Num 4))) => 7)
(test (eval(Add (Sub (Num 3)(Num 2)) (Num 4))) => 5)

(test (run "4") =>  4)
(test (run "{+ 3 4}") => 7)
(test (run "{+ { - 3 2} 4}") => 5)
(test (run "{+ 1 2 3 4 }") =error> "bad sytax")
|#
;;========================================
;;Class Code Updated
;;After adding the multiplication and division operation we can perform further tests as follows
(test (parse "{* 3 { / 25 5}}") => (Mul (Num 3)
                                        (Div (Num 25)
                                             (Num 5))))

#|

Let's look on the following syntax:
{* {+ 4 2} {+ 4 2}}
It seems to be fine, but the value in it appears twice in the same way, and we would like to shorten and streamline the code

{with {x {+ 4 2}}
    {* x x }}
Advantages in naming expressions/values:
1. Efficiency, avoiding redundant computations
2. Simplicity of the code anf readability
3. Avioding code duplications, may lead to bugs.
4. Expressivness, the ability of the programmer to convey info

Now, how we evaluate 'with' expression?
{with {x {+ 4 2}}
    {with {y {* x x }}
       {+ y y}}}
>>[addition]
{with {x 6}
    {with {y {* x x }}
       {+ y y}}}
>>[substitution]
    {with {y {* 6 6 }}
       {+ y y}}
>>[Multiplication]
    {with {y 36}
       {+ y y}}
>>[substitution]
       {+ 36 36}
>>[addition]
         72

A Question, why should we use 'with' if we have 'let'?
(let ([x (+ 4 2)])
     (* x x))

They are basically the same, but 'let' is in Racket only
while in our language (WAE) we construct the equivalent of 'let' using 'with'

|# 

;;After adding the 'with' option lets test the following
(test(parse "{with {x {+ 4 2}}{* x x }}") =>(With 'x
                                                (Add (Num 4)
                                                     (Num 2))
                                                (Mul (Id 'x) (Id 'x))))
                                                     
#|
Note that we had to cancel the run function since we did not set it well to process the With command
In the next lesson we will see how to evaluate the command with
Basic idea

eval({with {x E1} E2})
1. v <- eval(E1)
2. E2' <- subst(E2, x, y)
3. eval(E2')

eval(subst E2 x (eval E1)))
|#