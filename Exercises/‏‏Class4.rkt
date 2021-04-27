#lang pl

#| In this lesson we will continue build our programming language for arithmetic expressions (AE)

  <AE>:: = <num>      (1)
       | <AE> + <AE>  (2)
       | <AE> - <AE>  (3)

NOTE:: We can divert the operator to the left, more convenient to understand
Example: (+ <AE> <AE> )
         (- <AE> <AE> )

In addition, we will use curly braces so that our language does not become too similar to racket
Example: {+ <AE> <AE> }
         {- <AE> <AE> }

The data structure we will use is called an Abstract Syntax Tree - AST


|#

;;Defining th AST -- AE
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE])

(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))] ;; We just ask if we holding a list, which the first value is '+, and has two more values (3 in total)
    [(list '- l r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad sytax in ~s" sxp)]))   ;; We can write anything instead of 'else' 
    
(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(test (parse "4") => (Num 4))
(test (parse "{+ 3 4}") => (Add (Num 3) (Num 4)))
(test (parse "{+ 3 { - 5 4}}") => (Add (Num 3)
                                       (Sub (Num 5)
                                            (Num 4))))
(test (parse "{+ 2 3 4 5}") =error> "bad sytax")


;;Eval
#|
<AE> ::= <num>            ; a
       | <AE> + <AE>      ; b
       | <AE> - <AE>      ; c


eval("<num>") = <num>
eval("E1 + E2") = eval(E1) + eval(E2)
eval("E1 - E2") = eval(E1) - eval(E2)


eval(1 - 2 + 3) = eval(1) - eval(2 + 3)         [c]
                = eval(1) - (eval(2) + eval(3)) [a]
                = 1 - (2 + 3)                   [a,a,a]
                = -4

eval(1 - 2 + 3) = eval(1 - 2) + eval(3)          [b]
                = (eval(1) - eval(2)) + eval(3)) [c]
                = (1 - 2) + 3                    [a,a,a]
                = 2

That's old style.
Now we're using the following:

<AE> ::= <num>
         {+ <AE> <AE> }
         {- <AE> <AE> }

And so, lets check again the eval function
eval("<num>") = <num>
eval("{+ E1 E2}") = eval(E1) + eval(E2)
eval("{- E1 E2}") = eval(E1) - eval(E2)

E1 = {+ {- 3 2} 7}
E1 = 8

Compositionality - we don't care the shape or the method of the function, just the ending eval

|#

(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n] 
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
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

