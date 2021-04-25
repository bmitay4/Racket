#lang pl

#| In this lesson we will start build our programming language for arithmetic expressions (AE)
   BNF = Backus-Naur Form: We will define Context-free grammar using it

Explain: <AE> = non terminal, Infinite character
         <num> = Any number in racket

  <AE>:: = <num>      (1)
       | <AE> + <AE>  (2)
       | <AE> - <AE>  (3)

Example: 7-5+3
<AE> = <AE> - <AE>
     = <AE> - <AE> + <AE>
     =  7   -  5   + 3

NOTE: In a parse tree, left-to-right actions or right-to-left actions are sometimes important and different.
We will say that grammar holds ‘ambiguity’, if there is a word for which there are two different parse trees.

Grammar with parentheses
Example: ((7+3)-1)
<AE>:: = <num>         (1)
       = (<AE> + <AE>) (2)
       = (<AE> - <AE>) (3)

         <AE>
(3)    /  |  \
   ( <AE> - <AE> )
(2) /         \
((<AE> + <AE>) - <AE>)
   /
((7 + 3) - 1))

NOTE:: We can divert the operator to the left, more convenient to understand
Example: (+ <AE> <AE> )
         (- <AE> <AE> )

In addition, we will use curly braces so that our language does not become too similar to racket
Example: {+ <AE> <AE> }
         {- <AE> <AE> }

The data structure we will use is called an Abstract Syntax Tree - AST

We would like to build the parse function so that it exists
(parse "4") => (Num 4)
(parse "{+ 3 4}") = > (Add (Num 3) (Num 4))

The goal of parse:
Input: String describing the prigram
Output: AST (Abstract Syntax Tree) or an execption if the string is not valid

Two main phases:
1. Read -- Turn the string into a simple data structure (we'll use teh Racket type Sexpr)
(Not doing any calclutaion, just get into a data structure)

Sexpr Defenition:
Basis: Any Number/Symbol is an Sexpr
General: Any List of Sexpr is an Sexpr

2. Actual Parsing -- Turn an Sexpr into an AST


|#

;;Defining th AST -- AE
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE])

(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (cond [(number? sxp) (Num sxp)]
        [(and (list? sxp)
              (= (length sxp) 3)
              (eq? (first sxp) '+))
         (Add (parse-sexpr (second sxp))
              (parse-sexpr (third sxp)))]
        [(and (list? sxp)
              (= (length sxp) 3)
              (eq? (first sxp) '-))
         (Sub (parse-sexpr (second sxp))
              (parse-sexpr (third sxp)))]
        [else (error 'parse-sexpr "bad sytax in ~s" sxp)]))

(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(test (parse "4") => (Num 4))
(test (parse "{+ 3 4}") => (Add (Num 3) (Num 4)))
(test (parse "{+ 3 { - 5 4}}") => (Add (Num 3)
                                      (Sub (Num 5)
                                           (Num 4))))
(test (parse "{+ 2 3 4 5}") =error> "bad sytax")