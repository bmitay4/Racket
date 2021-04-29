#lang pl

;;We'll contiunie establish our AE language

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
 
(test (parse "{with {x {+ 4 2}}{* x x }}") =>(With 'x
                                                   (Add (Num 4)(Num 2))
                                                   (Mul (Id 'x) (Id 'x))))

#| Evaluation of with

eval({with {x E1} E2})
1. v <- eval(E1)
2. E2' <- subst(E2, x, y)
3. eval(E2')

eval(subst E2 x (eval E1)))

1. Binding Instance:  binding instance of an identifier is one that is used to name it in a new binding. in our <WAE> syntax,
   binding instances are only the <id> position of the 'with form
2. Scope: the scope of a binding instance is the region of program text in which instance of the identifier refer to the value bound in the
   binding insatnce.
3. Bound Instance: An instance of an identifier is bound if it is contanined within the scope of a bnding instance of its name.
4. Free Instance: An identifier that is not contained in the scope of any binding instance of its name is said to be free.

Therefore, we shall say the following regards the 'with command:
To substitude an identifier 'i' in an expression 'e' with an expression 'v', replace all the instance of 'i' that are free in 'e' with the
expression 'v'.
|#

;; Now we'll define and build our subst function
#|
subst will substitutes the second argument with the third argument in the first argument as per the rules of the substitution.
Thr resulting expression contains no free instances of the second argument
|#
(: subst : WAE Symbol WAE -> WAE)   ;returns expr[to/from]
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
    [(Id name) (if (eq? name from) to expr)])
  )

(test(subst(Mul (Id 'x) (Id 'x))
           'x
           (Num 6)) => (Mul (Num 6) (Num 6)))

(test(subst(Mul (Id 'x) (Id 'x))
           'x
           (Num 6)) => (Mul (Num 6) (Num 6)))

(test(subst(With 'y
                 (Add (Id 'x) (Num 4))
                 (Add (Id 'x) (Id 'y)))
           'x
           (Num 6)) => (With 'y
                             (Add (Num 6) (Num 4))
                             (Add (Num 6) (Id 'y))))
(test(subst (Id 'y)
            'x
            (Num 6)) => (Id 'y))