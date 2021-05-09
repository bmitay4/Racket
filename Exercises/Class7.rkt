#lang pl

#| Our Grammer

   <WAE> ::= <num>                       (1)
            {+ <WAE> <WAE>}              (2)
            {- <WAE> <WAE>}              (3)
            {* <WAE> <WAE>}              (4)
            {/ <WAE> <WAE>}              (5)
            {with { <id> <WAE>} <WAE> }  (6)
            <id>                         (7)

Where <num> stands for any Racket number, and <id> for any Racket symbole

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

;;Each constructor called Variant

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

;;Sends strings to parse-sexpr and returning them as a WAE expression
(: parse : String -> WAE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

#|
Keep in mind
1. Binding Instance:  binding instance of an identifier is one that is used to name it in a new binding. in our <WAE> syntax,
   binding instances are only the <id> position of the 'with form

2. Scope: the scope of a binding instance is the region of program text in which instance of the identifier refer to the value bound in the
   binding insatnce.

3. Bound Instance: An instance of an identifier is bound if it is contanined within the scope of a bnding instance of its name.

4. Free Instance: An identifier that is not contained in the scope of any binding instance of its name is said to be free.
|#

;;subst will substitutes the second argument with the third argument in the first argument as per the rules of the substitution.
;;The resulting expression contains no free instances of the second argument

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

#| Evaluation of with

eval({with {x E1} E2})
1. v <- eval(E1)
2. E2' <- subst(E2, x, y)
3. eval(E2')

eval(subst E2 x (eval E1)))

Consider the following example:
(eval (With 'x
            (Add (Num 5) (Num 3))
            (Mul (Id 'x) (Id 'x)))

(1) v <- (eval (Add (Num 5) (Num 3)))
(2) E2' <- (subst (Mul (Id 'x) (Id 'x)) 'x (Num 8)) ;;NOTE! it can't be just 8, because the third element of subst is WAE, not <num>
(3) (eval (Mul (Num 8) (Num 8)))
|#

(: eval : WAE -> Number)
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(With name named-expr body)
     (eval (subst body name (Num (eval named-expr))))]
    [(Id name) (error 'eval "free instance: ~s" name)]))
      

(: run : String -> Number)
(define (run code)
  (eval(parse code)))
   
(test (eval (With 'x
                  (Add (Num 5) (Num 3))
                  (Mul (Id 'x) (Id 'x)))) => 64)
(test (eval (With 'x
                  (Add (Num 5) (Num 3))
                  (Mul (Id 'x) (Id 'y)))) =error> "free instance")
(test (run "{with {x 5}
                  {+ 5 {with {x 3} x}}}") => 8)

#|
We now carry out an anonymous function, similar to Lambda, which performs a multiplication operation itself (square)
(lambda (x)
        (* x x))
      5)
     ->

We will define the fun function with its body, and use the call function to implement it on the given value. In this example it is 5
{call {fun {x}       ; x is formal argument
        {* x x}}
       5}            ; 5 is the actual argument
|#