#lang pl


#| Our Grammer

   <FLANG> ::= <num>                         (1)
            {+ <FLANG> <FLANG>}              (2)
            {- <FLANG> <FLANG>}              (3)
            {* <FLANG> <FLANG>}              (4)
            {/ <FLANG> <FLANG>}              (5)
            {with { <id> <FLANG>} <FLANG> }  (6)
            <id>                             (7)
            { fun { <id } <FLANG> }          (8)
            { call <FLANG> <FLANG> }         (9)

Where <num> stands for any Racket number, and <id> for any Racket symbole

|#

;;Defining the AST -- FLANG
(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [With Symbol FLANG FLANG]
  [Id Symbol]
  [Fun Symbol FLANG]  ;; parameter, body
  [Call FLANG FLANG]) ;; Func-expression, argument

;;Each constructor called Variant

#|
Remainder of Sexpr Defenition:
Basis: Any Number/Symbol is an Sexpr
General: Any List of Sexpr is an Sexpr
|#

(: parse-sexpr : Sexpr -> FLANG)
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
    [(cons 'fun more)
     (match sxp
       [(list 'fun (list(symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad fun syntax in ~s" sxp)])]
    [(list 'call fun-exp arg-exp) (Call (parse-sexpr fun-exp)(parse-sexpr arg-exp))]
    [else (error 'parse-sexpr "bad sytax in ~s" sxp)]))   ;; We can write anything instead of 'else' 

;;Sends strings to parse-sexpr and returning them as a FLANG expression
(: parse : String -> FLANG)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(test (parse "{fun {x} x}")
      => (Fun 'x (Id 'x)))

(test (parse "{fun {x} {/ x 5}}")
      => (Fun 'x (Div (Id 'x)
                      (Num 5))))

(test (parse "{call {fun {x} {/ x 5}} 8}")
      => (Call (Fun 'x (Div (Id 'x)
                            (Num 5)))
               (Num 8)))

(test (parse "{with {sqr {fun {x} {* x x}}}
                  {+ {call sqr 5}
                     {call sqr 6}}}")
      => (With 'sqr
               (Fun 'x (Mul (Id 'x) (Id 'x)))
               (Add (Call (Id 'sqr) (Num 5))
                    (Call (Id 'sqr) (Num 6)))))

#|
Keep in mind
1. Binding Instance:  binding instance of an identifier is one that is used to name it in a new binding. in our <FLANG> syntax,
   binding instances are only the <id> position of the 'with form

2. Scope: the scope of a binding instance is the region of program text in which instance of the identifier refer to the value bound in the
   binding insatnce.

3. Bound Instance: An instance of an identifier is bound if it is contanined within the scope of a bnding instance of its name.

4. Free Instance: An identifier that is not contained in the scope of any binding instance of its name is said to be free.
|#

#|
subst will substitutes the second argument with the third argument in the first argument as per the rules of the substitution.
The resulting expression contains no free instances of the second argument

N[v/x] = N
{+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
{- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
{* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
{/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}

y[v/x] = y
x[v/x] = v

{with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
{with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}

{call E1 E2} = {call E1[v/x] E2[v/x]}

{fun {y} E[v/x]} = {fun {y} E[v/x]}
|#

(: subst : FLANG Symbol FLANG -> FLANG)  
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
    [(Id name) (if (eq? name from) to expr)]
    [(Fun name body)
     (Fun name
          (if (eq? name from) body
              (subst body from to)))]
    [(Call fun-exp arg-exp)
     (Call (subst fun-exp from to)
           (subst arg-exp from to))]))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'x
             (Num 4))
      => (Fun 'x (Add (Id 'x) (Id 'y))))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'y
             (Num 4)) => (Fun 'x (Add (Id 'x) (Num 4))))

(test (subst (Call (Fun 'x (Div (Id 'x)
                                (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
             'x
             (Num 3))
      => (Call (Fun 'x (Div (Id 'x)
                            (Id 'y)))
               (Add (Num 3) (Id 'y))))

(test (subst (Call (Fun 'x (Div (Id 'x)
                                (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
             'y
             (Num 3))
      => (Call (Fun 'x (Div (Id 'x)
                            (Num 3)))
               (Add (Id 'x) (Num 3))))

(test (subst (Add (Call (Id 'sqr) (Num 5))
                  (Call (Id 'sqr) (Num 6)))
             'sqr
             (Fun 'x (Mul (Id 'x) (Id 'x))))
      => (Add (Call (Fun 'x (Mul (Id 'x) (Id 'x))) (Num 5))
              (Call (Fun 'x (Mul (Id 'x) (Id 'x))) (Num 6))))

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
(2) E2' <- (subst (Mul (Id 'x) (Id 'x)) 'x (Num 8)) ;;NOTE! it can't be just 8, because the third element of subst is FLANG, not <num>
(3) (eval (Mul (Num 8) (Num 8)))

eval(N) = N

eval({+ E1 E2}) = eval(E1) + eval(E2)

eval({- E1 E2}) = eval(E1) - eval(E2)

eval({* E1 E2}) = eval(E1) * eval(E2)

eval({/ E1 E2}) = eval(E1) / eval(E2)

eval(id) = ERROR!

eval({with {x E1} E2}) = eval(E2 [eval(E1)/x])

eval({fun {x} E}) = (fun {x} E)

eval({call E1 E2}) = if {fun {x} Ef} <-- eval(E1)
                         eval(Ef[eval(E2)/x])
                     otherwise ERROR!

|#

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
(define (arith-op op arg1 arg2)
  (: Num->Number : FLANG -> Number)
  (define (Num->Number arg)
    (cases arg
      [(Num n) n]
      [else (error 'Num->Number "expects a number, got: ~s" arg)]))
  (Num (op (Num->Number arg1) (Num->Number arg2))))


(: eval : FLANG -> FLANG)
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With name named-expr body)
     (eval (subst body
                  name
                  (eval named-expr)))]
    [(Id name) (error 'eval "free instance: ~s" name)]
    [(Fun name body) expr]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr)])
       (cases fval
         [(Fun name body)
          (eval (subst body name (eval arg-expr)))]
         [else (error 'eval "expect a function, got: ~s" fval)]))]))
      

#|(: run : String -> FLANG)
(define (run code)
  (eval(parse code)))
|#

(test (eval (With 'x
                  (Add (Num 5) (Num 3))
                  (Mul (Id 'x) (Id 'x)))) => (Num 64))
(test (eval (With 'x
                  (Add (Num 5) (Num 3))
                  (Mul (Id 'x) (Id 'y)))) =error> "free instance")
;;(test (eval "{with {x 5}
;;                  {+ 5 {with {x 3} x}}}") => (Num 8))



(: run : String -> Number)
(define (run code)
  (let ([res (eval(parse code))])
    (cases res
      [(Num n) n]
      [else (error 'run "evaluation returned a non-number: ~s" res)])))

(test (run "{with {x 5}
                  {+ 5 {with {x 3} x}}}") => 8)
;;(test (run "{with {fun {x} {+ x 1}} 4}") => 5)
(test (run "{call {with {foo {fun {y} {- y 6}}}
                         foo}
                         10}") => 4)

