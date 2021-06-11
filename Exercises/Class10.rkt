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


#| Evaluation 

eval(N, sc) = N

eval({+ E1 E2}, sc) = eval(E1, sc) + eval(E2, sc)
                      if eval(E1) and eval(E2) return numbers
                      otherwise Error

eval({- E1 E2}, sc) = eval(E1, sc) - eval(E2, sc)
                      if eval(E1) and eval(E2) return numbers
                      otherwise Error

eval({* E1 E2}, sc) = eval(E1, sc) * eval(E2, sc)
                      if eval(E1) and eval(E2) return numbers
                      otherwise Error

eval({/ E1 E2}, sc) = eval(E1, sc) / eval(E2, sc)
                      if eval(E1) and eval(E2) return numbers
                      otherwise Error

eval(id) = lookup(id, sc)

eval({with {x E1} E2}, sc) = eval(E2, extends(x, eval(E1, sc), sc))

eval({fun {x} E}, sc) = (fun {x} E)

eval({call E1 E2}, sc) = if {fun {x} Ef} <-- eval(E1, sc)
                         eval(Ef, extend(x, eval(E2, sc), sc))
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


(: eval : FLANG SubstCache -> FLANG)
(define (eval expr sc)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l sc) (eval r sc))]
    [(Sub l r) (arith-op - (eval l sc) (eval r sc))]
    [(Mul l r) (arith-op * (eval l sc) (eval r sc))]
    [(Div l r) (arith-op / (eval l sc) (eval r sc))]
    [(With name named-expr body)
     (eval body (extend name
                  (eval named-expr sc)
                  sc))]
    [(Id name) (lookup name sc)]
    [(Fun name body) expr]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr sc)])
       (cases fval
         [(Fun name body)
          (eval body
                (extend name (eval arg-expr sc) sc))]
         [else (error 'eval "expect a function, got: ~s" fval)]))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Substitution Cache
;; A type for substitution cache
(define-type SubstCache = (Listof (List Symbol FLANG)))

(: empty-subst : SubstCache)
(define empty-subst null)

(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend id expr sc)
  (cons (list id expr) sc))

(: lookup : Symbol SubstCache -> FLANG)
(define (lookup name sc)
  (let ([cell (assq name sc)])
    (if cell
        (second cell)
        (error 'lookup "free identifier ~s" name))))

;;assq - internal function at Racket which get a value and a list, and look for the
;;value in within the list.
;;If not found, return #f


(: run : String -> Number)
(define (run code)
  (let ([res (eval(parse code) empty-subst)])
    (cases res
      [(Num n) n]
      [else (error 'run "evaluation returned a non-number: ~s" res)])))
#|
(test (run "{with {x 5}
                  {+ 5 {with {x 3} x}}}") => 8)
(test (run "{with {fun {x} {+ x 1}} 4}") =error> "bad with syntax in")
(test (run "{call {with {foo {fun {y} {- y 6}}}
                         foo}
                         10}") => 4)
|#

(test (lookup 'x
              (extend 'y
                      (Num 5)
                      (extend 'x
                              (Num 33)
                              (extend 'foo
                                      (Fun 'x (Id 'x))
                                      (extend 'w (Num 5) empty-subst)))))
        => (Num 33))

(test (lookup 'x
              (extend 'x (Num 0)
                         (extend 'z (Num 5)
                                    (extend 'x (Num 7)
                                               (extend 'foo (Fun 'x (Id 'y))
                                                       empty-subst)))))
      => (Num 0))
(test (lookup 'foo
              (extend 'x (Num 0)
                         (extend 'z (Num 5)
                                    (extend 'x (Num 7)
                                               (extend 'foo (Fun 'x (Id 'y))
                                                       empty-subst)))))
      => (Fun 'x (Id 'y)))

(test (lookup 'foo
              (extend 'y
                      (Num 5)
                      (extend 'x
                              (Num 33)
                              (extend 'foo
                                      (Fun 'x (Id 'x))
                                      (extend 'w (Num 5) empty-subst)))))
        => (Fun 'x (Id 'x)))
(test (lookup 'f
              (extend 'y
                      (Num 5)
                      (extend 'x
                              (Num 33)
                              (extend 'foo
                                      (Fun 'x (Id 'x))
                                      (extend 'w (Num 5) empty-subst)))))
        =error> "free identifier")