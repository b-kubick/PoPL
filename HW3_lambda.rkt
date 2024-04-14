#lang plait
#|
Besjana Kubick
Homework 3
Due: 02/21/2024
|#

#|
Define the values that the program can produce.
closV - closures are funx values that capture the arg, body and environment where the funx was defined
Adding Boolean: extend Exp with new kind of expressions and add booleans to Value
|#
(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boolV [b : Boolean])
  (thunkV [body : Exp]
          [env : Env]))

#|
Define the exp that the language supports
lamE - lambda expressions (lam introduces new functions and application eliminates them)
appE - application expressions
letE -
{let {[<Symbol> <Exp>]} <Exp>} (n rhs body)
Video for let - Plait Binding 1 & 2 - let and parsing let - Book pg.53
x value is binded to the nearest x
static scoping
A variable's binding is determined by its position in the program and not by the order of execution

unlet - hides the nearest visible binding. (think scope in a function) but lets other bindings through
|#
(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  
  (trueE)
  (falseE)
  (eqE [l : Exp] ;for equality tests
       [r : Exp])
  (ifE [tst : Exp] ; for conditional forms
       [thn : Exp]
       [els : Exp])

  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (unletE [n : Symbol]
          [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (delayE [rhs : Exp])
  (forceE [rhs : Exp]))

#|
Define binding as a pair of a name and a value. Associates the name with the value.
E.x.: For instance, when we call a function, the act of calling associates (“binds”) the formal parameters with the actual values.
|#
(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

#|
environment is a list of bindings, represents the current state of bindings
|#
(define-type-alias Env (Listof Binding)) 

(define mt-env empty) ; it instantiates an empty environment
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
#|
Converts the S-expressions (human-readable form) into the internal 'Exp' representations.
|#
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    

    [(s-exp-match? `true s) (trueE)] ;parsing boolean (true)
    [(s-exp-match? `false s) (falseE)] ;;parsing boolean (false)
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]

  
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;parse eqE
    [(s-exp-match? `{= ANY ANY} s)
     (eqE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    ;parse ifE
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    ;parsing let
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    ; parse unlet
    [(s-exp-match? `{unlet ANY ANY} s)
     (unletE (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{force ANY} s)
     (forceE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{delay ANY} s)
     (delayE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `true) (trueE))
  (test (parse `false)(falseE))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; tests for boolean
(module+ test
  (test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse `9)
                mt-env))
  (test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse `10)
                mt-env))
  (test/exn (interp (parse `{if 1 2 3})
                    mt-env)
            "not a boolean"))

;; interp ----------------------------------------
#|
Could I just make a helper funx to evaluate bool-op? like the num-op? Ask Nate
Can I make a num= ? Ask Nate
|#
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(eqE l r) (num= (interp l env) (interp r env))]
    [(trueE) (boolV #t)]
    [(falseE) (boolV #f)]

    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    
    [(ifE tst thn els) (type-case Value (interp tst env)
                         [(boolV v) (if v
                                        (interp thn env)
                                        (interp els env))]
                         [else (error 'if "not a boolean")])]
    
    ; interp let
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]

    ;interp unlet
    [(unletE n body) (interp body (unbind n env))]
    ;delayE    
    [(delayE rhs) (thunkV rhs env)]
    ;forceE
    [(forceE rhs) (type-case Value (interp rhs env)
                    [(thunkV rhs env) (interp rhs env)]
                    [else (error 'interp "not a thunk")])]))

(define (unbind [n : Symbol] [env : Env]) : Env
  (type-case (Listof Binding) env
    [empty (error 'unbind "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name (first env))) (rest env)]
                        [else (extend-env (first env) (unbind n (rest env)))])]))

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  ;unlet
  ;; Test unlet after a variable has been bound and then unbound
  (test (interp (parse `{let {[x 10]}
                          {unlet x
                                 {+ x 2}}})
                (extend-env (bind 'x (numV 1)) mt-env))
        (numV 3))  

  (test/exn (interp (parse `{unlet x x}) mt-env)
          "free variable")
  

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;; num+ and num* ----------------------------------------
#|
Helper functions for arithmetic operations
to make sure both operands are numeric values (numV) before it applyes the operation
make a num= to help evaluate eqE --> cleaner
|#
(define (num-op op l r)
  (cond
   [(and (numV? l) (numV? r))
     (op (numV-n l) (numV-n r))]
   [else
    (error 'interp "not a number")]))

(define (num+ [l : Value] [r : Value]) : Value
  (numV (num-op + l r)))
(define (num* [l : Value] [r : Value]) : Value
  (numV (num-op * l r)))
(define (num= [l : Value] [r : Value]) : Value
  (boolV (num-op = l r)))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
#|
Environment lookup: it searches the environment for a binding by name and returns its value. If the name is not found then it signals an error
Don't forget to test for errors
|#
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (unbind 'x mt-env)
            "free variable")
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8))
  (test/exn (interp (parse `{let {[x 1]}
                              {unlet x
                                     x}})
                    mt-env)
            "free variable")
  (test (interp (parse `{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env)
        (interp (parse `3) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env)
        (interp (parse `6) mt-env))
  (test (interp (parse `{let {[f {lambda {z}
                                   {let {[z 8]}
                                     {unlet z
                                            z}}}]}
                          {f 2}})
                mt-env)
        (interp (parse `2) mt-env))

  (test/exn (interp (parse `{force 1})
                    mt-env)
            "not a thunk")
  (test (interp (parse `{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse `15)
                mt-env)))
