#lang plait
#|
Besjana Kubick
Assignment 5: Due 27.03.2024
|#


(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
   
    #|
    Case for letrec.If the s-exp is a letrec binding (recursive let), extract the binding and the body exp
    use the mk-rect-fun helper function
    recursively parse teh exp
    Example with Quasiquote Escapes from slides (pg.60, 68)
    Steps:
    1. if pattern match, evaluate and bind bs to the list of bindings in letrec exp
    2. extract a list of bindings bs from the input with name, rhs and body (1st, 2nd, 3rd)
    3. Construct a new exp. Use quasiquote. and recursively call parse on it
     lambda with name as parameter and body as body exp
     applymk-rec-fun on lambda exp with name as parameter and rhs as body exp
    |#
    [(s-exp-match? `{letrec {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))]) 
       (let ([name (first bs)] 
             [rhs (second bs)]
             [body (third (s-exp->list s))]) ;
         (parse `{{lambda {,name} ,body}
                  {,mk-rec-fun {lambda {,name} ,rhs}}})))]
    
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

;helper function (Y combinator/ fixpoint operator [{Y (lambda (f_in) f_out)}])
(define mk-rec-fun
         `{lambda {body-proc}
            {let {[fX {lambda {fX}
                        {let {[f {lambda {x}
                                   {{fX fX} x}}]}
                          {body-proc f}}}]}
              {fX fX}}})

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{+ {+ 3 4} 8})
        (plusE (plusE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE 'x (idE 'y))
              (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{if0 1 2 3})
        (if0E (numE 1) (numE 2) (numE 3)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  ;given test
  (test (interp (parse `{letrec {[f {lambda {n} 
                                      {if0 n 
                                           0 
                                           {+ {f {+ n -1}} -1}}}]} 
                          {f 10}}) 
                mt-env) 
        (numV -10))
  )

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(lamE n body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(if0E tst thn els)
     (interp (if (num-zero? (interp tst env))
                 thn
                 els)
             env)]))

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
  (test (interp (parse `{+ {+ 2 3} {+ 5 8}})
                mt-env)
        (numV 18))
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
  
  (test (interp (parse `{if0 0 2 3})
                mt-env)
        (numV 2))
  (test (interp (parse `{if0 1 2 3})
                mt-env)
        (numV 3))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{if0 {lambda {x} x} 2 3})
                    mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num-zero? [v : Value]) : Boolean
  (type-case Value v
    [(numV n) (zero? n)]
    [else (error 'interp "not a number")]))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num-zero? (numV 0))
        #t)
  (test (num-zero? (numV 1))
        #f))
#|
Part 2. Plus definition
|#
(define plus
  `{lambda {n}
     {lambda {m}
       {+ n m}}})
;given test
(module+ test
  (test (interp (parse (list->s-exp (list (list->s-exp (list plus `2)) `2))) mt-env)
        (interp (parse (list->s-exp (list `+ `2 `2))) mt-env)))

#|
Part 3. times definition
(follow implementing recursion slides)
apply times to n
return lambda that takes m
and then use letrec to define a recursive function fun that takes x
- use if0 to check if x=0, if so return 0 (base case)
- if not 0 recursive call where x is decremented by 1 and add n to the result

[n * m = n + (n * (m-1))]
|#

(define times
  `{lambda {n}
     {lambda {m}
       {letrec {[fun {lambda {x}
                       {if0 x 0
                            {+ n {fun {+ x -1}} }}}]}
         {fun m}}}})
                      

;given test (should produce the same)
(module+ test
    (test (interp (parse (list->s-exp (list (list->s-exp (list times `2)) `2)))
                mt-env)
        (numV (* 2 2))))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
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
        (numV 8)))
