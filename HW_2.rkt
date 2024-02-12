#lang plait
#|
PoPL - HW 2
Besjana Kubick
Nikolay Sizov
Due: 11.02.2024
|#


(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [args : (Listof Exp)]) ; Changed it from single Exp to a Listof Exp
  (maxE [l : Exp]
        [r : Exp]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [args : (Listof Symbol)] ; Changed it from single Symbol to a Listof Symbol
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
#|
Part 2:
1. Update Data Types for 'Exp' and 'Func-Defn'
2. Udate 'parse' Function
 - It will now handle the new data types.
 - Use 's-exp-match?' with '...' (it supports zero or more repetitions)
|#

(define (parse [s : S-Exp]) : Exp 
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


;;parses a function definition
(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

#;
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{max 3 4})
        (maxE (numE 3) (numE 4)))
  (test (parse `{double 9})
        (appE 'double (numE 9)))
  (test (parse `{max {+ 4 5} {+ 2 3}})
        (maxE (plusE (numE 4) (numE 5))
              (plusE (numE 2) (numE 3)))))
  #;(test/exn (parse `{{+ 1 2}})
            "invalid input")

  #;(test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  #;(test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}}))


;; interp ----------------------------------------
#|
Helper function to interpret funciton applications
- If there aren't any more arguments to process
 - return the body
- evaluate the first argument
- recursively evaluate the rest of the arguments
- substitute the evaluated arg into the body

|#
(define (interp-helper [args : (Listof Exp)]
                       [fd-body : Exp]
                       [fd-args : (Listof Symbol)]
                       [defs : (Listof Func-Defn)]) : Exp
  (if (empty? args)
      fd-body
      (let ([eval-arg (interp (first args) defs)])
        (interp-helper (rest args)
                       (subst (numE eval-arg) (first fd-args) fd-body)
                       (rest fd-args) defs))))


(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(appE s args) (local [(define fd (get-fundef s defs))]
                    (interp (interp-helper args (fd-body fd) (fd-args fd) defs) defs))]
    [(maxE l r) (max (interp l defs) (interp r defs))]
    ))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  #;(test (interp (parse `{double 8})
                (list double-def))
        16)
  #;(test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)

  (test (interp (parse `{max 1 2}) empty)
        2)

  (test (interp (parse `{max {+ 4 5} {+ 2 3}}) empty)
        9)
  )



;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    [(appE s args) (appE s (map (lambda (arg) (subst what for arg)) args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{max y x}))
        (parse `{max y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8})))

