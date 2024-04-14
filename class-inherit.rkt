#lang plait
#|
Besjana Kubick
Nikolay Sizov

Homework 7 - Object and Classes

|#


#|
defining expressions in oo-lang
|#
(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol]
        [args : (Listof Exp)])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (selectE [cond-expr : Exp] ; part 2
           [obj-expr : Exp])
  (instanceofE [obj-expr : Exp] ; part 3
               [class-name : Symbol]))

#|
defines class definition
|#
(define-type Class
  (classC [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * Exp))]
          [super-name : Symbol]))
#|
values that expresions can evaluate to. has constructors for numbers and obj (w/ class name and field values)
|#
(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof Value)]))

(module+ test
  (print-only-errors #t))

;; ------------------------------------------------------------------ 
#|
helper function that takes a list of pairs (symbol and value) and a symbol
returns the corresponding value associated with the symbol. It raises an error if the symbol is not found
|#

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a) 
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------
#|
newE: creates a new obj by finding the class definition, evaluating the field exp, and constructing an objV value with the class name and field value

getE: Evaluates the obj expression, finds the corresponding class definition and gets the field value for the given field name

sendE: eval the obj expression, finds the class def, invokes the method with the given method name

ssendE: like sendE but specifies the class name for the super method invocation

|#
(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        
         ;-------- Part 1   
        [(newE class-name field-exprs)   
         (local [(define c (if (equal? class-name 'Object)
                               (classC empty empty 'Object)
                               (find classes class-name)))
                 (define vals (map recur field-exprs))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        
        [(getE obj-expr field-name)  
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC field-names methods super-name)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           field-vals)
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]

        ;;-----part 2 add select
        [(selectE cond-expr obj-expr)
         (local [(define cond-val (recur cond-expr))
                 (define obj-val (recur obj-expr))]
           (cond
             [(and (numV? cond-val) (objV? obj-val))
              (if (zero? (numV-n cond-val))
                  (call-method (objV-class-name obj-val) 'zero classes obj-val (numV 0))
                  (call-method (objV-class-name obj-val) 'nonzero classes obj-val (numV 0)))]
             [(not (numV? cond-val))
              (error 'interp "not a number")]
             [else
              (error 'interp "not an object")]))]

        ;;--------part 3 instanceOf
        [(instanceofE obj-expr super-name)
         (type-case Value (recur obj-expr)
           [(objV obj-class-name _)
            (if (eq? obj-class-name super-name )
                (numV 0)
                (numV (subclass obj-class-name super-name classes)))]
           [else
            (error 'interp "not an object")])]))))

(define (subclass obj-name super-name classes)
  (cond
    [(eq? obj-name super-name) 1]
    [(eq? obj-name 'Object) 0]
    [else
     (let
         [(super-name (classC-super-name (find classes super-name)))]
       (subclass obj-name super-name classes))]
    ))

#;(define (subclass? obj-name class-name classes)
  (cond
    [(equal? obj-name class-name) #t]
    [(equal? obj-name 'Object) #f]
    [else
     (type-case Class (find classes obj-name)
       [(classC field-names methods super-name)
        (subclass? super-name class-name classes)])]))

#|
helper function for the interp. used to invoke a method on an obj.
|#
(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC field-names methods super-name)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples to test the class definitions for Posn

(module+ test
  (define posn-class
    (values 'Posn
            (classC 
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2)))))
             'Object)))
    
  (define posn3D-class
    (values 'Posn3D
            (classC 
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE))))
             'Object)))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count")
  (test (interp (newE 'Object empty)
                empty
                (objV 'Object empty)
                (numV 0))
        (objV 'Object empty)))
#|
- continuation of class.rkt
- extends the language with inheritance and uses an intermediate representation (ExpI and ClassI) and uses superI to invoke superclass methods
|#


#|
new data-type
represents expressions in the extended oo-lang w/ inheritance
uses superI to invoke superclass methods
|#
(define-type ExpI
  (numI [n : Number])
  (plusI [lhs : ExpI]
         [rhs : ExpI])
  (multI [lhs : ExpI]
         [rhs : ExpI])
  (argI)
  (thisI)
  (newI [class-name : Symbol]
        [args : (Listof ExpI)])
  (getI [obj-expr : ExpI]
        [field-name : Symbol])
  (sendI [obj-expr : ExpI]
         [method-name : Symbol]
         [arg-expr : ExpI])
  (superI [method-name : Symbol]
          [arg-expr : ExpI])
  (selectI [cond-expr : ExpI]
           [obj-expr : ExpI])
  (instanceofI [obj-expr : ExpI]
               [class-name : Symbol]))

#|
Class definition w/ inheritance and includes:
- superclass name
- list of field names
- list of methods where each method is a pair of a method name and an ExpI 
|#
(define-type ClassI
  (classI [super-name : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * ExpI))]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

#|
Converts an ExpI to the corresponding Exp.
Recursively converts sub-exp and handles the superI case by converting it to an ssendE exp with the superclass name
|#
(define (exp-i->c [a : ExpI] [super-name : Symbol]) : Exp
  (local [(define (recur expr)
            (exp-i->c expr super-name))]
    (type-case ExpI a
      [(numI n) (numE n)]
      [(plusI l r) (plusE (recur l) (recur r))]
      [(multI l r) (multE (recur l) (recur r))]
      [(argI) (argE)]
      [(thisI) (thisE)]
      [(newI class-name field-exprs)
       (newE class-name (map recur field-exprs))]
      [(getI expr field-name)
       (getE (recur expr) field-name)]
      [(sendI expr method-name arg-expr)
       (sendE (recur expr)
              method-name
              (recur arg-expr))]
      [(superI method-name arg-expr)
       (ssendE (thisE)
               super-name
               method-name
               (recur arg-expr))]
      [(selectI cond-expr obj-expr)
       (selectE (recur cond-expr)
                (recur obj-expr))]
      [(instanceofI obj-expr class-name)
       (instanceofE (recur obj-expr) class-name)])))

(module+ test
  (test (exp-i->c (numI 10) 'Object)
        (numE 10))
  (test (exp-i->c (plusI (numI 10) (numI 2)) 'Object)
        (plusE (numE 10) (numE 2)))
  (test (exp-i->c (multI (numI 10) (numI 2)) 'Object)
        (multE (numE 10) (numE 2)))
  (test (exp-i->c (argI) 'Object)
        (argE))
  (test (exp-i->c (thisI) 'Object)
        (thisE))
  (test (exp-i->c (newI 'Object (list (numI 1))) 'Object)
        (newE 'Object (list (numE 1))))
  (test (exp-i->c (getI (numI 1) 'x) 'Object)
        (getE (numE 1) 'x))
  (test (exp-i->c (sendI (numI 1) 'mdist (numI 2)) 'Object)
        (sendE (numE 1) 'mdist (numE 2)))
  (test (exp-i->c (superI 'mdist (numI 2)) 'Posn)
        (ssendE (thisE) 'Posn 'mdist (numE 2))))

;; ----------------------------------------

#|
- Usually when an obj of the subclass is created, it contains its own fields and the ones from the superclass. Here ClassI is converted to a Class def without flattening, meaning the resulting Class def only contains the fields and methods directly defined in the class without including the ingerited fields and methods from its superclass.
- It converts the method expressions using exp-i->c
|#
(define (class-i->c-not-flat [c : ClassI]) : Class
  (type-case ClassI c
    [(classI super-name field-names methods)
     (classC
      field-names
      (map (lambda (m)
             (values (fst m)
                     (exp-i->c (snd m) super-name)))
           methods)
      super-name)]))

(module+ test
  (define posn3d-mdist-i-method
    (values 'mdist
            (plusI (getI (thisI) 'z)
                   (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (values 'mdist
            (plusE (getE (thisE) 'z)
                   (ssendE (thisE) 'Posn 'mdist (argE)))))

  (define posn3d-i-class 
    (values 'Posn3D
            (classI
             'Posn
             (list 'z)
             (list posn3d-mdist-i-method))))
  (define posn3d-c-class-not-flat
    (values 'Posn3D
            (classC (list 'z)
                    (list posn3d-mdist-c-method)
                    'Posn)))

  (test (class-i->c-not-flat (snd posn3d-i-class))
        (snd posn3d-c-class-not-flat)))

;; ----------------------------------------
#|
flattens a class definition by merging the fields and methods from its supperclasses.
Recursively flattens the superclasses using flatten-super and combines the fields and methods using add-fields and add/replace-methods (they are below)

To flatten: The process of merging the fields and methods of a class with those ingerited from its superclasses to create a single, self-contained class definition
|#
(define (flatten-class [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case Class (find classes-not-flat name)
    [(classC field-names methods super-name)
     (type-case Class (flatten-super name classes-not-flat i-classes)
       [(classC super-field-names super-methods super-name)
        (classC
         (add-fields super-field-names field-names)
         (add/replace-methods super-methods methods) super-name)])]))
#|
retrieves the superclass definition of a given class from the list of ClassI definitions and flattens it recursively
|#
(define (flatten-super [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case ClassI (find i-classes name)
    [(classI super-name field-names i-methods)
     (if (equal? super-name 'Object)
         (classC empty empty 'Object)
         (flatten-class super-name
                        classes-not-flat
                        i-classes))]))

(module+ test
  (define posn-i-class
    (values
     'Posn
     (classI 'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                   (values 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0))))))))
  (define addDist-c-method
    (values 'addDist
            (plusE (sendE (thisE) 'mdist (numE 0))
                   (sendE (argE) 'mdist (numE 0)))))
  (define posn-c-class-not-flat
    (values
     'Posn
    (classC (list 'x 'y)
            (list (values 'mdist
                          (plusE (getE (thisE) 'x)
                                 (getE (thisE) 'y)))
                  addDist-c-method)
            'Object)))
  (define posn3d-c-class
    (values 'Posn3D
            (classC (list 'x 'y 'z)
                    (list posn3d-mdist-c-method
                          addDist-c-method)
                    'Object)))

  (test (flatten-class 'Posn3D
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        (snd posn3d-c-class)))

;; ----------------------------------------
#| helper func that appends two lists of field names|#
(define add-fields append)

#|
- Helper func that merges two lists of methods (takes them as input).
- If a method with the same name exists in both lists, the method from the second list replaces the one from the first list
- Returns the merged list of methods
- Is called recursively to handle merging multiple methods
|#

(define (add/replace-methods [methods : (Listof (Symbol * Exp))]
                             [new-methods : (Listof (Symbol * Exp))])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

#|
- Used in the flattening process
- Takes a list of methods and a single method (new-method) as new input
- it adds or replaces the new-method in the methods list. If a method with the same name exists in both lists, the method from the second list replaces the one from the first list, if not then it appends new-method to the end of methods
- Returns the updated list of methods
|#
(define (add/replace-method [methods : (Listof (Symbol * Exp))] 
                            [new-method : (Symbol * Exp)])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (fst (first methods))
                 (fst new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (values 'm (numE 0))))
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0))) empty)
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))))
  (test (add/replace-methods (list (values 'm (numE 0))
                                   (values 'n (numE 2)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))
                                   (values 'n (numE 2))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))

  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'm (numE 1)))
        (list (values 'm (numE 1))))
  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'n (numE 2)))
        (list (values 'm (numE 0))
              (values 'n (numE 2)))))

;; ----------------------------------------
#|
- Interpreter func for the language with inheritance. Takes an ExpI expression and a list of ClassI definitions.
- Converts the ExpI to an Exp using exp-i->c, and converts ClassI to Class using class-i->c-not-flat,
- flattens teh class definitions using flatten-class
- interprets the Exp using interp from class.rkt
|#
(define (interp-i [i-a : ExpI] [i-classes : (Listof (Symbol * ClassI))]) : Value
  (local [(define a (exp-i->c i-a 'Object))
          (define classes-not-flat
            (map (lambda (i)
                   (values (fst i)
                           (class-i->c-not-flat (snd i))))
                 i-classes))
          (define classes
            (map (lambda (c)
                   (let ([name (fst c)])
                     (values name
                             (flatten-class name classes-not-flat i-classes))))
                 classes-not-flat))]
    (interp a classes (objV 'Object empty) (numV 0))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'Posn3D (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'Posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18)))



(module+ test
  (print-only-errors #t))

#|
Extension of oo-lang interpreter. It gives a parser for the language so that the program can be written using S-expressions instead of using constructors of the ExpI and ClassI data types.
It adds a parsing layer to oo-langinterpreter.
|#

;; ----------------------------------------

#|
takes an S-expression representing a class definition and converts it to a ClassI value. It extracts the class name, superclass name, field names and method definition from the S-exp and constructs a ClassI value using the classI constructor
|#

(define (parse-class [s : S-Exp]) : (Symbol * ClassI)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values (s-exp->symbol (second (s-exp->list s)))
             (classI
              (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
   [else (error 'parse-class "invalid input")]))

;; takes an S-exp representing a field name and converts into a symbol
(define (parse-field [s : S-Exp]) : Symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

;; takes an S-exp representing a method def and converts it into a pair of the method name (as a symbol) and the parsed method body (as an ExpI value)
(define (parse-method [s : S-Exp]) : (Symbol * ExpI)
  (cond
   [(s-exp-match? `[SYMBOL {arg} ANY] s)
    (values (s-exp->symbol (first (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

#|
- Takes an S-exp representing an exp and converts it to an ExpI value. Recursively parses subexpressions. Handles cases: 
- obj creation (new)
- field access (get)
- method invocation (send)
- superclass method invocation

|#
(define (parse [s : S-Exp]) : ExpI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? `{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? `{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? `{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [(s-exp-match? `{select ANY ANY} s)
    (selectI (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
   [(s-exp-match? `{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse `{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse `{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse `{new Posn 1 2})
        (newI 'Posn (list (numI 1) (numI 2))))
  (test (parse `{get 1 x})
        (getI (numI 1) 'x))
  (test (parse `{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse `{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field `{x 1})
            "invalid input")

  (test (parse-method `[m {arg} this])
        (values 'm (thisI)))
  (test/exn (parse-method `[m {arg} 1 2])
            "invalid input")
  
  (test (parse-class `{class Posn3D extends Posn
                        {x y z}
                        [m1 {arg} arg]
                        [m2 {arg} this]})
        (values 'Posn3D
                (classI 'Posn
                        (list 'x 'y 'z)
                        (list (values 'm1 (argI))
                              (values 'm2 (thisI))))))
  (test/exn (parse-class `{class})
            "invalid input"))

;; ----------------------------------------
#|
Takes a list of S-exp representing class definitions and an S-exp representing the main exp to be evaluated. It parses the class definions using parse-class and the main exp using parse.
Then it interprets the parsed exp using interp-i.
It converts the resulting Value to an S-exp representation (numbers ---> number S-exp | objects ----> symbol object)
|#
(define (interp-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(objV class-name field-vals) `object])))

(module+ test
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

 (test (interp-prog 
        (list
         `{class Posn extends Object
            {x y}
            [mdist {arg} {+ {get this x} {get this y}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
         
         `{class Posn3D extends Posn
            {z}
            [mdist {arg} {+ {get this z} 
                            {super mdist arg}}]})
        
        `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
       `18)
  (test (interp-prog (list `{class Snowball extends Object
                              {size}
                              [zero {arg} this]
                              [nonzero {arg}
                                       {new Snowball {+ 1 {get this size}}}]})
                     `{get {select 0 {new Snowball 1}} size})
        `1)
  (test (interp-prog (list `{class Snowball extends Object
                              {size}
                              [zero {arg} this]
                              [nonzero {arg}
                                       {new Snowball {+ 1 {get this size}}}]})
                     `{get {select {+ 1 2} {new Snowball 1}} size})
        `2)
  (test/exn (interp-prog
             (list `{class Snowball extends Object
                     {size}
                     [zero {arg} this]
                     [nonzero {arg}
                       {new Snowball {+ 1 {get this size}}}]})
             `{get {select this {new Snowball 1}} size})
            "not a number")

  (test/exn (interp-prog
             (list `{class Snowball extends Object
                     {size}
                     [zero {arg} this]
                     [nonzero {arg}
                       {new Snowball {+ 1 {get this size}}}]})
             `{select 0 1})
            "not an object")
  (test (interp-prog (list `{class Fish extends Object
                              {size color}})
                     `{instanceof {new Fish 1 2} Object})
        `0)
  (test (interp-prog (list `{class Fish extends Object
                              {size color}})
                     `{instanceof {new Object} Fish})
        `1)
  (test (interp-prog (list `{class Fish extends Object
                              {size color}})
                     `{instanceof {new Fish 1 2} Fish})
        `0)
  (test (interp-prog (list `{class Fish extends Object
                              {size color}}
                           `{class Shark extends Fish
                              {teeth}})
                     `{instanceof {new Shark 1 2 3} Fish})
        `0)
  (test (interp-prog (list `{class Fish extends Object
                              {size color}}
                           `{class Shark extends Fish
                              {teeth}}
                           `{class Hammerhead extends Shark
                              {}})
                     `{instanceof {new Hammerhead 1 2 3} Fish})
        `0)
  (test (interp-prog (list `{class PlainFish extends Object
                              {size}}
                           `{class ColorFish extends PlainFish
                              {color}}
                           `{class Bear extends Object
                              {size color}
                              [rate-food {arg}
                                         {+ {instanceof arg ColorFish}
                                            {get arg color}}]})
                     `{send {new Bear 100 5} rate-food {new ColorFish 10 3}})
        `3))


