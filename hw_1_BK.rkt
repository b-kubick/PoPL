#lang plait
#|
PoPL - HW 1
Besjana Kubick
Due: 31.01.2024
|#

;; Tree datatype that implements a binary tree with a number in each node and leaf
(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree])
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 1. Sum
;; Implement a sum function that takes a tree and returns the sum of the numbers in the tree
;; Example: (sum (node 5 (leaf 6) (leaf 7))) should produce 18.

;; Write tests first function second.
;; sub-module so that the tests run after the function is build this way you don't get an error that the function is not defined

(module+ test
  (test (sum (leaf 1))
        1)
  (test (sum (node 5 (leaf 6) (leaf 7)))
        18)
  (test (sum (node 8 (leaf 2) (leaf -2)))
        8)
  )

#|
- sum function that takes a tr type Tree (defined above) and returns Number
- type-case Tree to handle different constructors. Use it to distinguish if the 'tree' is a leaf or a node
  inside that type-case, handle the cases
 - leaf case: if it is a leaf return the value of that leaf
 - node case: if it is a node sum the value of the node with the sum of the left and the right subtree
  -- explanation: it traverses through the tree. goes all the way until it is a leaf. Does a recursive call.
                  and builds back up

|#
(define (sum [tr : Tree]) : Number
  (type-case Tree tr
    [(leaf v) v] ; when it's a leaf return that values
    [(node v l r) (+ v
                     (+ (sum l) (sum r)))])
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Part 2. - Negate
;; Implement the function negate, which takes a tree and returns a tree that has the same shape but with all the numbers negated

;; Example: (negate (node 5 (leaf 6) (leaf 7))) should produce (node -5 (leaf -6) (leaf -7))

#|
- negate function that takes a tr type Tree (as defined above) and returns a Tree
- type-case Tree to handle different constructors. Use it to distinguish if the 'tree' is a leaf or a node
  inside that type-case, handle the cases
 - leaf case: if it is a leaf with a value v it creates a new leaf with value (0-v) ...to negate it...
 - node case: if it is a node it creates a new node with the negated value and it recursively negates the left 'l' and right 'r' subtrees until it reaches the leafs.
|#

(module+ test
  (test (negate (node 5 (leaf 6) (leaf 7)))
        (node -5 (leaf -6) (leaf -7)))
  (test (negate (node 5 (leaf -6) (leaf 7)))
        (node -5 (leaf 6) (leaf -7)))
  )


(define (negate [tr : Tree]) : Tree
  (type-case Tree tr
    [(leaf v) (leaf (- 0 v))]
    [(node v l r) (node (- 0 v) (negate l) (negate r))])
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Part 3.
;; Implement the function contains?, which takes a tree and a number and returns #t if the number is in the tree
; #f otherwise.

;; Example: (contains? (node 5 (leaf 6) (leaf 7)) 6) should produce #t.
;; The second argument to the contains? function is “along for the ride.”

#|
- contains? takes a tr type Tree and nr type Number as parameters and returns Boolean
- type-case tr to handle cases:
 - if 'tr' is a leaf, check if the leaf's value v is equal to the number nr
 - if 'tr' is a node check if the node's value v is equal to nr
   - or if it is contained in the left subtree
   - or if it is contained in the right subtree

|#
(module+ test
  (test (contains? (node 5 (leaf 6) (leaf 7)) 6) ; contained in the left 
        #t)
  (test (contains? (node 5 (leaf 6) (leaf 7)) 7) ; contained in the right
        #t)
  (test (contains? (node 5 (leaf 6) (leaf 7)) 8) ; not in the tree at all
        #f)
  (test (contains? (node 5 (leaf 6) (leaf 7)) 5) ; contained in the node 
        #t)
  )

(define (contains? [tr : Tree] [nr : Number]) : Boolean
  (type-case Tree tr
    [(leaf v) (equal? v nr)]
    [(node v l r)
          (or (equal? v nr)
              (contains? l nr)
              (contains? r nr))])
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Part 4. Big Leaves?

;; Implement the function big-leaves?, which takes a tree and returns #t if every leaf is bigger than the sum of numbers in the path of nodes from the root that reaches the leaf.

;; Examples: (big-leaves? (node 5 (leaf 6) (leaf 7))) should produce #t,
;; while (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) should produce #f (since 6 is smaller than 5 plus 2).

;; The big-leaves? function should be a thin wrapper on another function, perhaps bigger-leaves?,
;; that accumulates a sum of node values.

(module+ test
  (test (big-leaves? (node 5 (leaf 6) (leaf 7)))
        #t)
  (test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))
        #f)
  (test (big-leaves? (leaf 1))
        #t)
  )

#|
Helper function: (recursive)

- takes a tree and the sum-path and Returns Boolean
- type-case to handle leaf case and node case
 - if 'tr' is a leaf check if leaf value v is greater than sum-path until. if so return #t
 - if 'tr' is a node; calculate the sum-path by adding the nodes value v to the sum-path and check recursively
   the left and the right subtree (recursively calls itself until it reaches a leaf and then compares).

   Use AND since they both need to be #t for the entire funx to produce true
|#
(define (bigger-leaves? [tr : Tree] [sum-path : Number]) : Boolean
  (type-case Tree tr
    [(leaf v) (> v sum-path)]
    [(node v l r) (and (bigger-leaves? l (+ sum-path v))
                       (bigger-leaves? r (+ sum-path v)))])               
  )

#|
Main function to be called. Set path to 0
|#

(define (big-leaves? [tr : Tree]) : Boolean
  (bigger-leaves? tr 0)
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Part 5.
;; Implement the function positive-trees?, which takes a list of trees and returns #t if every member of the list is a positive tree, where a positive tree is one whose numbers sum to a positive value.

;; Hint 1: This function takes a list, not a tree, so don’t try to use the template for a tree function.

;; Hint 2: Use your sum function as a helper.

;; Hint 3: (positive-trees? empty) should produce #t, because there’s no tree in the empty list whose numbers sum to 0 or less.

(module+ test
  (test (positive-trees? (cons (leaf 6) empty))
        #t)
  (test (positive-trees? (cons (leaf -6) empty))
        #f)
  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6)) empty))
        #t)
  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1)) empty)))         
        #t)
  (test (positive-trees?  (cons (node -1 (leaf 6) (leaf -6))
                                (cons (node 0 (leaf 0) (leaf 1))  empty)))                            
        #f)
  )

#|
- Use sum function as a helper function / Empty tree is
- Define a function positive-trees? that takes a list of trees {tr : (Listof Tree)} and returns Boolean
-  cond
 - when tree is empty return #t
 - else
  - check if the sum of the first tree is positive
  - AND recursively check if the sum of the rest of the tree is positive
  - BOTH of these conditions need to be true
|#

(define (positive-trees? [tr : (Listof Tree)]) : Boolean
  (cond
    [(empty? tr) #t]
    [else
     (and (> (sum (first tr)) 0)
          (positive-trees? (rest tr)))])
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 6. - Bonus: Flatten

;;Implement the function flatten, which takes a tree and returns a list that has all numbers in the tree’s nodes and leaves.
;;The numbers should be ordered to match an inorder travsersal of the tree, and a number that appears multiple times in the tree should appear the same number of times in the list.

;; Your function should run in time proportional to the size of the tree, which rules out making a list of the tree numbers using append on recursive calls.
;; You may find it helpful to recur on a right subtree before a left subtree.
;; Hint: Does the function take a list or a tree? Which template should you use?


#|
Inorder Traversal: left - root - right
Build the list as you traverse the tree, without needing to traverse the list itself multiple times
=> build the list in reverse order
Don't use append => use cons => start from the right subtree since cons prepends it (meaning if left is done last then you get the order left - root - right)
Use a flatten-helper function
Since it takes a tree use the tree template function with type-case
;;;;;;;;;;;;;
(!!! type-case is used to perform pattern matching of the type. It differentiates between two possible constructors)
;;;;;;;;;;;;;
- flatten-helper takes a 'tr' type Tree and 'collect-nr' type (Listof Number) which is a list that collects numbers in inorder traversal (left - root - right)
- type-case to handle cases when it is a leaf or a node
 - if it is a leaf then add the leaf value v to the collect-nr (cons for prepending)
 - if it is a node recursively call flatten-helper
  - first recursively flatten the right subtree and prepends that value to the
    current collect-nr (flatten-helper r collect-nr)
  - then prepend the node value v to the list created from flattening the right subtree
  - recursively flatten the left subtree
|#

(module+ test
  #;(test (flatten (leaf empty)) ; how do I test an empty tree. Do I need to?
        empty)
  (test (flatten (leaf 3))
        (list 3))
  (test (flatten (node 1 (leaf 2) (leaf 3)))
        (list 2 1 3))
  (test (flatten (node 1 (node 2 (leaf 3) (leaf 4)) (node 5 (leaf 6) (leaf 7))))
        (list 3 2 4 1 6 5 7))
  )



(define (flatten-helper [tr : Tree] [collect-nr : (Listof Number)]) : (Listof Number)
  (type-case Tree tr
    [(leaf v) (cons v collect-nr)]
    [(node v l r)
     (flatten-helper l (cons v (flatten-helper r collect-nr)))
     ])
  )

#|
main function that uses the helper function
Starts the flatten-helper with an empty list which will collect the nr from the tree that will be flatten
|#
(define (flatten [tr : Tree]) : (Listof Number)
  (flatten-helper tr empty)
  )






