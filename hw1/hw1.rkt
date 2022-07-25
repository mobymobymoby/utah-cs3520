#lang plait

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

; part 1
(define (sum (tree : Tree)) : Number
  (type-case Tree tree
    [(leaf val) val]
    [(node val left right) (+ val (+ (sum left) (sum right)))]))

(test (sum (leaf 10)) 10)
(test (sum (node 1 (leaf 2) (leaf 3))) 6)

; part 2
(define (negate (tree : Tree)) : Tree
  (type-case Tree tree
    [(leaf val) (leaf (- 0 val))]
    [(node val left right) (node (- 0 val) (negate left) (negate right))]))

(test (negate (leaf 1)) (leaf -1))

(test (negate (node 1 (leaf 2) (leaf 3)))
      (node -1 (leaf -2) (leaf -3)))

; part 3
 (define (contains? (tree : Tree) (n : Number)) : Boolean
   (type-case Tree tree
     [(leaf val) (= val n)]
     [(node val left right)
      (cond ; Isn't there any AND operator...?
        [(= val n) #t]
        [(contains? left n) #t]
        [(contains? right n) #t]
        [else #f])]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 7) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 5) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 1) #f)

; part 4
(define (bigger-leaves? (tree : Tree) (base : Number)) : Boolean
  (type-case Tree tree
    [(leaf v) (< base v)]
    [(node v l r)
     (cond
       [(not (bigger-leaves? l (+ base v))) #f]
       [(not (bigger-leaves? r (+ base v))) #f]
       [else #t])]))
  
(define (big-leaves? (tree : Tree)) : Boolean
  (bigger-leaves? tree 0))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
(test (big-leaves? (node 1
                         (node 2
                               (node 3
                                     (leaf 7)
                                     (leaf 7))
                               (leaf 4))
                         (leaf 1))) ; <<
      #f)

(test (big-leaves? (node 1
                         (node 2
                               (node 3
                                     (leaf 7)
                                     (leaf 7))
                               (leaf 4))
                         (leaf 2)))
      #t)

(test (big-leaves? (node 1
                         (node 2
                               (node 3
                                     (leaf 6) ; <<
                                     (leaf 7))
                               (leaf 4))
                         (leaf 2)))
      #f)

; part 5
(define (positive-trees? (tree_list : (Listof Tree))) : Boolean
  (cond
    [(= (length tree_list) 0) #t]
    [(<= (sum (first tree_list)) 0) #f]
    [(positive-trees? (rest tree_list)) #t]
    [else #f]))

(test (positive-trees? (cons (leaf 6)
                             empty))
      #t)

(test (positive-trees? (cons (leaf -6)
                             empty))
      #f)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                             empty))
      #t)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                             (cons (node 0 (leaf 0) (leaf 1))
                                   empty)))
      #t)

(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                             (cons (node 0 (leaf 0) (leaf 1))
                                   empty)))
      #f)


; part 6 - traverse in-order
(define (flatten (tree : Tree)) : (Listof Number)
  (type-case Tree tree
    [(leaf v)
     (list v)]
    [(node v left right)
     (cons v (append (flatten left) (flatten right)))]))

(test (flatten (node 1 (node 2 (leaf 3) (leaf 4)) (node 5 (leaf 6) (leaf 7))))
      (list 1 2 3 4 5 6 7))

(test (flatten (leaf 1))
      (list 1))

(test (flatten (node 1 (leaf 2) (node 3 (leaf 4) (leaf 5))))
      (list 1 2 3 4 5))
