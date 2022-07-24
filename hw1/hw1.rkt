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
