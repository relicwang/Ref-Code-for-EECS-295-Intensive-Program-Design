;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lec06c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A RandNumTree is one of:
;; - (make-node Number Natural RandNumTree RandNumTree)
;; - '()
(define-struct node (key size left right))

;; An example tree:
(define A-TREE
  (make-node 4 3
             (make-node 2 1 '() '())
             (make-node 6 1 '() '())))

;; contains? : RandNumTree Number -> Boolean
;; Returns whether the given Number is in the tree.
;;
;; Strategy: struct. decomp.
(define (contains? t k)
  (cond
    [(empty? t) #false]
    [else
     (cond
      [(< k (node-key t)) (contains? (node-left t) k)]
      [(> k (node-key t)) (contains? (node-right t) k)]
      [else #true])]))

(check-expect (contains? A-TREE 2) #t)
(check-expect (contains? A-TREE 4) #t)
(check-expect (contains? A-TREE 6) #t)
(check-expect (contains? A-TREE 1) #f)
(check-expect (contains? A-TREE 3) #f)

;; tree-size : RandNumTree -> Natural
;; Gets the size of a possibly empty tree.
;;
;; Strategy: struct. decomp.
(define (tree-size t)
  (cond
    [(node? t) (node-size t)]
    [else      0]))

(check-expect (tree-size '()) 0)
(check-expect (tree-size A-TREE) 3)

;; new-node : Natural RandNumTree RandNumTree-> RandNumTree
;; Creates a new singleton tree with the given value.
;;
;; Strategy: function composition
(define (new-node k l r)
  (make-node k (+ 1 (tree-size l) (tree-size r)) l r))

(check-expect (new-node 5 '() '()) (make-node 5 1 '() '()))
(check-expect (new-node 1 '() A-TREE) (make-node 1 4 '() A-TREE))


;; leaf-insert : RandNumTree Number -> RandNumTree
;; Inserts a key as a leaf, returning the modified tree.
;;
;; Strategy: struct. decomp.
(define (leaf-insert t k)
  (cond
    [(empty? t) (new-node k '() '())]
    ;; Strategy note: the remaining cases should be, strictly speaking,
    ;; a nested cond in the `else` branch of the outer cond.
    [(< k (node-key t))
     (new-node (node-key t)
               (leaf-insert (node-left t) k)
               (node-right t))]
    [(> k (node-key t))
     (new-node (node-key t)
               (node-left t)
               (leaf-insert (node-right t) k))]
    [else
     t]))

(check-expect (leaf-insert A-TREE 2) A-TREE)
(check-expect (leaf-insert A-TREE 4) A-TREE)
(check-expect (leaf-insert A-TREE 6) A-TREE)
(check-expect (leaf-insert A-TREE 5)
              (make-node 4 4
                         (make-node 2 1 '() '())
                         (make-node 6 2
                                    (make-node 5 1 '() '())
                                    '())))

;; rotate-right : RandNumTree -> Void
;; Performs a right rotation
;;
;; ASSUMPTION: d is non-empty and has a non-empty left child.
;;
;; Strategy: struct. decomp.
(define (rotate-right d)
  (new-node (node-key (node-left d))
            (node-left (node-left d))
            (new-node (node-key d)
                      (node-right (node-left d))
                      (node-right d))))

(check-expect (rotate-right A-TREE)
              (make-node 2 3 '()
                         (make-node 4 2 '()
                                    (make-node 6 1 '() '()))))

;; rotate-left : RandNumTree -> Void
;; Performs a left rotation
;;
;; ASSUMPTION: d is non-empty and has a non-empty right child.
;;
;; Strategy: struct. decomp.
(define (rotate-left b)
  (new-node (node-key (node-right b))
            (new-node (node-key b)
                      (node-left b)
                      (node-left (node-right b)))
            (node-right (node-right b))))

(check-expect (rotate-left A-TREE)
              (make-node 6 3
                         (make-node 4 2
                                    (make-node 2 1 '() '())
                                    '())
                         '()))

;; root-insert : RandNumTree Number -> RandNumTree
;; Inserts an element at the root, returning the modified tree.
;;
;; Strategy: struct. decomp.
(define (root-insert t k)
  (cond
    [(empty? t) (new-node k '() '())]
    [(< k (node-key t))
     (rotate-right (new-node
                    (node-key t)
                    (root-insert (node-left t) k)
                    (node-right t)))]
    [(> k (node-key t))
     (rotate-left (new-node
                   (node-key t)
                   (node-left t)
                   (root-insert (node-right t) k)))]
    [else
     t]))

(check-expect (root-insert A-TREE 5)
              (make-node 5 4
                         (make-node 4 2
                                    (make-node 2 1 '() '())
                                    '())
                         (make-node 6 1 '() '())))
(check-expect (root-insert A-TREE 4) A-TREE)
(check-expect (root-insert A-TREE 2)
              (make-node 2 3
                         '()
                         (make-node 4 2
                                    '()
                                    (make-node 6 1 '() '()))))

;; insert : RandNumTree Number -> RandNumTree
;; Inserts an element, maintaining randomness and returning the modified tree.
;;
;; Strategy: struct. decomp.
(define (insert t k)
  (cond
    [(empty? t) (new-node k '() '())]
    [(zero? (random (add1 (tree-size t))))
     (root-insert t k)]
    [(< k (node-key t))
     (new-node (node-key t)
               (insert (node-left t) k)
               (node-right t))]
    [(> k (node-key t))
     (new-node (node-key t)
               (node-left t)
               (insert (node-right t) k))]
    [else t]))

(check-expect (tree-size (insert '() 5)) 1)
(check-expect (tree-size (insert A-TREE 3)) 4)
(check-expect (tree-size (insert A-TREE 4)) 3)
(check-expect (tree-size (insert A-TREE 2)) 3)
(check-expect (tree-size (insert A-TREE 6)) 3)

;; It's hard to get coverage when the function uses randomness,
;; so we repeat the tests two more times.

(check-expect (tree-size (insert A-TREE 3)) 4)
(check-expect (tree-size (insert A-TREE 4)) 3)
(check-expect (tree-size (insert A-TREE 2)) 3)
(check-expect (tree-size (insert A-TREE 6)) 3)

(check-expect (tree-size (insert A-TREE 3)) 4)
(check-expect (tree-size (insert A-TREE 4)) 3)
(check-expect (tree-size (insert A-TREE 2)) 3)
(check-expect (tree-size (insert A-TREE 6)) 3)

;; build-tree : Natural [Natural -> Number] -> RandNumTree
;; Builds a random tree of `n` elements, using `f` to produce each element
;; (like `build-list`)
;;
;; Strategy: struct. decomp.
(define (build-tree n f)
  (cond
    [(zero? n) '()]
    [else      (insert (build-tree (sub1 n) f) (f (sub1 n)))]))

(check-expect (build-tree 0 identity) '())
(check-expect (build-tree 1 identity) (new-node 0 '() '()))

;; height : RandNumTree -> Natural
;; Returns the height (max depth) of a tree.
;;
;; Strategy: struct. decomp.
(define (height t)
  (cond
    [(node? t) (add1 (max (height (node-left t))
                          (height (node-right t))))]
    [else 0]))

(check-expect (height A-TREE) 2)

;; sample-height : Natural -> Natural
;; Builds a tree with `n` elements and returns its height.
;;
;; Strategy: struct. decomp.
(define (sample-height n)
  (height (build-tree n identity)))

(check-expect (< (sample-height 100) 20) #t)

#|
Deletion Algorithm
------------------

To delete an element from the tree, we first search for it in the normal
way. Once we find the node containing the element to delete, we delete
it by replacing it with its subtrees, joined as follows: We select one
or the other tree to be the root of the joined subtree, randomly, where
the size of each tree provides its weight. Suppose we choose the left
subtree to be the root—then the left subtree of the left subtree remains
the same, but we make its right subtree the join of its right subtree
and the right subtree. The right subtree case is symmetric.

For example, suppose we want to remove D from this tree:

      __D__
     /     \
    B       F
   / \     / \
  A   C   E   G

We then have to join the two subtrees:

    B       F
   / \  +  / \
  A   C   E   G

Suppose we randomly select the tree rooted at F to replace D. Then we
recursively replace E, the left subtree of F, with the join of subtrees
B and E:

       ___F___
      /       \
    B + E      G
   / \
  A   C

Note that everything remains ordered.
|#

;; join : RandNumTree RandNumTree -> RandNumTree
;; Joins two trees, assuming all the keys of the first are less than
;; all the keys of the second.
;;
;; Strategy: struct. decomp. (t1 and t2)
(define (join t1 t2)
  (cond
    [(empty? t1) t2]
    [(empty? t2) t1]
    [(< (random (+ (tree-size t1) (tree-size t2))) (tree-size t1))
     (new-node (node-key t1)
               (node-left t1)
               (join (node-right t1) t2))]
    [else
     (new-node (node-key t2)
               (join t1 (node-left t2))
               (node-right t2))]))

;; delete : RandNumTree Number -> RandNumTree
;; Deletes an element from a tree, returning the modified tree.
;;
;; Strategy: struct. decomp.
(define (delete t k)
  (cond
    [(empty? t) t]
    [(< k (node-key t))
     (new-node (node-key t)
               (delete (node-left t) k)
               (node-right t))]
    [(> k (node-key t))
     (new-node (node-key t)
               (node-left t)
               (delete (node-right t) k))]
    [else
     (join (node-left t) (node-right t))]))

(check-expect (delete A-TREE 2)
              (new-node 4 '() (new-node 6 '() '())))
(check-expect (delete A-TREE 6)
              (new-node 4 (new-node 2 '() '()) '()))
(check-expect (delete A-TREE 3) A-TREE)

;; Doing this four times should get us coverage 15/16 of the time.
(check-member-of (delete A-TREE 4)
                 (new-node 2 '() (new-node 6 '() '()))
                 (new-node 6 (new-node 2 '() '()) '()))
(check-member-of (delete A-TREE 4)
                 (new-node 2 '() (new-node 6 '() '()))
                 (new-node 6 (new-node 2 '() '()) '()))
(check-member-of (delete A-TREE 4)
                 (new-node 2 '() (new-node 6 '() '()))
                 (new-node 6 (new-node 2 '() '()) '()))
(check-member-of (delete A-TREE 4)
                 (new-node 2 '() (new-node 6 '() '()))
                 (new-node 6 (new-node 2 '() '()) '()))
