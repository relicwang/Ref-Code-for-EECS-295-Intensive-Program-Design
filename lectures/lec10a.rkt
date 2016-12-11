#lang dssl

;; Graph Search
;; ------------

;; A week ago we were introduced to a graph representation, and we wrote
;; a function to determine when a graph has a path between two given
;; nodes. The function we wrote was very inefficient—exponential time,
;; actually—because it explored every path through the graph. (Remember
;; the example?) We can do better using less naive graph search
;; algorithms: depth-first search and breadth-first search. Let’s look
;; at DFS first.

; A Graph is:
;   (make-graph Nat (Nat[< nodes] -> [List-of Nat[< nodes]]))
; the `nodes` field tells us the number of nodes in the graph;
; the `succs` field gives us a function from each node to the list
; of its successors.
(define-struct graph (nodes succs))

(define A-GRAPH
  (make-graph 7
              (lambda (node)
                (case node
                  [(0) '(1 2 3)]
                  [(1) '(3 0 4)]
                  [(2) '(4 0)]
                  [(3) '(4 1 2)]
                  [(4) '(5 4)]
                  [(5) '()]
                  [(6) '(3 4)]))))

; A SearchTree is [Vector-of [Or Nat Bool]]
; where for each node n, the nth element is the parent node of node n;
; the value for the root is #true, and the value for unreachable nodes is
; #false.

; dfs : Graph Nat -> SearchTree
; Searches `a-graph` starting at `start-node`, returning the search
; tree.
;
; Strategy: generative recursion. Termination: `visit` can only recur
; a finite number of times, because each time it recurs it removes a
; node from the unvisited set, which can happen only a finite number of
; times.
(define (dfs a-graph start-node)
  ; : SearchTree
  (define result (make-vector (graph-nodes a-graph) #false))
  ; : Nat -> Void
  (define (visit node)
    (for ([succ ((graph-succs a-graph) node)])
      (when (false? (vector-ref result succ))
        (vector-set! result succ node)
        (visit succ))))

  (vector-set! result start-node #true)
  (visit start-node)
  result)

(check-expect (dfs A-GRAPH 0)
              (vector #t 0 3 1 3 4 #f))
(check-expect (dfs A-GRAPH 1)
              (vector 2 #t 3 1 3 4 #f))
(check-expect (dfs A-GRAPH 5)
              (vector #f #f #f #f #f #t #f))
(check-expect (dfs A-GRAPH 6)
              (vector 1 3 0 6 3 4 #t))

;; Now we can write a much more efficient route-exists?:

; route-exists? : Nat Nat Graph -> Bool
; Determines whether the graph contains a path from `start` to `end`.
;
; Strategy: function composition
(define (route-exists? start end a-graph)
  (not (false? (vector-ref (dfs a-graph start) end))))

(check-expect (route-exists? 0 4 A-GRAPH) #true)
(check-expect (route-exists? 0 6 A-GRAPH) #false)
(check-expect (route-exists? 3 4 A-GRAPH) #true)
(check-expect (route-exists? 4 3 A-GRAPH) #false)
(check-expect (route-exists? 5 5 A-GRAPH) #true)


;; DFS uses the stack-like structure of evaluation to remember which
;; node to search next. We can instead make the stack of nodes to search
;; explicit, and then we can generalize that to get other graph search
;; algorithms.

;; In particular, we can generalize graph search as follows. Given a
;; graph, start with an empty to-do list and empty search tree. Add the
;; given starting node to the to-do list. Then repeat so long as the
;; to-do list is non-empty: Remove a node n (any node) from the to-do
;; list and examine each of its successors. If successor s has not been
;; seen according to the search tree, then record n as s's predecessor
;; in the search tree and add s to the to-do list.

;; To implement this algorithm we need a data structure to serve as the
;; to-do list. We can partially specify the requirements as an ADT:

;; Container ADT:
;;  - empty : -> Container
;;  - empty? : Container -> Bool
;;  - add! : Container Element -> Void
;;  - remove! : Container -> Element

;; Different container implementations will remove nodes in different
;; orders, yielding different kinds of graph searches.

;; We can write our search in a generic way, where we don’t specify
;; ahead of time what kind of container to use for the to-do list. To do
;; this, we make a structure that holds functions for all the container
;; ADT operations, and then pass that structure to the search function.

; A *CONTAINER* is (for some Repr)
;  (make-container [-> [Repr X]]
;                  [[Repr X] -> Bool]
;                  [[Repr X] X -> Void]
;                  [[Repr X] -> X])
; where the four functions are interpreted as the operations of the
; Container ADT.
(define-struct container (empty empty? add! remove!))

;; Here’s an example for how a *CONTAINER* is used:

; container-example : *CONTAINER* [List-of X] -> [List-of X]
; Uses `*C*` to create an empty container, to which it adds the elements
; of `elements` in order; then removes all the elements and returns them
; in a list in the order removed.
(define (container-example *C* elements)
  (define result '())
  (define container ((container-empty *C*)))
  (for ([element elements])
    ((container-add! *C*) container element))
  (while (not ((container-empty? *C*) container))
    (set! result (cons ((container-remove! *C*) container) result)))
  (reverse result))

;; And here’s a simple implementation of a *CONTAINER*:

; *LIST-STACK* : *CONTAINER*
(define *LIST-STACK*
  (local
    [(define-struct list-stack (list))]
    (make-container
     (lambda () (make-list-stack '()))
     (lambda (s) (empty? (list-stack-list s)))
     (lambda (s x) (set-list-stack-list! s (cons x (list-stack-list s))))
     (lambda (s)
       (define result (first (list-stack-list s)))
       (set-list-stack-list! s (rest (list-stack-list s)))
       result))))

(check-expect (container-example *LIST-STACK* '()) '())
(check-expect (container-example *LIST-STACK* '(1)) '(1))
(check-expect (container-example *LIST-STACK* '(1 2 3 4)) '(4 3 2 1))

;; Now we can write the search algorithm generically, where it is passed
;; what kind of container to use:

; generic-search : *CONTAINER* Graph Nat -> SearchTree
; Performs a graph search from the given start node, using the given
; container implementation to order the search.

; Strategy: generative iteration. Termination: each iteration through
; the loop removes a node from `to-do`, and each node is added to `to-do`
; at most once, since it is also marked in `result` as having been visited,
; and thus won’t be visited again.
(define (generic-search *C* a-graph start-node)
  (define result (make-vector (graph-nodes a-graph) #false))
  (define to-do ((container-empty *C*)))
  (vector-set! result start-node #true)
  ((container-add! *C*) to-do start-node)
  (while (not ((container-empty? *C*) to-do))
    (define node ((container-remove! *C*) to-do))
    (for ([succ ((graph-succs a-graph) node)])
      (when (false? (vector-ref result succ))
        (vector-set! result succ node)
        ((container-add! *C*) to-do succ))))
  result)

;; If we use a stack as the container, we get DFS:
(check-expect (generic-search *LIST-STACK* A-GRAPH 0)
              (vector #t 0 0 0 3 4 #f))
(check-expect (generic-search *LIST-STACK* A-GRAPH 5)
              (vector #f #f #f #f #f #t #f))
(check-expect (generic-search *LIST-STACK* A-GRAPH 6)
              (vector 2 3 3 6 6 4 #true))

;; Another possible container is a queue:

; *BANKERS-QUEUE* : *CONTAINER*
(define *BANKERS-QUEUE*
  (local
    ((define-struct bankers-queue (front back)))
    (make-container
     (lambda () (make-bankers-queue '() '()))
     (lambda (q)
       (and (empty? (bankers-queue-front q))
            (empty? (bankers-queue-back q))))
     (lambda (q x)
       (set-bankers-queue-back! q (cons x (bankers-queue-back q))))
     (lambda (q)
       (cond
         [(cons? (bankers-queue-front q))
          (define result (first (bankers-queue-front q)))
          (set-bankers-queue-front! q (rest (bankers-queue-front q)))
          result]
         [else
          (define new-front (reverse (bankers-queue-back q)))
          (set-bankers-queue-front! q (rest new-front))
          (set-bankers-queue-back! q '())
          (first new-front)])))))

(check-expect (container-example *BANKERS-QUEUE* '()) '())
(check-expect (container-example *BANKERS-QUEUE* '(1)) '(1))
(check-expect (container-example *BANKERS-QUEUE* '(1 2 3 4)) '(1 2 3 4))

;; Using a stack as the container gets us BFS (breadth-first search),
;; which visits nodes in order by increasing distance from the start
;; node.
(check-expect (generic-search *BANKERS-QUEUE* A-GRAPH 0)
              (vector #t 0 0 0 1 4 #f))
(check-expect (generic-search *BANKERS-QUEUE* A-GRAPH 5)
              (vector #f #f #f #f #f #t #f))
(check-expect (generic-search *BANKERS-QUEUE* A-GRAPH 6)
              (vector 1 3 3 6 6 4 #t))
