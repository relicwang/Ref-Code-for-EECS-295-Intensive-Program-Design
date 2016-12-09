#lang dssl

;; Problem: find the shortest path between two points in a weighted graph.
;;
;; It turns out that the reasonable ways to compute this need to compute more
;; information than we actually need, in particular, we typically will need to
;; compute the shortest path from the starting node to *every* node. This is
;; called the Single-Source Shortest Path problem.

;; To work on this problem, first we need a representation of weighted graphs.

; A WEdge is (make-wedge Nat Number Nat)
(define-struct wedge (src weight dst))
; Interpretation: an edge exists from `src` to `dst` with weight
; `weight`.

; A WGraph is (make-wgraph Nat [Nat -> [List-of WEdge]])
(define-struct wgraph (nodes edges))
; Interpretation: wgraph-nodes gives the number of nodes in the graph, and
; the function wgraph-edges is a function that, given a node, returns a
; list of all departing edges.

;; (Note: WGraph can be used to represent a weighted, directed graph, or
;; an undirected graph as a special case of that. The algorithms below
;; work on both, but our example is an undirected graph.)

; build-wdigraph : Nat [List-of [List Nat Number Nat]] -> WGraph
; Builds a directed graph with `nodes` nodes and the edges specified
; by `edges`.
(define (build-wdigraph nodes edges)
  (define adjacencies (make-vector nodes '()))
  
  (define (cons! edge v)
    (vector-set! adjacencies v (cons edge (vector-ref adjacencies v))))
  
  (for ([edge edges])
    (cons! (make-wedge (first edge) (second edge) (third edge)) (first edge)))
  
  (make-wgraph nodes (lambda (v) (vector-ref adjacencies v))))

; build-wugraph : Nat [List-of [List Nat Number Nat]] -> WGraph
; Builds an undirected graph with `nodes` nodes and the edges specified
; by `edges`.
(define (build-wugraph nodes edges)
  (build-wdigraph nodes (append edges (map reverse edges))))

;; Graph from Wikipedia Dijkstra’s algo page
(define A-GRAPH
  (build-wugraph
    7
    '((1 7 2)
      (1 9 3)
      (1 14 6)
      (2 10 3)
      (2 15 4)
      (3 11 4)
      (3 2 6)
      (4 6 5)
      (5 9 6))))

;; SSSP has an interesting structure. Consider this: Suppose the last node
;; on the shortest path to v is u. Then the rest of the shortest path to v
;; is the same as the shortest path to u. So to track shortest paths, we
;; just need to know the path predecessor nodes—the parents in a search tree.

; An SSSPResult is (make-sssp [Vector-of [Or Nat Bool]] [Vector-of Weight])
(define-struct sssp (preds weights))
; Interpretation: sssp-preds gives the predecessor on the shortest path to
; each node, and sssp-weights gives the weight of the path. For unreachable
; nodes sssp-preds will be false, and for the start node it will be true.
; For unreachable nodes, sssp-weights will be 'inf, standing for infinity.

; A Weight is one of:
;  - Number
;  - 'inf


;; The shortest path to any node v must pass through one of its graph
;; predecessor nodes u, and its distance will be the sum of shortest path
;; length to u and the edge weight from u to v. So to find the shortest
;; path, provided we know the shortest paths to all possible u nodes, we
;; just have to inspect each possibility and minimize. One way to think of
;; this is that we gain knowledge as we search the graph, and in particular
;; when we arrive at a node v from a node u, we know that its path is no
;; worse than the path to u plus the edge to v. So we can check if we’ve
;; found a better path, and update our knowledge if we haven’t.
;;
;; This update is called relaxation.

; relax : SSSPResult Nat Nat Weight -> Void
; Updates the distance to `v` given the best distance to `u` found so far
; and the weight `weight` of the edge between `u` and `v`.
(define (relax result u weight v)
  (define old-weight (vector-ref (sssp-weights result) v))
  (define new-weight (weight+ weight (vector-ref (sssp-weights result) u)))
  (when (weight< new-weight old-weight)
    (vector-set! (sssp-weights result) v new-weight)
    (vector-set! (sssp-preds result) v u)))

; weight+ : Weight Weight -> Weight
; Adds two weights, respecting infinity.
(define (weight+ a b)
  (if (and (number? a) (number? b))
      (+ a b)
      'inf))

(check-expect (weight+ 3 4) 7)
(check-expect (weight+ 'inf 4) 'inf)
(check-expect (weight+ 3 'inf) 'inf)
(check-expect (weight+ 'inf 'inf) 'inf)

; weight< : Weight Weight -> Bool
; Compares two weights, respecting infinity.
(define (weight< a b)
  (cond
    [(symbol? a) #false]
    [(symbol? b) #true]
    [else (< a b)]))

(check-expect (weight< 3 4) #true)
(check-expect (weight< 4 3) #false)
(check-expect (weight< 'inf 'inf) #false)
(check-expect (weight< 3 'inf) #true)
(check-expect (weight< 'inf 4) #false)

;; One way we can compute SSSP is to relax all the edges enough times that
;; all information propagates. How many times at most do we have to relax
;; the edges of a graph?

; bellman-ford : WGraph Nat -> SSSPResult
; Computes SSSP from the given start node.
; ASSUMPTION: The graph contains no negative cycles.
(define (bellman-ford a-graph start-node)
  (define result (make-sssp (make-vector (wgraph-nodes a-graph) #false)
                            (make-vector (wgraph-nodes a-graph) 'inf)))
  (vector-set! (sssp-preds result) start-node #true)
  (vector-set! (sssp-weights result) start-node 0)
  (for ([_ (in-range (wgraph-nodes a-graph))])
    (for ([u (in-range (wgraph-nodes a-graph))])
      (for ([edge ((wgraph-edges a-graph) u)])
        (relax result u (wedge-weight edge) (wedge-dst edge)))))
  result)

(check-expect (bellman-ford A-GRAPH 1)
              (make-sssp (vector #f #t 1 1 3 6 3)
                         (vector 'inf 0 7 9 20 20 11)))
(check-expect (bellman-ford A-GRAPH 3)
              (make-sssp (vector #f 3 3 #t 3 6 3)
                         (vector 'inf 9 10 0 11 11 2)))


;; However, if we are clever (and if there are no negative edges, which will
;; mess this up), we can do it faster. In particular, we can relax each edge
;; only once, provided we do it in the right order. What’s the right order?

;; (We want to see the nodes in order from closest to the start to farthest.
;; That way, every time we look at an edge, we know that the source of the edge
;; already has its shortest path found.)

; dijkstra : WGraph Nat -> SSSPResult
; Computes SSSP from the given start node.
; ASSUMPTION: The graph contains no negative edges.
(define (dijkstra a-graph start-node)
  (define visited (make-vector (wgraph-nodes a-graph) #false))
  (define result  (make-sssp (make-vector (wgraph-nodes a-graph) #false)
                             (make-vector (wgraph-nodes a-graph) 'inf)))
  ; : Nat [Or Nat #false] -> [Or Nat #false]
  (define (find-nearest-unvisited current best-so-far)
    (if (= current (wgraph-nodes a-graph)) best-so-far
      (if (and (not (vector-ref visited current))
               (or (false? best-so-far)
                   (weight< (vector-ref (sssp-weights result) current)
                            (vector-ref (sssp-weights result) best-so-far))))
        (find-nearest-unvisited (add1 current) current)
        (find-nearest-unvisited (add1 current) best-so-far))))

  (vector-set! (sssp-preds result) start-node #true)
  (vector-set! (sssp-weights result) start-node 0)
  (let loop ()
    (define u (find-nearest-unvisited 0 #false))
    (unless (false? u)
      (for ([edge ((wgraph-edges a-graph) u)])
        (relax result u (wedge-weight edge) (wedge-dst edge)))
      (vector-set! visited u #true)
      (loop)))

  result)

(check-expect (dijkstra A-GRAPH 1)
              (make-sssp (vector #f #t 1 1 3 6 3)
                         (vector 'inf 0 7 9 20 20 11)))
(check-expect (dijkstra A-GRAPH 3)
              (make-sssp (vector #f 3 3 #t 3 6 3)
                         (vector 'inf 9 10 0 11 11 2)))

;; What’s the time complexity of our implementation of Dijkstra’s algorithm?
;; How might we improve it?
