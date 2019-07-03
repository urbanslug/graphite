#lang racket

(require libuuid)
(require racket/serialize)
(require graph)

;; TODO: represent bases in binary
(serializable-struct
 node*
 (sequence id offset edges)
 #:transparent)

;; adjacent-nodes is a set of edges out of the node

;; define node as a fn
;; a negative offset means the offset for the node isn't yet determined
(define (node sequence  [offset -1] (adj (set)))
  (node* sequence (uuid-generate) offset adj))


;; The graph is a *dict* of nodes and links
;; a graph here is an empty variation graph
;; we use a hashmap so that we don't duplicate nodes
(define empty-graph (make-immutable-hash))

;; Do we need this?
(define (add-node graph node)
  (hash-set graph (node*-id node) node))


;; TODO: do this without having to recreate the node
;; an edge is a UUID
;; rename to add-edge-to-node?
(define (add-edge n edge)
  (let* ([adj (node*-edges n)]
         [updated-adj (set-add adj edge)]

         [seq (node*-sequence n)]
         [id (node*-id n)]
         [offset (node*-offset n)])

    (node* seq id offset updated-adj)))

(define (add-adjacent-node graph node1 node2)
  (let* ([node2-id (node*-id node2)]
         [node1-id (node*-id node1)]
         ;; TODO: use better names
         ;; update node1
         [g* (hash-set graph node1-id (add-edge node1 node2-id))]
         ;; add node2 to the graph
         [g** (hash-set g* (node*-id node2) node2)])
    g**))

;; serialization
(define (write-graph g)
  (let* ([output-path "/Users/urbanslug/src/racket/graphite/data/output/graphiter"]
        [port (open-output-file output-path #:exists 'replace)])
    (write (serialize g) port)
    (close-output-port port)))

(define (read-graph)
  (let* ([input-path "/Users/urbanslug/src/racket/graphite/data/output/graphiter"]
         [port (open-input-file input-path)]
         [g (deserialize (read port))])
    (close-input-port port)
    g))

