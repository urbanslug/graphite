#lang racket

(require libuuid)
(require racket/serialize)
(require file/sha1)
;;(require graph)
(require "../IO/fasta.rkt")
(require "../IO/vcf.rkt")
(require "./utils.rkt")

(provide node empty-graph add-adjacent-node node*)

;; TODO: represent bases in binary
(serializable-struct
 node*
 (sequence id offset edges)
 #:transparent)

;; sh256 hash as a hex hash
(define (string->hash s)
  (bytes->hex-string (sha1-bytes (string->bytes/utf-8 s))))

;; adjacent-nodes is a set of edges out of the node

;; Should I replace UUID with a cryptographic hash of the sequence?
;; a negative offset means the offset for the node isn't yet determined
;; TODO: use bytestrings for the sequence
(define (node seq #:id [id #f] #:offset [offset #f] #:edges [adj (set)])
  (let* ([s (if offset
               (string-append seq "+" (number->string offset))
               seq)]
         [id* (if id id (string->hash s)) ])
    (node* seq id* offset adj)))

;; doesn't mutate the node
;; not allowed to change node id
;; if you want to do that remove the node and generate a new one?
;; What about all adjacent nodes? how do we update those?
;; how can we make this faster?
;; TODO: remove this fn or make sure it doesn't cause IDs to break
(define (update-node n #:sequence [seq #f] #:offset [offset #f] #:edges [edges #f])
  (let ([s (or seq (node*-sequence n))]
        [i (node*-id n)]
        [o (or offset  (node*-offset n))]
        [e (or edges (node*-edges n))])
    (node s #:id i #:offset o #:edges e)))

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
;; O(1)
(define (add-edge n edge)
  (update-node n #:edges (set-add (node*-edges n) edge)))

(define (remove-edge n edge)
  (update-node n #:edges (set-remove (node*-edges n) edge)))

;; return the node or false
;; takes an id or a node
(define (get-node g #:node [n #f] #:id [id #f])
  (if n
      (hash-ref g (node*-id n) (lambda () n))
      (hash-ref g id (lambda () #f))))

;; O(1)
(define (add-adjacent-node graph node1 node2)
  (let* ([node2-id (node*-id node2)]
         [node1-id (node*-id node1)]
         ;; TODO: use better names
         ;; update node1
         ;; [g* (hash-set graph node1-id (add-edge  node2-id))]
         [g* (hash-set graph
                       node1-id
                       (add-edge (get-node graph #:node node1)
                                 node2-id))]
         ;; add node2 to the graph
         [g** (hash-set g* (node*-id node2) node2)])
    g**))

;; what about removing the edge from the graph?
(define (remove-adjacent-node graph node1 node2)
  (let* ([node2-id (node*-id node2)]
         [node1-id (node*-id node1)]
        ;; update node1
         [g* (hash-set graph
                       node1-id
                       (remove-edge (get-node graph #:node node1)
                                    node2-id))]
        ;;remove node2 from the graph if it's still there
        [g** (hash-remove g* node2-id)])
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



;; Move to GFA



(define (node->gfa-node id n)

  (let* ([seq (node*-sequence n)]
        [edges (node*-edges n)]
        [edges* (string-join (set-map edges (lambda (l) (string-append "\nL\t" id "\t+\t" l "\t+\t" "0M"))))])
    (string-append "\nS\t" id "\t" seq edges*)))

(define (vg->gfa-string g)
  (let ([gfa-header "H\tVN:Z:1.0"])
    (string-append gfa-header
                   (string-join (hash-map g  node->gfa-node)))))


(define (write-gfa s)
  (let* ([output-path "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa"]
         [port (open-output-file output-path #:exists 'replace)])
    (fprintf port s)
    (close-output-port port)))

;; (write-gfa (vg->gfa-string g3))


(define reference
  "ATTTCCGATAGATCGATATGCGATGCGATGCAGTAGC")


(define queue empty)
(define growth-nodes empty)

(define (extract-fragment ref start stop)
  (if stop
      (sublist ref start stop)
      (drop ref start)))

(define (gen-directed-graph g l)
  (foldr (lambda (node-list g*)
           (add-adjacent-node g* (first node-list) (second node-list)))
         g
         l))

;; Generate a graph from a reference and VCF
;; list -> list -> hash map
(define (gen-vg ref vcf [g empty-graph])
  ;; VCF is expected to be small

  (foldr
   (lambda (variation* g*)
     (let* ([pos      (variation-position variation*)]
            [next-pos (+ pos 1)]
            [prev-pos (- pos 1)]
            [kmer* (variation-kmer variation*)]
            [frag (extract-fragment ref pos (if (empty? queue) #f (last queue)))]
            ;; fragment
            [n1 (node (list->string frag) #:offset next-pos)]
            ;; seq at point
            [n2 (node (string (list-ref ref pos)) #:offset next-pos)]
            ;; variation
            [n3 (node kmer* #:offset next-pos)]

            [g-nodes (if (empty? growth-nodes)
                         empty
                         (map (lambda (x) (list n1 x)) growth-nodes))]
            [g1 (gen-directed-graph g* (append (list (list n2 n1) (list n3 n1)) g-nodes))])
       (append queue (list frag))
       (append growth-nodes (list n2 n3))
       g1))
   g
   vcf))

(define v1 (variation 10 "TGA"))
(define v2 (variation 15 "ACA"))
(define v3 (variation 30 "CCA"))
(define variations (list v1 v2 v3))

(define vg (gen-vg (string->list reference) variations))
(write-gfa (vg->gfa-string vg))


