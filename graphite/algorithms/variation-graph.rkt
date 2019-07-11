#lang racket

(provide gen-vg)


(require "./graph.rkt")
(require "../structures/graph.rkt")
(require "../IO/vcf.rkt")
(require "./utils.rkt")

;; Generate a graph from a reference and VCF
;; list -> list -> hash map
(define (gen-vg-uncapped ref vcf [g empty-graph])
  (foldr
   (lambda (variation* accum)
     (let* ([g*             (hash-ref accum 'graph)]
            [prev-split-pos (hash-ref accum 'prev-split-pos)]
            [prev-nodes     (hash-ref accum 'prev-nodes)]

            [kmer*    (variation-kmer variation*)]
            [pos      (variation-position variation*)]
            [next-pos (+ pos 1)]

            [fragment (slice ref pos (if prev-split-pos prev-split-pos #f))]

            ;; fragment
            [n1 (create-node (list->string fragment) #:offset next-pos)]
            ;; seq at point
            [n2 (create-node (string (list-ref ref pos)) #:offset next-pos)]
            ;; variation
            [n3 (create-node kmer* #:offset next-pos)]

            [x (if (empty? prev-nodes)
                   empty
                   (map (lambda (x) (cons n1 x)) prev-nodes))]

            [nodes (append (list (cons n2 n1) (cons n3 n1)) x)]
            [y (gen-directed-graph g* nodes)])

       (hash
        'graph y
        'prev-nodes (list (hash-ref y (node-id n2))
                          (hash-ref y (node-id n3)))
        'prev-split-pos pos)))

   (hash 'graph g
         'prev-nodes empty
         ;; where we last split the ref
         'prev-split-pos #f)
   vcf))

(define (gen-vg ref vcf [g empty-graph])
  (let* ([uncapped (gen-vg-uncapped (string->list ref) vcf g)]
         [cap-frag (slice (string->list ref) 0 (variation-position (first vcf)))]
         [cap-node (create-node (list->string  cap-frag) #:offset 0)])

    (gen-directed-graph
     (hash-ref uncapped 'graph)
     (list (cons cap-node (first (hash-ref uncapped 'prev-nodes)))
           (cons cap-node (second (hash-ref uncapped 'prev-nodes)))))))
