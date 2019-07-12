#lang racket

(provide gen-vg)


(require "./graph.rkt")
(require "./utils.rkt")
(require "../structures/graph.rkt")
(require "../structures/variations.rkt")

;; Force everything into strings for now at least
;; May have some duplicates but it won't matter
(define (gen-nodes ref
                  vcf
                  [prev-pos #f]
                  ;; a pair of prev-nodes and prev tail
                  ;; prev-nodes should be a set  to avoid duplicates
                  [prev-nodes empty])

  (if (empty? vcf)
      ;; base case
      (map
       (lambda (prev-node) (cons (create-node (list->string (take ref prev-pos)) #:offset 0)
                                 prev-node))
       (car prev-nodes))

      ;; Recursive call
      (let* ([current (last vcf)]
             ;; because the VCF is 1 indexed
             [pos (- (variation-position current) 1)]
             [seg-var (~a (variation-kmer current))]

             [seg-ref* (if (and (number? prev-pos) (= pos prev-pos))
                           (cdr prev-nodes)
                           (slice ref (+ 1 pos) prev-pos))]

             ;; How did this happen?
             ;; this is bad we should be using nodes here
             [seg-ref (cond [(string? seg-ref*) seg-ref*]
                            [(list? seg-ref*) (list->string seg-ref*)]
                            ;; assume that it's a node or  a string
                            [else seg-ref*
                                  ])]

             [orig-var (~a
                        (if (and (number? prev-pos) (= pos prev-pos))
                            (first (car prev-nodes))
                            (list-ref ref pos))) ]

             [alt-node (create-node seg-var #:offset pos)]
             [tail-node (if (string? seg-ref)
                            (create-node seg-ref #:offset (+ 1 pos))
                            seg-ref)]
             [orig-node (create-node orig-var #:offset pos)])

        (append empty
                ;; connect current string ref to previous nodes
                (map (lambda (prev-node)  (cons tail-node prev-node))
                     (if (and (number? prev-pos) (not (= pos prev-pos)) (pair? prev-nodes))
                         (car prev-nodes)
                         empty))

                ;; connect variation and original to "tail"
                (list (cons alt-node tail-node) (cons orig-node tail-node))

                ;; call with rest
                (gen-nodes ref
                          (drop-right vcf 1)
                          pos
                          (cons (if (and (number? prev-pos) (= pos prev-pos))
                                    (remove-duplicates (append (list orig-node alt-node) (car prev-nodes)))
                                    (remove-duplicates (list orig-node alt-node)))
                                tail-node))))))


;; Generate a graph from a reference and VCF
(define (gen-vg reference variation-list)
  (gen-directed-graph empty-graph
   (gen-nodes (string->list reference) variation-list)))

