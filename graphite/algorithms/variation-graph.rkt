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
       (lambda (prev-node)
         (cons (list->string (take ref prev-pos)) prev-node)) (car prev-nodes))

      ;; recursive call
      (let* ([current (last vcf)]
             [pos (car current)]
             [seg-var (~a (cdr current))]

             [seg-ref* (if (and (number? prev-pos) (= pos prev-pos))
                  (cdr prev-nodes)
                  (slice ref (+ 1 pos) prev-pos))]
             [seg-ref (if (string? seg-ref*)
                          seg-ref*
                          (list->string seg-ref*))]

             [orig-var (~a
                        (if (and (number? prev-pos) (= pos prev-pos))
                            (first (car prev-nodes))
                            (list-ref ref pos))) ]

             ;[alt-node (create-node seg-var #offset pos)]
             ;[adj (create-node seg-ref #offset (+ 1 pos))]
             ;[orig-node (create-node orig-var #offset pos)]
             )


        (append empty
                ;; connect current string ref to previous nodes
                (map (lambda (prev-node)  (cons seg-ref prev-node))
                     (if (and (number? prev-pos) (not (= pos prev-pos)) (pair? prev-nodes))
                         (car prev-nodes)
                         empty))

                ;; connect variation and original to "tail"

                (list (cons seg-var seg-ref) (cons orig-var seg-ref))

                ;; call with rest
                (gen-nodes ref
                          (drop-right vcf 1)
                          pos
                          (cons (if (and (number? prev-pos) (= pos prev-pos))
                                    (remove-duplicates (append (list orig-var seg-var) (car prev-nodes)))
                                    (remove-duplicates (list orig-var seg-var)))
                                seg-ref))))))


;; Generate a graph from a reference and VCF
(define (gen-vg reference variation-list)
  (gen-directed-graph
   (gen-nodes (string->list reference) variation-list)))
