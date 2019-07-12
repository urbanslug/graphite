#lang racket

(provide gen-vg)


(require "./graph.rkt")
(require "./utils.rkt")
(require "../structures/graph.rkt")
(require "../structures/variations.rkt")


;; Force everything into strings for now at least
;; May have some duplicates but it won't matter
(define (gen-nodes ref vcf [prev-pos #f] [prev-nodes empty])
  (cond
    [(empty? vcf)
     (map
      (lambda (prev-node) (cons (create-node (list->string (take ref prev-pos)) #:offset 0) prev-node))
      (car prev-nodes))]

    [(and (number? prev-pos)
          (= (- (variation-position (last vcf)) 1) prev-pos))
     (let* ([current (last vcf)]
             ;; because the VCF is 1 indexed
             [pos (- (variation-position current) 1)]
             [seg-var (~a (variation-kmer current))]

             ;; How did this happen?
             ;; this is bad we should be using nodes here
             [orig-var (~a (first (car prev-nodes))) ]

             [alt-node (create-node seg-var #:offset pos)]
             [tail-node (cdr prev-nodes)])

       (append empty
               (list (cons alt-node tail-node))
                ;; connect current string ref to previous nodes

                ;; connect variation and original to "tail"
                ;; (list (cons alt-node tail-node) (cons orig-node tail-node))

                ;; call with rest
                (gen-nodes ref
                          (drop-right vcf 1)
                          pos
                          (cons (append (car prev-nodes) (list alt-node))
                                tail-node))))]

    [else
     (let* ([current (last vcf)]
             ;; because the VCF is 1 indexed
             [pos (- (variation-position current) 1)]
             [seg-var (~a (variation-kmer current))]


             ;; How did this happen?
             ;; this is bad we should be using nodes here
             [seg-ref (list->string (slice ref (+ 1 pos) prev-pos))]

             [orig-var (~a (list-ref ref pos))]

             [alt-node (create-node seg-var #:offset pos)]
             [tail-node (create-node seg-ref #:offset (+ 1 pos))]
             [orig-node (create-node orig-var #:offset pos)])

        (append empty
                ;; connect current string ref to previous nodes
                (map (lambda (prev-node) (cons tail-node prev-node))
                     (if (pair? prev-nodes)
                         (car prev-nodes)
                         empty))

                ;; connect variation and original to "tail"
                (list (cons alt-node tail-node) (cons orig-node tail-node))

                ;; call with rest
                (gen-nodes ref
                          (drop-right vcf 1)
                          pos
                          (cons (list orig-node alt-node) tail-node))))]))
