#lang racket

(provide gen-vg)


(require "./graph.rkt")
(require "./utils.rkt")
(require "../structures/graph.rkt")
(require "../structures/variations.rkt")

(define (cap ref prev-pos prev-nodes)
  (map
   (lambda (prev-node)
     (cons (create-node (list->string (take ref prev-pos)) #:offset 0)
           prev-node))
   (car prev-nodes)))

(define (gen-alt-node vcf)
  (let* ([current (last vcf)]
         ;; because the VCF is 1 indexed
         [pos (- (variation-position current) 1)]
         [alt-kmer (~a (variation-kmer current))]
         [alt-node (create-node alt-kmer #:offset pos)])
    (values alt-node pos)))

(define (handle-duplicate ref vcf [prev-pos #f] [prev-nodes empty])
  (let-values ([(alt-node pos) (gen-alt-node vcf)]
               [(tail-node) (cdr prev-nodes)])
    (append empty
            (list (cons alt-node tail-node))

            ;; call with rest
            (gen-nodes ref
                       (drop-right vcf 1)
                       pos
                       (cons (append (car prev-nodes) (list alt-node))
                             tail-node)))))


(define (handle-unique ref vcf [prev-pos #f] [prev-nodes empty])
  (let*-values ([(alt-node pos) (gen-alt-node vcf)]

                [(seg-ref) (list->string (slice ref (+ 1 pos) prev-pos))]
                [(tail-node) (create-node seg-ref #:offset (+ 1 pos))]

                [(orig-var) (~a (list-ref ref pos))]
                [(orig-node) (create-node orig-var #:offset pos)])
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
                          (cons (list orig-node alt-node) tail-node)))))

;; Force everything into strings for now at least
;; May have some duplicates but it won't matter
(define (gen-nodes ref vcf [prev-pos #f] [prev-nodes empty])
  (cond
    [(empty? vcf) (cap ref prev-pos prev-nodes)]

    [(and (number? prev-pos) (= (- (variation-position (last vcf)) 1) prev-pos))
     (handle-duplicate ref vcf prev-pos prev-nodes)]

    [else (handle-unique ref vcf prev-pos prev-nodes)]))


;; Generate a graph from a reference and VCF
(define (gen-vg reference variation-list)
  (gen-directed-graph empty-graph
                      (gen-nodes (string->list reference) variation-list)))
