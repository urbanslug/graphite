#lang racket


(require graph)

(require "../IO/vcf.rkt")
(require "../IO/fasta.rkt")

(provide gen-sequence-graph gen-and-write-graph write-graph)

(define (gen-sequence-graph local-graph previous-position ref var)
  (if (empty? var)
      ;; add the tail
      (let ([ref-var (variation (+ 2 previous-position) ref)])
        (append local-graph
                (list (list (last (last local-graph)) ref-var))
                (list (list (last (list-ref local-graph (- (length local-graph) 2))) ref-var))))
      ;; add nodes
      (match-let* ([x (first var)]
                   [(cons pos changex) x]
                   [g (if (char? changex) (string changex) changex)]
                   [change (variation (+ 1 pos) g)]
                   [new-pos (- pos previous-position)]
                   [current  (variation (+ 1 pos) (string (string-ref ref new-pos)))]
                   [pre (variation pos (substring ref 0 new-pos))])
        (gen-sequence-graph
         (if (empty? local-graph)
             ;; the tree is empty
             (append local-graph
               (list (list pre current))
               (list (list pre change)))
             (append local-graph
               (list (list (last (last local-graph)) pre))
               (list (list (last (list-ref local-graph (- (length local-graph) 2))) pre))
               (list (list pre current))
               (list (list pre change))))
         pos
         (substring ref (+ 1 new-pos))
         (if (empty? var) (var) (rest var))))))


(define (extract-ref-string fp)
     (let* ([references (read-fasta-file fp)]
                      [sequences  (hash-keys references)]
                      [seq (first sequences)])
            (hash-ref references seq)))

(define (gen-variation-list var-hash)
  (let ([hash-list (hash->list var-hash)])
    (sort hash-list (lambda (x y) (< (car x) (car y))))))


(define (extract-var-list fp)
  (let ([var-hash (read-vcf fp)])
    (gen-variation-list var-hash)))

(define (gen-sequence-list ref-fp var-fp)
  (let ([var-list (extract-var-list var-fp)]
        [ref (extract-ref-string ref-fp)])
    (gen-sequence-graph empty 0 ref var-list)))



(define (write-graph g output-file-path)
  (let* ([port (open-output-file output-file-path #:exists 'replace)])
    (graphviz g #:output port)
    (close-output-port port)))

(define (gen-and-write-graph reference variation fp)
  (let* ([seq-list (gen-sequence-list reference variation)]
        [graph (unweighted-graph/directed seq-list)])
    (write-graph graph fp)))

