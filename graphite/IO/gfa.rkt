#lang racket

(require "../algorithms/graph.rkt")
(require "../algorithms/variation-graph.rkt")
(require "./vcf.rkt")
(require "./fasta.rkt")
(require "../structures/graph.rkt")

(provide write-gfa
         vg->gfa-string)

(define (node->gfa-node id n)
  (let* ([seq         (node-segment n)]
         [edges       (node-edges n)]
         [format-edge (lambda (l) (format "\nL\t~a\t+\t~a\t+\t0M" id l))]
         [edges*      (string-join (set-map edges format-edge))])
    (string-append "\nS\t" id "\t" seq edges*)))

(define (vg->gfa-string g)
  (let ([gfa-header "H\tVN:Z:1.0"])
    (string-append
     gfa-header
     (string-join (hash-map g node->gfa-node)))))

(define (write-gfa s output-fp)
  (let* ([output-path output-fp]
         [port (open-output-file output-path #:exists 'replace)])
    (fprintf port s)
    (close-output-port port)))
