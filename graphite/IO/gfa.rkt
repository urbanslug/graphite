#lang racket

(require "../algorithms/graph.rkt")
(require "../algorithms/variation-graph.rkt")
(require "./vcf.rkt")
(require "../structures/graph.rkt")


(define (node->gfa-node id n)
  (let* ([seq (node-segment n)]
        [edges (node-edges n)]
        [edges* (string-join
                 (set-map edges
                          (lambda (l) (string-append "\nL\t" id "\t+\t" l "\t+\t" "0M"))))])
    (string-append "\nS\t" id "\t" seq edges*)))

(define (vg->gfa-string g)
  (let ([gfa-header "H\tVN:Z:1.0"])
    (string-append
     gfa-header
     (string-join (hash-map g  node->gfa-node)))))


(define (write-gfa s)
  (let* ([output-path "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa"]
         [port (open-output-file output-path #:exists 'replace)])
    (fprintf port s)
    (close-output-port port)))
