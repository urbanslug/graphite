#lang racket


(require racket/cmdline)

(require "IO/graphviz.rkt")
(require "algorithms/graph.rkt")

(define output-file "graph.gv")

(define (get-output-file filepath)
  (set! output-file filepath))

(define (input-file g)
  (display g))

(define (overall-menu)
  (command-line
   #:program "Graphite"
   #:once-each
   [("-o" "--output") filepath
                      "Default is data/output/graph.gv"
                      (get-output-file filepath)]
   #:args (reference-file vcf-file)
   (cons reference-file vcf-file)))


(define (start)
  (write-graphviz my-graph "data/output/" output-file))


;; TODO: Implement a menu
(define (main)
  (overall-menu)
  (start))


(main)
