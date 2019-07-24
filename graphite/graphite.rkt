#lang racket

(require racket/cmdline
         "./IO/fasta.rkt"
         "./IO/vcf.rkt"
         "./IO/gfa.rkt"
         "./IO/dot.rkt"
         "./structures/variations.rkt"
         "./algorithms/variation-graph.rkt")

(define output-file "./data/output/gfa/output.gfa")

(define (get-output-file filepath)
  (set! output-file filepath))

(define (gen-and-write-graph reference-file-path variation-file-path output-format)
  (let* ([fasta-hash (read-fasta-file reference-file-path)]
         [g (gen-vg (first (hash-values fasta-hash))
                    (sort (read-vcf variation-file-path) < #:key variation-position))])
    (cond output-format
          ["dot" (vg->dot g output-file)]
          ["gfa" (vg->gfa g output-file)])))

(define (overall-menu)
  (command-line
   #:program "Graphite"
   #:once-each
   [("-o" "--output") filepath
                      "Default is data/output/gfa/output.gfa"
                      (get-output-file filepath)]
   #:args (reference-file vcf-file)
   (gen-and-write-graph reference-file vcf-file)))

(define (main)
  (overall-menu))

(main)
