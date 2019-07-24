#lang racket

(require racket/cmdline
         "./IO/fasta.rkt"
         "./IO/vcf.rkt"
         "./IO/gfa.rkt"
         "./IO/dot.rkt"
         "./macros.rkt"
         "./structures/variations.rkt"
         "./algorithms/variation-graph.rkt")

(define output-file "./data/output/gfa/output.gfa")

(define (get-output-file filepath)
  (set! output-file filepath))

(define (gen-and-write-graph reference-file-path variation-file-path [output-format "gfa"])
  (let* ([fasta-hash (read-fasta-file reference-file-path)]
         [g (gen-vg (first (hash-values fasta-hash))
                    (sort (read-vcf variation-file-path) < #:key variation-position))])
    (match output-format
          ["dot" (vg->dot g output-file)]
          ["gfa" (vg->gfa g output-file)])))


(define (menu)
  (multi-command-line
   #:program "Graphite: variation graph tool."
   #:once-each
   [("-o" "--output") filepath
                      "Default is data/output/gfa/output.gfa"
                      (get-output-file filepath)]

   #:usage-help "Options:\n\tconstruct\n\tview\n\tupdate\n"

   #:subcommands
   ["construct"
    #:args (reference-file vcf-file)
    (gen-and-write-graph reference-file vcf-file)]
   ["view"
    #:args (reference-file vcf-file)
    (gen-and-write-graph reference-file vcf-file)]
   ["update"
    #:args (graph vcf-file)
    (gen-and-write-graph graph vcf-file)]
   ["all"
    #:args (reference-file vcf-file output-format)
    (gen-and-write-graph reference-file vcf-file output-format)]))

(define (main)
       (menu))

(main)
