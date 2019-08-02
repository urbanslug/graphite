#lang racket

(require racket/cmdline
         "./IO/fasta.rkt"
         "./IO/vcf.rkt"
         "./IO/gfa.rkt"
         "./IO/graph.rkt"
         "./IO/dot.rkt"
         "./macros.rkt"
         "./structures/variations.rkt"
         "./algorithms/variation-graph.rkt"
         "./algorithms/progressive-update.rkt"
         "./algorithms/poa.rkt"
         )

(define output-file "./data/output/gfa/output.gfa")
(define output-format "gfa")


(define (set-output-file filepath)
  (set! output-file filepath))

(define (set-output-format f)
  (set! output-format f))

(define (gen-output g)
  (match output-format
    ["dot" (vg->dot g output-file)]
    ["gfa" (vg->gfa g output-file)]
    ["gra" (vg->gra g output-file)]))


(define (gen-and-write-graph reference-file-path variation-file-path)
  (let* ([fasta-hash (read-fasta-file reference-file-path)]
         [v (sort (read-vcf variation-file-path) < #:key variation-position)]
         [g (gen-vg (first (hash-values fasta-hash)) v)])
    (gen-output g)))

(define (update-graph g v)
  (let* ([x (gra->vg g)]
        [v* (sort (read-vcf v) < #:key variation-position)]
        [g (update-vg x v*)])
    (gen-output g)))

(define (viz-graph g)
  (let ([x (gra->vg g)])
    (gen-output x)))

(define (align-reads reads-fp)
  (let ([fasta-hash (read-fasta-file reads-fp)])
    (align fasta-hash)))

(define (menu)
  (multi-command-line
   #:program "Graphite: variation graph tool."
   #:once-each
   [("-o" "--output") filepath
                      "Default is data/output/gfa/output.gfa"
                      (set-output-file filepath)]
   [("-f" "--format") output-format
                      "Output format: gfa, gra or dot. Default is gfa"
                      (set-output-format output-format)]

   #:usage-help "Options:\n\tconstruct\n\tupdate\n"

   #:subcommands
   ["construct"
    #:args (reference-file vcf-file)
    (gen-and-write-graph reference-file vcf-file)]
   ["update"
    #:args (graph-filepath vcf-file)
    (update-graph graph-filepath vcf-file)]
   ["view"
    #:args (graph-filepath)
    (viz-graph graph-filepath)]
   ["align"
    #:args (reads-filepath)
    (align-reads reads-filepath)]))

(define main menu)

(main)
