#lang racket

(require racket/cmdline
         "./IO/fasta.rkt"
         "./IO/vcf.rkt"
         "./IO/gfa.rkt"
         "./algorithms/variation-graph.rkt")

(define output-file "./data/output/gfa/output.gfa")

(define (get-output-file filepath)
  (set! output-file filepath))

(define (gen-and-write-graph reference-file-path variation-file-path)
  (let* ([variations-map (read-fasta-file reference-file-path)]
        [key (hash-iterate-key variations-map (hash-iterate-first variations-map))])
    (write-gfa
     (vg->gfa-string
      (gen-vg (hash-ref variations-map key)
              (read-vcf variation-file-path)))
     output-file)))


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
