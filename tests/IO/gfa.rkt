#lang racket


(require rackunit)
(require "../../graphite/IO/gfa.rkt")
(require "../../graphite/IO/vcf.rkt")
(require "../../graphite/algorithms/variation-graph.rkt")


(define reference
  "ATTTCCGATAGATCGATATGCGATGCGATGCAGTAGC")

(define v1 (variation 10 "TGA"))
(define v2 (variation 15 "ACA"))
(define v3 (variation 30 "CCA"))
(define variations (list v1 v2 v3))

(define vg (gen-vg reference variations))

(write-gfa (vg->gfa-string vg))
