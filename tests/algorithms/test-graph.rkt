#lang racket

(require rackunit)
(require "../../graphite/algorithms/graph.rkt")

(provide test-graph-generation)

(define ref "ACTGAATTTGTA")
(define var1 (cons 2 "GHGD"))
(define var2 (cons 4 #\C))

(define output-file "../../data/output/test.gv")

(define (test-graph-generation)
  (gen-sequence-graph empty 0 ref (list var1 var2)))
