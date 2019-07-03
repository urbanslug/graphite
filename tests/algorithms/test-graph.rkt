#lang racket

(require rackunit)
(require "../../graphite/algorithms/graph.rkt")

;; TODO: remove
(require graph)

(provide test-graph-generation)

(define ref "ACTGAATTTGTA")
(define var1 (cons 2 "GGGA"))
(define var2 (cons 4 #\C))

(define output-file "../../data/output/test.gv")

(define (test-graph-generation)
  (gen-sequence-graph empty 0 ref (list var1 var2)))

(define (test-graph-production)
  (let* ([x (gen-sequence-graph empty 0 ref (list var1 var2))]
        [g (unweighted-graph/directed x)])
    (write-graph g output-file)))