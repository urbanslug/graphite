#lang racket

(require graph)

(define example-sequence "ATCGGGGTTTCCCCAAAA")
(define example-sequence-cont "GGGTTTTCCCCAAA")
(define variation "ATGTTTGGGAAA")
(define second-variation "TTGGAAATTGG")
(define gv-file "graph.gv")

(define my-graph (unweighted-graph/directed
                  (list (list example-sequence variation)
                        (list example-sequence second-variation)
                        (list variation example-sequence-cont)
                        (list second-variation example-sequence-cont))))


(define (write-graphviz g filename)
  (let ([x (open-output-file filename)])
    (graphviz g #:output x)))

(write-graphviz my-graph gv-file)
