#lang racket

(require "IO/graphviz.rkt")
(require "algorithms/graph.rkt")

;; TODO: Use racket cmdline
;; TODO: Implement a menu
(define (main)
  (display "VG implementation in racket"))

(let ([output-dir "data/output/"]
      [gv-file "graph.gv"])
  (write-graphviz my-graph output-dir gv-file))
