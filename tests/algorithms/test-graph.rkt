#lang racket

(require rackunit)
(require "../../graphite/algorithms/graph.rkt")
(require "../../graphite/structures/graph.rkt")


(define x (create-node "ABISD" #:offset 12))
(define y (create-node "ABOPD" #:offset 13))
(define z (create-node "OPYUD" #:offset 15))

(define dag (gen-directed-graph empty-graph (list (cons x y) (cons y z))))

(print-graph dag)
