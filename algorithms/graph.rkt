#lang racket

(require graph)

(provide my-graph)

(define example-sequence "ATCGGGGTTTCCCCAAAA")
(define example-sequence-cont "GGGTTTTCCCCAAA")
(define variation "ATGTTTGGGAAA")
(define second-variation "TTGGAAATTGG")


(define my-graph (unweighted-graph/directed
                  (list (list example-sequence variation)
                        (list example-sequence second-variation)
                        (list variation example-sequence-cont)
                        (list second-variation example-sequence-cont))))

;; single node graph




