#lang racket

(require graph)

(define example-sequence "ATCGGGGTTTCCCCAAAA")
(define example-sequence-cont "GGGTTTTCCCCAAA")
(define variation "ATGTTTGGGAAA")
(define second-variation "TTGGAAATTGG")


(define my-graph (unweighted-graph/directed
                  (list (list example-sequence variation)
                        (list example-sequence second-variation)
                        (list variation example-sequence-cont)
                        (list second-variation example-sequence-cont))))

(define (write-graphviz g output-dir filename)
  (let* ([output-path (string-append output-dir filename)]
         [mkdir-and-file (lambda ()
                           (make-directory output-dir)
                           (open-output-file output-path))]
         [y (if (directory-exists? output-dir)
                (open-output-file output-path)
                (mkdir-and-file))])
    (graphviz g #:output y)))

(let ([output-dir "data/output/"]
      [gv-file "graph.gv"]
      (write-graphviz my-graph output-dir gv-file)))
