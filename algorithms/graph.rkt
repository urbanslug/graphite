#lang racket


(require graph)


(provide my-graph gen-local-graph ref-s var1 var2 var3)

(define example-sequence "ATCGGGGTTTCCCCAAAA")
(define example-sequence-cont "GGGTTTTCCCCAAA")
(define variation "ATGTTTGGGAAA")
(define second-variation "TTGGAAATTGG")

(define ref "ABCDEFGHIJKLMONOPQRSTUVWXYZ")

(define var1 (cons 4 "123"))
(define var2 (cons 12 "456"))
(define var3 (cons 18 "789"))

(define my-graph (unweighted-graph/directed
                  (list (list example-sequence variation)
                        (list example-sequence second-variation)
                        (list variation example-sequence-cont)
                        (list second-variation example-sequence-cont))))


(define other-graph (unweighted-graph/directed null))

(define output-file "../data/output/test.gv")

(define (write-graph g)
  (let* ([port (open-output-file output-file #:exists 'replace)])
    (graphviz g #:output port)
    (close-output-port port)))

(define ref-s "ABCDEFGHIJKLMONOPQRSTUVWXYZ")

(define ref-g
  (unweighted-graph/directed (list ref-s)))



(define (gen-initial reference variation-list)
  (gen-local-graph (list) reference variation-list))

(define (gen-local-graph local-graph previous-position ref var)
  (if (empty? var)
      (append local-graph
               (list (list (last (last local-graph)) ref))
               (list (list (last (list-ref local-graph (- (length local-graph) 2))) ref)))
      (match-let* ([x (first var)]
                   [(cons pos change) x]
                   [new-pos (- pos previous-position)]
                   [current (string (string-ref ref new-pos))]
                   [pre (substring ref 0 new-pos)]
                   )
        (gen-local-graph
         (if (empty? local-graph)
             (append local-graph
               (list (list pre current))
               (list (list pre change)))
             (append local-graph
               (list (list (last (last local-graph)) pre))
               (list (list (last (list-ref local-graph (- (length local-graph) 2))) pre))
               (list (list pre current))
               (list (list pre change)))

             )
         
         pos
         (substring ref (+ 1 new-pos))
         (if (empty? var) (var) (rest var))))))

;(gen-local-graph (list) 0 ref-s (list var1 var2 var3))

(define (insert-variation g variation)
  (match-let* ([(cons pos var) variation]
               [pre (substring ref 0 pos)]
               [post (substring ref pos)])
    (unweighted-graph/directed
     (list (list )))))
