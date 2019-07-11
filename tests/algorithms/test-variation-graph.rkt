#lang racket

;; examples
(define reference
  "ATTTCCGATAGATCGATATGCGATGCGATGCAGTAGC")

(define n1 (node reference 3))
(define n2 (node reference  5))
(define n3 (node  reference  10))

;; n -> n2
(define g1 (add-adjacent-node empty-graph n1 n2))

;; n -> n2 and n2->n3
(define g2 (add-adjacent-node g1  n1 n3))
;; n -> n2 n2 -> n3  n -> n3
(define g3 (add-adjacent-node g2 n2  n3))


(define g4 (remove-adjacent-node g3 n1 n2))


(write-graph g4)
(read-graph)

(define variation-file-path
  "/Users/urbanslug/src/racket/graphite/data/RSV/refererence_and_vcf_file/H_3801_22_04.freebayes.vcf")
(define reference-file-path
  "/Users/urbanslug/src/racket/graphite/data/RSV/refererence_and_vcf_file/9465113.fa")

(gen-vg (hash-ref (read-fasta-file reference-file-path) "9465113") (read-vcf variation-file-path))



;; Crap
(define reference
  "ATTTCCGATAGATCGATATGCGATGCGATGCAGTAGC")

(define v1 (variation 10 "TGA"))
(define v2 (variation 15 "ACA"))
(define v3 (variation 30 "CCA"))
(define variations (list v1 v2 v3))

(define n1 (create-node reference #:offset 3))
(define n2 (create-node reference #:offset 5))
(define n3 (create-node reference #:offset 10))
(define n4 (create-node reference #:offset 14))
(define n5 (create-node reference #:offset 17))


