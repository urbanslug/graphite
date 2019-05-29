#lang racket

(define example-matrix
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

; has to be a 2x2 matrix

;; Implement a graph as an adjacency matrix.
(define (extract-from-matrix matrix row col)
  (list-ref (list-ref matrix row) col))

;; takes a row/list
(define (row->spaced-string row)
  (map (lambda (elem) (~a elem " ")) row))

;; pretty print the matrix
(define (pprint-matrix matrix)
  (map
   (lambda (i)
     (display i)
     (display "\n"))
   example-matrix))


