#lang racket

(provide update-vg)

(require "../structures/variations.rkt"
         "../structures/graph.rkt"
         "./variation-graph.rkt"
         "./graph.rkt"
         "../IO/gfa.rkt"
         "./utils.rkt"
         )



(struct tuple (f s)
  #:methods gen:custom-write
  [(define (write-proc tuple port mode)
     (let* ([f (tuple-f tuple)]
            [s (tuple-s tuple)])
       (fprintf port "(, ~a ~a)" f s)))])

(define (, f s)
  (tuple f s))

;; TODO: move to graph
(define (previous-nodes g id)
  (flatten
   (hash-map
    g
    (lambda (k v)
      (if (in? id (node-edges v))
          k
          empty)))))

;; TODO: move to graph
(define (next-nodes g id)
  (node-edges (get-node g #:id id)))


(define (exists-node-with-offset? g offset)
  (foldr (lambda (f s) (or f s))
         #f
         (hash-map
          g
          (lambda (k n)
            (if (=(node-offset n) offset)
                #t
                #f)))))

;; TODO: move to graph
(define (get-node-id-with-offset g offset)
  (first
   (flatten
    (hash-map
     g
     (lambda (k n)
       (if (=(node-offset n) offset)
           k
           empty))))))

(define (update-previous g variation)
  (let* ([offset (- (variation-position variation) 1)]

         [n-id (get-node-id-with-offset g offset)]
         [p (previous-nodes g n-id)] ;; a list of previous node ids
         [n (next-nodes g n-id)] ;; a list of next node ids

         [variation-node (create-node (variation-kmer variation)
                                      #:offset (- (variation-position variation) 1)
                                      #:edges n)]
         [variation-node-id (node-id variation-node)]
         ;; insert node into previous nodes
         ;; p* is a list of nodes
         [p* (map (lambda (n*) (add-edge (get-node g #:id n*) variation-node-id) ) p)])
    (foldr
     (lambda (x accum) (add-node accum x))
     g
     (cons variation-node p*))))

(define (insert-variation g variation)
  (let ([offset (- (variation-position variation) 1)])
    (if (exists-node-with-offset? g offset)
        ;; I realize there's double computation here
        (update-previous g variation)
        1
        )))


(define (update-vg g variations)
  (foldr
   (lambda (v accum) (insert-variation accum v))
   g
   variations))

(define reference "0123456789")

(define v1 (variation 3 "TGA"))
(define v2 (variation 5 "ACA"))
(define v3 (variation 8 "CCA"))
(define v4 (variation 8 "GAT"))

(define variations (list v1 v2 v3 v4))


(define g (gen-vg reference variations))
; (display-graph g)


(vg->gfa
 g
 "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa")

;(previous-nodes g "3bab7138267925ef5f7f1e8ac0c2c21245f7bf6cbe46f2c8f12cc26214593ef0")

;(exists-node-with-offset? g 6)

(define v5 (variation 3 "UAG"))

(define g* (update-vg g (list v5)))

(vg->gfa
 g*
 "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa")
