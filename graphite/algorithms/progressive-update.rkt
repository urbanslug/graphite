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

(define (exists-node-with-greater-offset? g offset)
  (foldr (lambda (f s) (or f s))
         #f
         (hash-map
          g
          (lambda (k n)
            (if (> (node-offset n) offset)
                #t
                #f)))))

(define (exists-node-with-lower-offset? g offset)
  (foldr (lambda (f s) (or f s))
         #f
         (hash-map
          g
          (lambda (k n)
            (if (< (node-offset n) offset)
                #t
                #f)))))

(define (get-node-ids-with-offset g offset)
  (flatten
   (hash-map
    g
    (lambda (k n)
      (if (=(node-offset n) offset)
          k
          empty)))))

;; TODO: move to graph
(define (get-node-id-with-offset g offset)
  (first
   (flatten
    (hash-map
     g
     (lambda (k n)
       (if (= (node-offset n) offset)
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

;; check is like >, =, < etc
;; TODO: this name is wrong
(define (get-nodes-to-split g offset check)
  (sort (flatten
               (hash-map g
                         (lambda (k n)
                           (if (check (node-offset n) offset)
                               n
                               empty))))
              check
              #:key node-offset))

(define (get-offset v)
  (- (variation-position v) 1))

;; this is trying to split the previous instead of the current node
(define (split-and-insert g n v)
  (let* ([n*       (if (node? n) n (get-node g #:id n))]
         [n*-id (node-id n*)]
         [n*-offset (node-offset n*)]
         [k (previous-nodes g n*-id)]
         [segment  (node-segment n*)]
         [offset   (get-offset v)]
         [split-index (- offset n*-offset)]

         [f (sublist (string->list segment) 0 split-index)]
         [s (sublist (string->list segment) (+ 1 split-index))]
         ;; not actually original but you get it.
         [original (string-ref segment split-index)]

         [f-node (create-node (list->string f) #:offset n*-offset)]
         [s-node (create-node (list->string s) #:offset (+ 1 split-index) #:edges (node-edges n*))]
         [alt-node (create-node (variation-kmer v) #:offset split-index)]
         [original-node (create-node (string original) #:offset split-index)]

         ;; remove old edge
         [g* (foldr
              (lambda (i accum) (remove-adjacent-node accum (get-node g #:id i) n*))
              g
              k)]

         ;; add new edge
         [g** (foldr
               (lambda (i accum) (add-adjacent-node accum (get-node g #:id i) f-node))
               g*
               k)]

         [x (list (cons f-node alt-node)
                  (cons f-node original-node)
                  (cons alt-node s-node)
                  (cons original-node s-node))])
    (gen-directed-graph g** x)))

(define (split-and-insert* g n v)
  (let* ([n*       (if (node? n) n (get-node g #:id n))]
         [n*-id (node-id n*)]
         [n*-offset (node-offset n*)]
         [n*-segment  (node-segment n*)]

         [offset (get-offset v)]
         [split-index (- offset n*-offset)]

         [f (sublist (string->list n*-segment) 0 split-index)]
         [s (sublist-inc (string->list n*-segment) (+ 1 split-index) (- (string-length n*-segment) 1) )]
         [original (string-ref n*-segment split-index)]

         [f-node (create-node (list->string f) #:offset n*-offset)]
         [s-node (create-node (list->string s) #:offset (+ 1 split-index) #:edges (node-edges n*))]
         [alt-node (create-node (variation-kmer v) #:offset split-index)]
         [original-node (create-node (string original) #:offset split-index)]

         [g* (hash-remove g n*-id)]

         [x (list (cons f-node alt-node)
                  (cons f-node original-node)
                  (cons alt-node s-node)
                  (cons original-node s-node))])
    (gen-directed-graph g* x)))

(define (split-and-insert-next g variation)
  (let* ([offset (- (variation-position variation) 1)]
         [first-node (first (get-nodes-to-split g offset <))]

         ;; these are the nodes we wish to split
         [node-ids-with-offset (get-node-ids-with-offset g (node-offset first-node))])
    ;; split and insert into node=ids-with-offset
    (foldr (lambda (p* accum) (split-and-insert* g p* variation))
           g
           node-ids-with-offset)))

(define (split-and-insert-previous g variation)
  (let* ([offset (- (variation-position variation) 1)]
         [last-node (last (get-nodes-to-split g offset <))]

         ;; these are the nodes we wish to split
         [node-ids-with-offset (get-node-ids-with-offset g (node-offset last-node))])
    (foldr (lambda (p* accum) (split-and-insert g p* variation))
           g
           node-ids-with-offset)))

(define (insert-variation g variation)
  (let ([offset (- (variation-position variation) 1)])
    (cond
      [(exists-node-with-offset? g offset) (update-previous g variation)]
      [(exists-node-with-greater-offset? g offset)
       (split-and-insert-next g variation)]
      [else (split-and-insert-previous g variation)])))

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

(define variations (list v4))

(define g (gen-vg reference variations))

(define g* (update-vg g (list v1)))

; (display-graph g)
; (vg->gfa g "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa")

;(display-graph g*)
;(vg->gfa g* "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa")

(define g** (update-vg g* (list v3)))
(vg->gfa g** "/Users/urbanslug/src/racket/graphite/data/output/gfa/test.gfa")
