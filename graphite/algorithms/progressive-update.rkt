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

;; sort nodes asc
(define (sort-nodes-by-offset g)
  (sort (hash-values g)
        <
        #:key node-offset))

(define (nodes-with-largest-offset g)
  (let* ([sorted-nodes (sort-nodes-by-offset g)]
         [last-node (last sorted-nodes)])
    (filter (lambda (n) (= (node-offset n) (node-offset last-node)))
            sorted-nodes)))


;; may lead to broken edges
(define (remove-node g n)
  (hash-remove g (if (node? n) (node-id n) n)))

(define (split-single-tail-node g n v)
  (let* ([n* (if (node? n) n (get-node g n))]
         [p  (previous-nodes g (node-id n*))]

        [offset     (get-offset v)]
        [n*-segment (node-segment n*)]
        [n*-offset  (node-offset n*)]
        [insert-pos (- offset n*-offset)]

        [f (sublist (string->list n*-segment) 0 insert-pos)]
        [s (slice (string->list n*-segment) (+ 1 insert-pos))]
        [o (string-ref n*-segment insert-pos )]

        [f-node   (create-node (list->string f) #:offset n*-offset)]
        [s-node   (create-node (list->string s) #:offset n*-offset)]
        [o-node   (create-node (string o) #:offset n*-offset)]
        [alt-node (create-node (variation-kmer v) #:offset n*-offset)]

        ;; remove links to old node
        [g* (foldr
             (lambda (p* accum)
               (remove-adjacent-node accum
                                     (get-node g #:id p*)
                                     n*))
             g
             p)]

        ;; point to new f-node
        [g** (foldr
              (lambda (p* accum) (add-adjacent-node accum
                                                    (get-node g #:id p*)
                                                    f-node))
             g*
             p)])

    (gen-directed-graph
     g**
     (list (cons f-node o-node)
           (cons f-node alt-node)
           (cons alt-node s-node)
           (cons o-node s-node)))))

(define (insert-at-the-end g v)
  ;; there could be multiple nodes at the end
  (foldr (lambda (n accum) (split-single-tail-node accum n v))
         g
         (nodes-with-largest-offset g)))

(define (split-single-head-node g n v)
  (let* ([n* (if (node? n) n (get-node g #:id n))]
         [p (previous-nodes g (node-id n*))]

         [offset     (get-offset v)]
         [n*-segment (node-segment n*)]
         [n*-offset  (node-offset n*)]
         [insert-pos (- offset n*-offset)]

         [f (sublist (string->list n*-segment) 0 insert-pos)]
         [s (slice (string->list n*-segment) (+ 1 insert-pos))]
         [o (string-ref n*-segment insert-pos)]

         [f-node   (create-node (list->string f) #:offset n*-offset)]
         [s-node   (create-node (list->string s) #:offset (+ 1 offset) #:edges (node-edges n*))]
         [o-node   (create-node (string o) #:offset offset)]
         [alt-node (create-node (variation-kmer v) #:offset offset)]

         ;; point to new f-node
         [g* (foldr
               (lambda (p* accum) (add-adjacent-node accum
                                                     (get-node g #:id p*)
                                                     f-node))
               g
               p)]


         ;; remove links to old node
         [g** (foldr
              (lambda (p* accum)
                (remove-adjacent-node accum
                                      (get-node g #:id p*)
                                      n*))
              g*
              p)]

         ;; remove old n*
         [g*** (remove-node g** n*)])

    (gen-directed-graph
     g***
     (list (cons f-node o-node)
           (cons f-node alt-node)
           (cons alt-node s-node)
           (cons o-node s-node)))))

(define (insert-elsewhere g v)
  ;; there could be multiple nodes at the end
  (let* ([x
          (last (sort (filter (lambda (n) (< (node-offset n) (get-offset v)))
                              (hash-values g))
                  <
                  #:key node-offset))]
        [l (get-node-ids-with-offset g (node-offset x))])

    (foldr (lambda (n accum) (split-single-head-node accum n v))
           g
           l)))


(define (insert-at-start? g offset)
  (let ([lowest-offset-node (first (sort-nodes-by-offset g))])

    ;; check whether any next node has a greater offset than offset
    (foldr
     (lambda (n-id accum)
       (< (node-offset (get-node g #:id n-id))) offset
       )
     #f
     (set->list (node-edges lowest-offset-node)))))

(define (insert-variation g v)
  (let ([offset (get-offset v)])
    (cond
      ;; add an extra alt where it exists
      [(exists-node-with-offset? g offset) (update-previous g v)]

      ;; insert a node at the end
      [(not (exists-node-with-greater-offset? g offset))
       (insert-at-the-end g v)
       ]

      ;; insert a node at the beginning or
      ;; insert in a node that has next and previous
      [else (insert-elsewhere g v)]

      
      ;[else (split-and-insert g variation 2)]
      )))

(define (update-vg g variations)
  (foldr
   (lambda (v accum) (insert-variation accum v))
   g
   variations))
