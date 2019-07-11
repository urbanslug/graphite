#lang racket

(require racket/serialize)
(require file/sha1)
(require "../algorithms/utils.rkt")


(provide empty-graph

         node
         node-offset
         node-segment
         node-edges
         node-id)

(define (shorten-segment s)
  (if (< (string-length s) 20)
      s
      (substring s 0 20)))

;; TODO: add info on stable seq
(serializable-struct
 node (segment id offset edges)
 #:methods gen:custom-write
 [(define (write-proc node port mode)
    (let* ([seg       (shorten-segment (node-segment node))]
           [short-id  (shorten-hash (node-id node))]
           [offset*   (number->string (node-offset node))]
           [edge-set  (node-edges node)]
           [map-edges (lambda (edge-id) (shorten-hash edge-id))]
           [edges*    (if (set-empty? edge-set)
                          empty
                          (set-map edge-set map-edges))])
      (fprintf port "~a ~a ~a ~a" short-id seg offset* edges*)))])


(define empty-graph (make-immutable-hash))
