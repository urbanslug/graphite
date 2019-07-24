#lang racket

(require "../algorithms/graph.rkt"
         "../structures/graph.rkt")

(provide vg->dot)


(define (gen-labels id n)
  (let* ([offset (node-offset n)]
         [segment* (string->list (node-segment n))]
        ;; to make things actually legible in graphviz
        [segment (if (> (length segment*) 100)
                     (format "~a ..." (list->string (take segment* 100)))
                     (list->string segment*))]
        [attributes (format " [label=\"~a: ~a\"];\n" offset segment)])
    (string-append "\n\t\"" id "\"" attributes)))


(define (gen-links id n)
  (let* ([edges       (node-edges n)]
         [format-edge (lambda (l) (format "\n\t\"~a\" -> \"~a\";\n" id l ))]
         [edges*      (string-join (set-map edges format-edge))])
    edges*))

(define (vg->dot-string g)
  (let ([gfa-header "digraph {\n"]
        [horizontal "\trankdir=\"LR\";\n"]
        [edge-style "\tedge [arrowhead=normal,tailport=e,headport=w,arrowtail=dot];\n"]
        [node-style "\tnode [shape=box];\n"]
        [closer "}"])
    (string-append
     gfa-header
     horizontal
     edge-style
     node-style
     (string-join (hash-map g gen-labels))
     (string-join (hash-map g gen-links))
     closer)))

(define (write-dot s output-fp)
  (let* ([output-path output-fp]
         [port (open-output-file output-path #:exists 'replace)])
    (printf "Generating DOT file at output ~a\n" output-fp )
    (fprintf port s)
    (close-output-port port)
    (printf "DOT output file generated at ~a\n" output-fp )))

(define (vg->dot g fp)
  (write-dot (vg->dot-string g) fp))
