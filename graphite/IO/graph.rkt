#lang racket

(require racket/serialize)

;; TODO: move to IO
;; serialization
(define (write-graph g)
  (let* ([output-path "/Users/urbanslug/src/racket/graphite/data/output/graphiter"]
         [port (open-output-file output-path #:exists 'replace)])
    (write (serialize g) port)
    (close-output-port port)))

(define (read-graph)
  (let* ([input-path "/Users/urbanslug/src/racket/graphite/data/output/graphiter"]
         [port (open-input-file input-path)]
         [g (deserialize (read port))])
    (close-input-port port)
    g))
