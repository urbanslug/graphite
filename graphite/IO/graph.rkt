#lang racket

(require racket/serialize)

(provide vg->gra
         gra->vg)



(define (vg->gra g fp)
  (let* ([output-path fp]
         [port (open-output-file output-path #:exists 'replace)])
    (write (serialize g) port)
    (close-output-port port)))

(define (gra->vg fp)
  (let* ([port       (open-input-file fp #:mode 'binary)]
         [g          (deserialize (read port))])
    (close-input-port port)
    g))
