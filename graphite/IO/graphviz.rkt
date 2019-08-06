#lang racket

(provide write-graphviz)


(define (write-graphviz g output-dir filename)
  (let* ([output-path (string-append output-dir filename)]
         [port (open-output-file output-path #:exists 'replace)]
         [mkdir-and-file (lambda ()
                           (make-directory output-dir)
                           port)]
         [y (if (directory-exists? output-dir)
                port
                (mkdir-and-file))])
    (graphviz g #:output y)
    (close-output-port y)))
