#lang info
(define collection "graphite")
(define deps '(("racket" "7.3")))
(define version "0.0.0-alpha.1")

(define scribblings '(("graphite/graphite.scrbl" (multi-page toc))))

;; ignore whatever is in graphite and this file as well
(define test-omit-paths '("graphite" "info.rkt"))
