#lang info
(define collection "graphite")
(define deps '(("racket" "7.2") "graph"))
(define version "0.0.0-alpha.1")

;; test only that which is in tests
(define test-include-paths '("tests"))

;; ignore whatever is in graphite and this file as well
(define test-omit-paths '("graphite" "info.rkt"))
