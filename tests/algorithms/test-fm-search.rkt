#lang racket

(require rackunit)

(require math/array)

(require "../../graphite/algorithms/fm-search.rkt")
(require "../../graphite/algorithms/fm-index.rkt")
(require "../../graphite/algorithms/suffix-array.rkt")
(require "../../graphite/algorithms/bwt.rkt")

(define s "abaaba")

(define fl-list (fl-map s))

(check-equal? (run-length-encode-f (second fl-list))
              (array #[1 4 2]))

(check-equal? (run-length-encode (third fl-list))
              '((#\a . 1) (#\b . 2) (#\a . 1) (#\$ . 1) (#\a . 2)))
