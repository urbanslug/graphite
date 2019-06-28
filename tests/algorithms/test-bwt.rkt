#lang racket

(require rackunit)
(require "../../graphite/algorithms/bwt.rkt")
(require "../../graphite/algorithms/utils.rkt")

(define s "abaaba")

(check-equal? (sorted-bwm s)
              (sort (gen-bwm s) string<=?))

(check-equal? (bwt s)
              "abba$aa")
