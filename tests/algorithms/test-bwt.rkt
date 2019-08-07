#lang racket

(require rackunit
         "../../graphite/algorithms/bwt.rkt"
         "../../graphite/algorithms/utils.rkt")


(define s "abaaba")

(check-equal? (sorted-bwm s)
              (sort (gen-bwm s) string<=?))

(check-equal? (bwt s)
              "abba$aa")
