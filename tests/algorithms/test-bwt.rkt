#lang racket

(require rackunit)
(require "../../graphite/algorithms/bwt.rkt")
(require "../../graphite/algorithms/utils.rkt")

(define s "abaaba")

(check-equal? (rotate-string s) "baabaa")

(check-equal? (unsorted-bwm s)
              '("abaaba$"
                "baaba$a"
                "aaba$ab"
                "aba$aba"
                "ba$abaa"
                "a$abaab"
                "$abaaba"))

(check-equal? ((sort-bwm . .> . unsorted-bwm) s)
              '("$abaaba"
                "a$abaab"
                "aaba$ab"
                "aba$aba"
                "abaaba$"
                "ba$abaa"
                "baaba$a"))

(check-equal? (burrows-wheeler-matrix s)
              ((sort-bwm . .> . unsorted-bwm) s))


(check-equal? (bwt s)
              "abba$aa")
