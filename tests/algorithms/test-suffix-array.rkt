#lang racket

(require rackunit)
(require "../../graphite/algorithms/suffix-array.rkt")
(require "../../graphite/algorithms/utils.rkt")

(define s "abaaba")

(check-equal? (sa s)
              '(6 5 2 3 0 4 1))


(define bwm (gen-bwm s))

(check-equal? (sa-string (gen-sorted-bwm s))
              "$aaaabb"
              "sa string failure")


(check-equal? (gen-ranks bwm)
              '((0 . "abaaba$")
                (1 . "baaba$a")
                (2 . "aaba$ab")
                (3 . "aba$aba")
                (4 . "ba$abaa")
                (5 . "a$abaab")
                (6 . "$abaaba"))
              "gen-ranks fail")

(check-equal? (sort-ranked-bwm (gen-ranks bwm))
              '((6 . "$abaaba")
                (5 . "a$abaab")
                (2 . "aaba$ab")
                (3 . "aba$aba")
                (0 . "abaaba$")
                (4 . "ba$abaa")
                (1 . "baaba$a"))
              "sort ranked bwm")
