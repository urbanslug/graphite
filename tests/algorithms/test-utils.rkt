#lang racket

(require rackunit)

(require "../../graphite/algorithms/utils.rkt")

(define s "abaaba")


(check-equal? (rotate-string s) "baabaa")

(check-equal? (gen-bwm s)
              '("abaaba$"
                "baaba$a"
                "aaba$ab"
                "aba$aba"
                "ba$abaa"
                "a$abaab"
                "$abaaba"))


(check-equal? (gen-sorted-bwm s)
              '("$abaaba"
                "a$abaab"
                "aaba$ab"
                "aba$aba"
                "abaaba$"
                "ba$abaa"
                "baaba$a"))
(define s2 "aaasddsdsdqwqsdsderwfw")

(check-equal? (#\z . in? . (string->list s2)) #f)

(check-equal? (#\w . in? . (string->list s2)) #t)


(check-equal? (extract-alphabet "aaasddsdsdqwqsdsderwfwr$")
              '(#\$ #\r #\w #\f #\e #\d #\s #\q #\a))
