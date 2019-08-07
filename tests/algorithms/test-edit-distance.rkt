#lang racket

(require rackunit
         "../../graphite/algorithms/edit-distance.rkt")


(define A )
(define B)

(check-equal? (align "distance"  "editing")
              '((#\- . #\e)
                (#\d . #\d)
                (#\i . #\i)
                (#\s . #\-)
                (#\t . #\t)
                (#\a . #\i)
                (#\n . #\n)
                (#\c . #\g)
                (#\e . #\-)))
