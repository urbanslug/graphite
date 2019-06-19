#lang racket

(define example-string "abaaba")

;; naive BWT

;;
(define (rotate-string str)
  (let ([head (string-ref str 0)]
        [tail (substring str 1)])
    (~a tail head)))
