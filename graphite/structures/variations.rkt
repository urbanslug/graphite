#lang racket


(provide variation
         variation-position
         variation-kmer)


;; TODO: kmer shouldn't be one char/base but a list of all possibilities there
(struct variation (position kmer)
  #:methods gen:custom-write
  [(define (write-proc variation port mode)
     (let* ([f (number->string (variation-position variation))]
            [s (variation-kmer variation)]
            [p (if (char? s) (string s) s)])
       (fprintf port "~a: ~a" f p)))])
