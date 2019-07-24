#lang racket


(provide variation
         variation-original
         variation-position
         variation-kmer)


;; TODO: kmer shouldn't be one char/base but a list of all possibilities there
(struct variation (position original kmer)
  #:methods gen:custom-write
  [(define (write-proc variation port mode)
     (let* ([f         (number->string (variation-position variation))]
            [original* (variation-original variation)]
            [alt       (variation-kmer variation)])
       (fprintf port "~a ~a ~a" f original* alt)))])
