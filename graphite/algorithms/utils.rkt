#lang racket

(provide .>
         rotate-string
         gen-sorted-bwm
         sort-bwm
         extract-alphabet
         gen-bwm)


;; Burrows Wheeler Matrix (BWM)
;;;; Burrows Wheeler Transform (BWT)


;; function composition
(define (.> f g)
  (lambda (x)
    (f (g x))))

;; move the first character to the end of the string
(define/contract (rotate-string str)
  (string? . -> . string?)
  (let ([head (string-ref str 0)]
        [tail (substring str 1)])
    (~a tail head)))

;; Generate an unsorted Burrows-Wheeler Matrix
(define/contract (gen-bwm s [bwt-list empty])
  (-> string? list?)
  (if (char=? (string-ref s 0) #\$)
      bwt-list
      (if (empty? bwt-list)
          (let ([dollar-s (~a s "$")])
            (gen-bwm dollar-s (list dollar-s)))
          (let* ([s* (rotate-string s)]
                 [bwt-list* (append bwt-list ( list s*))])
            (gen-bwm s* bwt-list*)))))

(define/contract (sort-bwm bwt-list)
  (list? . -> . list?)
  (sort bwt-list string<=?))

(define (gen-sorted-bwm s)
  (sort-bwm (gen-bwm s)))


;; Test membership
(define (in? element l)
  (if (member element l) #t #f))

(define (id)
  (lambda (x) x))


;; get all the unique characters in a string
(define (extract-alphabet s)
  (let* ([l* (string->list s) ])
    (foldr (lambda (char accum) (if (in? char accum)
                                    accum
                                    (append accum (list char))))
           empty
           l*)))

;; list -> list
(provide char-sort-ascending)
(define (char-sort-ascending l)
  (sort l char<=?))
