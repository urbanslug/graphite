#lang racket

(require "./utils.rkt")


(provide rotate-string
         unsorted-bwm
         sort-bwm
         burrows-wheeler-matrix
         bwt)

;; Burrows Wheeler Matrix (BWM)
;;;; Burrows Wheeler Transform (BWT)

;; move the first character to the end of the string
(define/contract (rotate-string str)
  (string? . -> . string?)
  (let ([head (string-ref str 0)]
        [tail (substring str 1)])
    (~a tail head)))

(define/contract (unsorted-bwm s [bwt-list empty])
  (-> string? list?)
  (if (char=? (string-ref s 0) #\$)
      bwt-list
      (if (empty? bwt-list)
          (let ([dollar-s (~a s "$")])
            (unsorted-bwm dollar-s (list dollar-s)))
          (let* ([s* (rotate-string s)]
                 [bwt-list* (append bwt-list ( list s*))])
            (unsorted-bwm s* bwt-list*)))))

(define/contract (sort-bwm bwt-list)
  (list? . -> . list?)
  (sort bwt-list string<=?))

(define (burrows-wheeler-matrix s)
  (sort-bwm (unsorted-bwm s)))

(define (sa-string l)
  (list->string (map (lambda (s*) (string-ref s* 0) ) l)))

(define (bwm-string l)
  (list->string
   (map (lambda (s*) (string-ref s* (- (string-length s*) 1))) l)))

(define/contract (bwt s)
  (string? . -> . string?)
  (bwm-string (sort-bwm (unsorted-bwm s))))
