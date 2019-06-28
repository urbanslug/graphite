#lang racket

(require "./utils.rkt")

(provide sorted-bwm
         extract-bwt
         bwt)


(define (sorted-bwm s)
  (sort-bwm (gen-bwm s)))


(define (extract-bwt sorted-bwm)
  (list->string
   (map (lambda (s*) (string-ref s* (- (string-length s*) 1))) sorted-bwm)))

;; ignore spaces for now

;; BWT brings like characters together in runs because it sorts by right context

(define/contract (bwt s)
  (string? . -> . string?)
  (extract-bwt (sorted-bwm s)))
