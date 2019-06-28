#lang racket

(require "./utils.rkt")

(provide sorted-bwm
         bwt)


(define (sorted-bwm s)
  (sort-bwm (gen-bwm s)))


(define (bwm-string l)
  (list->string
   (map (lambda (s*) (string-ref s* (- (string-length s*) 1))) l)))

;; ignore spaces for now

;; BWT brings like characters together in runs because it sorts by right context

(define/contract (bwt s)
  (string? . -> . string?)
  (bwm-string (sorted-bwm s)))
