#lang racket

(require "./utils.rkt")

(provide sorted-bwm
         extract-bwt
         bwt)



;; string -> list of strings
(define sorted-bwm (compose sort-bwm gen-bwm))

;; list of strings -> string
(define (extract-bwt sorted-bwm*)
  ((compose list->string map)
   (lambda (s) (string-ref s (sub1 (string-length s))))
   sorted-bwm*))

;; ignore spaces for now

;; string -> string
;; BWT brings like characters together in runs because it sorts by
;; right context i.e. the text that comes right after the,
(define bwt (compose extract-bwt sorted-bwm))

