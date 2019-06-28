#lang racket

(require "./utils.rkt")

(provide sa-string
         sa
         gen-ranks
         extract-sa
         sort-ranked-bwm)

;; pass this the unsorted bwm
;; O(n)
(define (gen-ranks bwm* [pos 0] [sa empty])
  (if (empty? bwm*)
      sa
      (let* ([s* (first bwm*)]
             [new-sa (append sa (list (cons pos s*)))])
        (gen-ranks (rest bwm*) (+ 1 pos) new-sa))))

;; Sorts the ranked bwm based on their strings
(define (sort-ranked-bwm ranked-bwm)
  (sort ranked-bwm
        (lambda (f s) (string<=? (cdr f) (cdr s)))))

;; Extract all first chars from a sorted BWM and "compile" them into a string
(define (sa-string l)
  (list->string (map (lambda (s*) (string-ref s* 0) ) l)))

(define (extract-sa ranked-bwm)
  (map car ranked-bwm))

(define (sa s)
  (extract-sa (sort-ranked-bwm (gen-ranks (gen-bwm s)))))
