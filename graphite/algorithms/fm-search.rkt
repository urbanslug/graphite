#lang racket

(require "./utils.rkt")
(require "./suffix-array.rkt")
(require "./bwt.rkt")
(require "./fm-index.rkt")
(require math/array math/matrix)


(provide run-length-encode run-length-encode-f search)

;; TODO: run length encode f. This would be one array.
;; an array of count which is indexed by the lex order of the bases
;; string -> array
(define (run-length-encode-f s)
  (let* ([compressed (run-length-encode s)]
        [size (vector  (length compressed))])
    (list->array
     (map (lambda (x) (cdr x)) compressed))))

;; compress a string with run length encoding
(define (run-length-encode s)
  (let* ([alphabet (extract-alphabet s)]
        [pos-map (alphabet->matrix-pos-map alphabet)]
        [s* (string->list s)])
    (foldl
     (lambda (c accum)
       (if (empty? accum)
           (list (cons c 1))
           (let* ([prev (last accum)]
                  [c* (car prev)]
                  [count  (cdr prev)])
             (if (char=? c c*)
                 (replace-last accum (cons c (+ count 1)))
                 (append accum (list (cons c 1)))))))
     empty
     s*)))

;; replace last element in list with value-blame
;; O(n)
(define (replace-last l v)
  (if (empty? l)
      l
      (list-set l (- (length l) 1) v)))

(define (calculate-tallies tally-matrix col start-row stop-row)
  (let* ([idxs (array  #[(vector start-row col) (vector stop-row col)])])
    (array-indexes-ref tally-matrix idxs)))

;; takes q char and an SA list and gives you the indices that it occurs
(define (find-index-range char str)
  (let ([x (indexes-of (string->list str) char)])
    (values (first x) last x)))

;; sum the values in an array
(define (sum-array arr)
  (foldr + 0 (array->list arr)))

;; where does a* occur in f and how many times
(define (search-rec kmer tally-matrix f l f-comp l-comp alphabet [a-ranks null])
   (if (<= (length kmer) 1)

        (let ([start (array-ref a-ranks (vector 0))]
              [stop (array-ref a-ranks (vector 1))])
          (list start stop))

        (let ([a* (first kmer)]
              [b* (second kmer)])

          (if (null? a-ranks)
          (let* ([a* (first kmer)]
                 [b* (second kmer)]
                 ;; where does a* occur
                 [a-start-in-f (hash-ref (alphabet->matrix-pos-map alphabet) a*)]
                 ;; how many of a*
                 [a-stop-in-f (array-ref f-comp (vector a-start-in-f))]
                 ;; get tally matrix col with b*
                 [col (get-col b* alphabet)]
                 [tallies (calculate-tallies tally-matrix col a-start-in-f a-stop-in-f)])
            (search-rec (rest kmer) tally-matrix f l f-comp l-comp alphabet tallies))

          (let* ([f (- (array-ref a-ranks (vector 0)) 1)]
                [s (- (array-ref a-ranks (vector 1)) 1)]
                ;; where does a* occur
                [a-in-f-comp (hash-ref (alphabet->matrix-pos-map alphabet) a*)]
                [a-start-in-f (sum-array
                               (array-slice-ref f-comp (list (:: 0 a-in-f-comp))))]
                ;; how many of a*
                [a-stop-in-f (+ a-start-in-f s)]
                ;; get tally matrix col with b*
                [col (get-col b* alphabet)]
                [tallies (calculate-tallies tally-matrix col a-start-in-f a-stop-in-f)])
            (search-rec (rest kmer) tally-matrix f l f-comp l-comp alphabet tallies))))))


(define (search  kmer ref)
  (match-let* ([(list sa sa-str bwt) (fl-map ref)]
               [tally-matrix (gen-tally-matrix bwt)]
               ;; use bwt because it contains $
               [alphabet (extract-alphabet bwt)]
               [fc (run-length-encode-f sa-str)]
               [lc (run-length-encode sa-str)]
               [(list start stop)
                (search-rec (string->list kmer) tally-matrix sa-str bwt fc lc alphabet)])
    (sublist-inc sa start stop)))

(time (search "aba" "abaaba"))
