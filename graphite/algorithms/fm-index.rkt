#lang racket

(require "./utils.rkt")
(require "./suffix-array.rkt")
(require "./bwt.rkt")
(require math/array math/matrix)

(provide fl-map gen-tally-matrix alphabet->matrix-pos-map)


(define (fl-map s)
  (let* ([bwm (gen-bwm s)]
         [sorted-bwm (sort-bwm bwm)]
         [sa (extract-sa (sort-ranked-bwm (gen-ranks bwm)))]
         [f (sa-string sorted-bwm)]
         [l (extract-bwt sorted-bwm)])
    (list sa f l)))


;; TODO: similar to gen-ranks generalize it
(define (gen-pos-map sorted-alphabet
                            [ht (make-immutable-hash)]
                            [counter 0])
  (if (empty? sorted-alphabet)
      ht
      (gen-pos-map
       (rest sorted-alphabet)
       (hash-set ht (first sorted-alphabet) counter)
       (+ 1 counter))))

(define (gen-char-pos l
                     [ranks empty]
                     [counter 0])
  (if (empty? l)
      ranks
      (gen-char-pos
       (rest l)
       (append ranks (list (cons (first l) counter)))
       (+ 1 counter))))


;; We can avoid this hash map if we know the alphabet beforehand
;; A hash map of alphabet to it's position in an list where they're lex ordered
;; (-> list? hash-map?)
(define (alphabet->matrix-pos-map alphabet)
  (let ([sorted-alphabet (char-sort-ascending alphabet)])
    (gen-pos-map sorted-alphabet)))

(define (populate-row row arr alphabet-pos ht-count)
  (let ([l-count  (hash->list ht-count)])
    (map
     (lambda (char-pair)
       (let* ([char (car char-pair)]
             [value (cdr char-pair)]
             [col (hash-ref alphabet-pos char)]
             [idx (vector row col)])
         (array-set! arr idx value)))
     l-count)))

(provide get-col)
;; Calculate the column of the character in the tally matrix
(define (get-col c alphabet)
  (hash-ref (alphabet->matrix-pos-map alphabet) c))

;; BAD ALGO: quadratic time and memory
;; now I see why index generation needed so much mem
;; TODO:
;; sort alphabet
;; count in l and update count
;; use length of bwm
;; Lookup is O(1) but size is m x |sigma|
(define (gen-tally-matrix s)
  (let* ([alphabet (extract-alphabet s)]
         [m (string-length s)] ;; rows are positions
         [n (length alphabet)] ;; cols are characters in the alphabet
         ;; initialize tally matrix with nulls
         ;; are nulls better for space?
         [tally-matrix (array->mutable-array (make-matrix m n 0))]
         ;; char column in the tally matrix
         [alphabet-pos (alphabet->matrix-pos-map alphabet)]
         [ht-count (make-hash (foldr
                               (lambda (x accum)
                                 (append accum (list (cons x 0))))
                               empty
                               alphabet))]
         ;; a list of pairs of char to pos
         [char-pos (gen-char-pos (string->list s))])
    (map
     (lambda (p)
       (let* (
              [row (cdr p)]
              [col  (hash-ref alphabet-pos (car p)) ]
              [idx  (vector row col)]
              [value (+ 1 (hash-ref ht-count (car p))) ])
         (populate-row row tally-matrix alphabet-pos ht-count)
         (array-set! tally-matrix idx value)
         (hash-set! ht-count (car p) value)))
     char-pos)
    tally-matrix))


