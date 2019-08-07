#lang racket

(require math/matrix)

(provide display-matrix)

(define (display-matrix m)
  (let-values ([(rows cols) (matrix-shape m)])
    (for ([col (build-list cols values)])
      (for ([row (build-list rows values)])
        (printf "~a " (matrix-ref m row col)))
      (displayln ""))))
