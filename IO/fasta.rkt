#lang racket

(require racket/cmdline)

;; Read files and display them to the user
(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (displayln line)
      (next-line-it file))))


;; TODO: Use racket cmdline
(define (main)
  (let* ([f (first (vector->list (current-command-line-arguments)))]
        [f* (open-input-file f)])
    (next-line-it f*)))

(main)
