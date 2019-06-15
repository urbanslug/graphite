#lang racket

(provide read-fastas fasta-hash)

;; In this context sequence means a base or amino acid sequence
(define fasta-hash (make-hash))
(define current-sequence-identifier null)

(define (parse-fasta-file line)
  (if (eqv? (string-ref line 0) #\>) ;; if the line starts with a greater than sign
      (let ([sequence-identifier (substring line 1)])
        ;; use mutable state to keep track of current sequence
        (set! current-sequence-identifier sequence-identifier)
        (hash-set! fasta-hash sequence-identifier ""))
      ;; append to current seq
      (hash-set! fasta-hash
                 current-sequence-identifier
                 (string-append (hash-ref fasta-hash current-sequence-identifier) line))))

;; Read files and display them to the user
(define (next-line-it file)
  (let ([line (read-line file 'any)])
    (unless (eof-object? line)
      (parse-fasta-file line)
      (next-line-it file))))

(define (read-fastas filepath)
  (let* ([port (open-input-file "../data/RSV/refererence_and_vcf_file/9465113.fa")])
    (next-line-it port)
    fasta-hash))
