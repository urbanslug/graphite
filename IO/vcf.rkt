#lang racket

(provide read-vcf save-line)

;; store reference file section
;; Get spec from https://samtools.github.io/hts-specs/VCFv4.2.pdf

(define fasta-hash (make-hash))

(define (save-line line)
  ;; Make a hash of pos and base
  (let* ([position-pair (regexp-match-positions #px"[ATCG]\\s" line)]
         [base-position  (cdr (car position-pair))] ;; sometimes this isn't a position pair but #f
         [base (string-ref line base-position)]
         [p (regexp-match-positions #px"\\s\\d*\\s" line)]
         [f (+ (car (car p)) 1)]
         [s (- (cdr (car p)) 1)]
         [position  (string->number (substring line f s))])
    (hash-set! fasta-hash position base)))

(define (parse-vcf line)
  (when (regexp-match-positions #rx"AF=." line)
      (let* ([position-pair (regexp-match-positions #rx"AF=." line)]
             [position  (- (cdr (car position-pair)) 1)]
             [allele-frequency-char (string-ref line position)]
             [allele-frequency-num (string->number (make-string 1 allele-frequency-char))]
             )
        (when (> allele-frequency-num 0)
            (save-line line)
            ))))

;; Read files and display them to the user
(define (next-line-it file)
  (let ([line (read-line file 'any)])
    (unless (eof-object? line)
      (parse-vcf line)
      (next-line-it file))))

(define (read-vcf filepath)
  (let* ([port (open-input-file "../data/RSV/refererence_and_vcf_file/H_3801_22_04.freebayes.vcf")])
    (next-line-it port)
    fasta-hash))

