#lang racket

(provide read-vcf save-line variation-hash)

;; store reference file section
;; Get spec from https://samtools.github.io/hts-specs/VCFv4.2.pdf

(define variation-hash (make-hash))

(define (save-line line)
  ;; Make a hash of pos and base
  (let* ([position-pair (regexp-match-positions #px"[ATCG]\\s" line)]
         [base-position  (cdr (car position-pair))] ;; sometimes this isn't a position pair but #f
         [base (string-ref line base-position)]
         [p (regexp-match-positions #px"\\s\\d*\\s" line)]
         [f (+ (car (car p)) 1)]
         [s (- (cdr (car p)) 1)]
         [position  (string->number (substring line f s))])
    (hash-set! variation-hash position base)))

(define (parse-vcf line)
  (when (regexp-match-positions #rx"AF=." line)
      (let* ([position-pair (regexp-match-positions #rx"AF=." line)]
             [position  (- (cdr (car position-pair)) 1)]
             [allele-frequency-char (string-ref line position)]
             [allele-frequency-num (string->number (make-string 1 allele-frequency-char))]
             )
        (when (> allele-frequency-num 0)
            (save-line line)))))

;; Read files and display them to the user
(define (next-line-it file)
  (let ([line (read-line file 'any)])
    (unless (eof-object? line)
      (parse-vcf line)
      (next-line-it file))))

(define (read-vcf filepath)
  (let* ([port (open-input-file filepath)])
    (next-line-it port)
    variation-hash))


(define (gen-variation-list var-hash)
  (let ([hash-list (hash->list var-hash)])
    (sort hash-list (lambda (x y) (< (car x) (car y))))))
