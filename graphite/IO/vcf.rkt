#lang racket

(provide read-vcf)

(require "../structures/variations.rkt")

;; store reference file section
;; Get spec from https://samtools.github.io/hts-specs/VCFv4.2.pdf

;; append to this global variations variable
;; a list
(define variations empty)

(define (save-line line)
  (let* ([position-pair (regexp-match-positions #px"[ATCG]\\s" line)]
         ;; sometimes this isn't a position pair but #f
         [base-position  (cdr (car position-pair))]
         [base (string-ref line base-position)]
         [p (regexp-match-positions #px"\\s\\d*\\s" line)]
         [f (+ (car (car p)) 1)]
         [s (- (cdr (car p)) 1)]
         [position  (string->number (substring line f s))])
    (set! variations
          (append variations (list (variation position base))))))

(define (parse-vcf line)
  (when (regexp-match-positions #rx"AF=." line)
    (let* ([position-pair (regexp-match-positions #rx"AF=." line)]
           [position  (- (cdr (car position-pair)) 1)]
           [allele-frequency-char (string-ref line position)]
           [allele-frequency-num (string->number
                                  (string allele-frequency-char))])
      (when (> allele-frequency-num 0)
        (save-line line)))))

(define (next-line-it file)
  (let ([line (read-line file 'any)])
    (unless (eof-object? line)
      (parse-vcf line)
      (next-line-it file))))

(define (read-vcf filepath)
  (let* ([port (open-input-file filepath)])
    (next-line-it port)
    variations))
