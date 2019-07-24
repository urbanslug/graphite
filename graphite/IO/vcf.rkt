#lang racket

(provide read-vcf)

(require "../structures/variations.rkt")

;; store reference file section
;; Get spec from https://samtools.github.io/hts-specs/VCFv4.2.pdf

;; append to this global variations variable
;; a list
(define variations empty)



(define (save-line line)
  (let* ([stripped (string-split line)]
         ;; split by every comma
         [alt (string-split (list-ref stripped 4) ",")]
         [original (list-ref stripped 3)]
         [position  (string->number (list-ref stripped 1 ))]

         [var-list (map (lambda (alt*) (variation position original alt*)) alt)])
    (set! variations (append variations var-list))))

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
    (displayln "Parsing VCF. Considering AF=1")
    (next-line-it port)
    (displayln "Successfully loaded variation data.")
    variations))
