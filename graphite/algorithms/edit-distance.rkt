#lang racket



(require math/matrix
         math/array
         "../utils/matrix.rkt")

(define (insertion i j distance-matrix)
  (let ([j* (if (> j 0) (- j 1) j)])
    (+ 1 (matrix-ref distance-matrix i j*))))

(define (deletion i j distance-matrix)
  (let ([i* (if (> i 0) (- i 1) i)])
    (+ 1 (matrix-ref distance-matrix i* j))))

(define (mismatch i j distance-matrix)
  (let ([i* (if (> i 0) (- i 1) i)]
        [j* (if (> j 0) (- j 1) j)])
    (+ 1 (matrix-ref distance-matrix i* j*))))

(define (my-match i j distance-matrix)
  (let ([i* (- i 1)]
        [j* (- j 1)])
    (matrix-ref distance-matrix i* j*)))


(define (cost i j first-string second-string distance-matrix)
  (let ([i-char  (string-ref first-string i)]
        [j-char  (string-ref second-string j)])
    (cond
      [(and (= 0 i) (= 0 j))  0]
      [(= 0 i)                (add1 (matrix-ref distance-matrix i (- j 1)))]
      [(= 0 j)                (add1 (matrix-ref distance-matrix  (- i 1) j))]
      [(char=? i-char j-char) (min (my-match i j distance-matrix)
                                   (deletion i j distance-matrix)
                                   (insertion i j distance-matrix))]
      [else                   (min (mismatch i j distance-matrix)
                                   (deletion i j distance-matrix)
                                   (insertion i j distance-matrix))])))

#|
Calculate the length of a string once because it's expensive i.e. O(n)
First string makes i/rows
Second string makes j/columns
Spec: https://github.com/urbanslug/graphite/issues/1
|#
(define (distance-matrix first-string second-string)
  (let* ([rows  (add1 (string-length first-string))]
         [cols (add1 (string-length second-string))]
         [distance-matrix (array->mutable-array (make-matrix rows cols 0))]
         [first-string* (string-append "^" first-string)]
         [second-string* (string-append "^" second-string)])

    ;; is there a way to use for-matrix?
    (for ([i (build-list rows values)])
      (for ([j (build-list cols values)])
        (array-set! distance-matrix
                    (vector i j)
                    (cost i j first-string* second-string* distance-matrix))))
    distance-matrix))

(display-matrix (distance-matrix "dist" "edi"))

