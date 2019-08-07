#lang racket


(require math/matrix
         math/array)

;; i = x
;; j = y



(define (my-string-ref s pos)
  
  (string-ref s pos))
;; Make the first string the x axis/columns and
;; the second string the y axis/rows

(define (insertion i j distance-matrix)
  (let ([j* (if (> j 0) (- j 1) j)])
    (+ 1 (matrix-ref distance-matrix i j*))
    ))

(define (deletion i j distance-matrix)
  (let ([i* (if (> i 0) (- i 1) i)])
    (+ 1 (matrix-ref distance-matrix i* j))
    ))

(define (mismatch i j distance-matrix)
  (let ([i* (if (> i 0) (- i 1) i)]
        [j* (if (> j 0) (- j 1) j)])
    (+ 1 (matrix-ref distance-matrix i* j*))
    ))

(define (my-match i j distance-matrix)
  (let ([i* (- i 1)]
        [j* (- j 1)])
    (matrix-ref distance-matrix i* j*)
    ))

(define (indel-match i j first-string second-string distance-matrix)
  (list
   (my-match i j distance-matrix)
   (deletion i j distance-matrix)
   (insertion i j distance-matrix)
   ))

(define (indel-mismatch i j first-string second-string distance-matrix)
  (list
   (mismatch i j distance-matrix)
   (deletion i j distance-matrix)
   (insertion i j distance-matrix)))

(define (cost i j first-string second-string distance-matrix)
  (let ([i-char  (my-string-ref first-string i)]
        [j-char  (my-string-ref second-string j)])
    (cond
      [(and (= 0 i) (= 0 j))
       0]
      [(= 0 i)
       (add1 (matrix-ref distance-matrix i (- j 1)))]
      [(= 0 j)
       (add1 (matrix-ref distance-matrix  (- i 1) j))]
      [(char=? (my-string-ref first-string i) (my-string-ref second-string j))
       (begin 
              (apply min (indel-match i j first-string second-string distance-matrix))
              )
       
       ]
      [else
       (begin 
              (apply min (indel-mismatch i j first-string second-string distance-matrix))
              )
       ])))

#|
|#
;; We want to calculate the length of the strings once because
;; it is expensive. Approximately  O(n) time
;; First string makes i rows and second string makes j or columns
(define (calculate-edit-distance first-string second-string)
  (let* ([rows  (add1 (string-length first-string))]
         [cols (add1 (string-length second-string))]

         [distance-matrix (array->mutable-array (make-matrix rows cols 0))]

         [first-string* (string-append "^" first-string)]
         [second-string* (string-append "^" second-string)]

         )

    ;; is there a way to use for-matrix?
    (for ([i (build-list rows values)])
      (for ([j (build-list cols values)])
        (array-set! distance-matrix
                    (vector i j)
                    (cost i j first-string* second-string* distance-matrix))
        )
      )
    distance-matrix
    ))

(define (display-matrix m)
  (let-values ([(rows cols) (matrix-shape m)])
    (for ([col (build-list cols values)])
      (for ([row (build-list rows values)])
        (printf "~a " (matrix-ref m row col)))
      (displayln ""))))

(display-matrix (calculate-edit-distance "dist" "edi"))


#|
(build-matrix
 5
 5
 (lambda (i j) 0)
 )
(build-matrix 3 3 (lambda (i j) 0))
|#
