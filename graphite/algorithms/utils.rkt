#lang racket

(provide .>)

(define (.> f g)
  (lambda (x)
    (f (g x))))
