#lang racket/base

(require rackunit
         "ecc.rkt")

;;; equal

(define file-tests
  (test-suite
   "Tests for ecc.rkt"
 
   (test-case
    "Field element equality"
    (let ([a (make-field-element 2 31)]
          [b (make-field-element 2 31)]
          [c (make-field-element 15 31)])
      (check-equal? (equal a b) #t)
      (check-equal? (equal a c) #f))
    )))