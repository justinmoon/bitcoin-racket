#lang racket/base

(require rackunit "ecc.rkt")
(require rackunit/text-ui)

;;; equal

(define file-tests
  (test-suite
   "Tests for ecc.rkt"
 
   (test-case
    "Field element equality"
    (let ([a (field-element 2 31)]
          [b (field-element 2 31)]
          [c (field-element 15 31)])
      (check-equal? (fe-equal a b) #t)
      (check-equal? (fe-equal a c) #f))
    )

   (test-case
    "Field element addition"
    (let ([a (field-element 12 17)]
          [b (field-element 13 17)]
          [c (field-element 8 17)])
      (check-equal? (fe-plus a b) c))
   
    )

   ))

(run-tests file-tests)

