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
          [c (field-element 8 17)]
          [d (field-element 8 16)])
      (check-equal? (fe-plus a b) c)
     (check-exn
      exn:fail:contract?
      (lambda () (fe-plus a d)))
     )
    )

   (test-case
    "Field element subtraction"
      (check-equal? (fe-minus (field-element 29 31)
                              (field-element 4 31))
                    (field-element 25 31))
      (check-equal? (fe-minus (field-element 15 31)
                              (field-element 30 31))
                    (field-element 16 31))
      (check-exn
       exn:fail:contract?
       (lambda () (fe-minus (field-element 1 2) (field-element 1 3))))
    )

   (test-case
    "Field element multiplication"
      (check-equal? (fe-multiply (field-element 24 31)
                                 (field-element 19 31))
                    (field-element 22 31))
      (check-exn
       exn:fail:contract?
       (lambda () (fe-multiply (field-element 1 2) (field-element 1 3))))
    )

   (test-case
    "Field element exponentiation"
      (check-equal? (fe-expt (field-element 17 31) 3)
                    (field-element 15 31))
      (check-equal? (fe-expt (field-element 17 31) -3)
                    (field-element 29 31))
    )
   
   ))

(run-tests file-tests)

