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
      (check-equal? a b)
      (check-not-equal? a c))
    )

   (test-case
    "Field element addition"
    (let ([a (field-element 12 17)]
          [b (field-element 13 17)]
          [c (field-element 8 17)]
          [d (field-element 8 16)])
      (check-equal? (+ a b) c)
     (check-exn
      exn:fail:contract?
      (lambda () (+ a d)))
     )
    )

   (test-case
    "Field element subtraction"
      (check-equal? (- (field-element 29 31)
                       (field-element 4 31))
                    (field-element 25 31))
      (check-equal? (- (field-element 15 31)
                       (field-element 30 31))
                    (field-element 16 31))
      (check-exn
       exn:fail:contract?
       (lambda () (- (field-element 1 2) (field-element 1 3))))
    )

   (test-case
    "Field element multiplication"
      (check-equal? (* (field-element 24 31)
                       (field-element 19 31))
                    (field-element 22 31))
      (check-exn
       exn:fail:contract?
       (lambda () (* (field-element 1 2) (field-element 1 3))))
    )

   (test-case
    "Field element exponentiation"
      (check-equal? (expt (field-element 17 31) 3) (field-element 15 31))
      (check-equal? (expt (field-element 17 31) -3) (field-element 29 31))
    )

   (test-case
    "Field element division"
      (check-equal? (/ (field-element 3 31) (field-element 24 31))
                    (field-element 4 31))
      (check-exn
       exn:fail:contract?
       (lambda () (/ (field-element 1 2) (field-element 1 3))))
    )

   (test-case
    "Point guard"
    (check-exn
     exn:fail?  ; Is there a more precis exception I could use?
     (lambda () (point 2 2 0 0))))

   (test-case
    "Point addition with integers"
    (let ([point-at-infinity (point +inf.0 +inf.0 5 7)]
          [p1 (point 3 -7 5 7)]
          [p2 (point 3 7 5 7)]
          [p3 (point -1 -1 5 7)]
          [p4 (point 2 -5 5 7)]
          [p5 (point -1 1 5 7)]
          [p6 (point 18 -77 5 7)])

      ; first argument is point-at-infinity
      (check-equal? (+ p1 point-at-infinity) p1)

      ; first argument is point-at-infinity
      (check-equal? (+ point-at-infinity p1) p1)

      ; first argument is second argument reflected across x-axis
      (check-equal? (+ p1 p2) point-at-infinity)

      ; different x-coordinates
      (check-equal? (+ p2 p3) p4)

      ; arguments equal
      (check-equal? (+ p5 p5) p6)
    ))

   (test-case
    "Point addition with field elements"
      (define prime 223)
      (define a (field-element 0 prime))
      (define b (field-element 7 prime))
      (define p1 (point (field-element 192 prime) (field-element 105 prime) a b))
      (define p2 (point (field-element 17 prime) (field-element 56 prime) a b))
      (define p3 (point (field-element 170 prime) (field-element 142 prime) a b))

      (check-equal? (+ p1 p2) p3)
    )
   
   ))

(run-tests file-tests)

