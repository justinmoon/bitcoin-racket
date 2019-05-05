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

            
      (define p4 (point (field-element 47 prime) (field-element 71 prime) a b))
      (define p5 (point (field-element 117 prime) (field-element 141 prime) a b))
      (define p6 (point (field-element 60 prime) (field-element 139 prime) a b))
      (check-equal? (+ p4 p5) p6)

      (define p7 (point (field-element 143 prime) (field-element 98 prime) a b))
      (define p8 (point (field-element 76 prime) (field-element 66 prime) a b))
      (define p9 (point (field-element 47 prime) (field-element 71 prime) a b))
      (check-equal? (+ p7 p8) p9)
    )

   (test-case
    "scalar*point"


      (define prime 223)
      (define a (field-element 0 prime))
      (define b (field-element 7 prime))
      
      (check-equal? (* 2 (point (field-element 192 prime) (field-element 105 prime) a b))
                    (point (field-element 49 prime) (field-element 71 prime) a b))

      (check-equal? (* 21 (point (field-element 47 prime)(field-element 71 prime) a b))
                    (point +inf.0 +inf.0 a b))
      )
   
   (test-case
    "s256 order"
    (check-equal? (point-x (* G N)) +inf.0))
   
   (test-case
     "signatures"
        (define x (s256-field-element #x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c))
        (define y (s256-field-element #x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34))
        (define pt (s256-point x y))
        (define z #xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60)
        (define r #xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395)
        (define s #x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4)
        (define sig (signature r s))
        #t
        (check-true (signature-valid? pt z sig))
        )
   ))

(run-tests file-tests)

