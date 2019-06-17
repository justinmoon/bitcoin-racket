#lang racket
(require rackunit "utils.rkt")
(require rackunit/text-ui)

(define file-tests
  (test-suite
   "Tests for utils.rkt"

   (test-case
    "little->int"
    (define b (bytes 1 2 3))
    (check-equal? 197121 (little->int b)))

   (test-case
    "big->int"
    (define b (bytes 1 2 3))
    (check-equal? 66051 (big->int b)))

   (test-case
    "int->big"
    (check-equal? (int->big 66051 4) (bytes 0 1 2 3) ))

   (test-case
    "int->little"
    (check-equal? (int->little 197121 4) (bytes 1 2 3 0)))

  )
)

(run-tests file-tests)
