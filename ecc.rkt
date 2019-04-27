#lang racket/base


;;; Field Elements
;;; (Should I use structs instead? https://docs.racket-lang.org/guide/define-struct.html)
(struct field-element (number prime) #:transparent)

(define (field-element-check-prime fe-one fe-two caller)
  ; the "caller" variable is a hack ...
  (if (not (= (field-element-prime fe-one) (field-element-prime fe-two)))
      (raise-argument-error caller "field elements with matching primes"
                            (cons (field-element-prime fe-one) (field-element-prime fe-two)))
      "field elements have matching primes"))

(define (field-element-add fe-one fe-two)
  (field-element-check-prime fe-one fe-two 'field-element-add)
  (field-element
                 (modulo (+ (field-element-number fe-one) (field-element-number fe-two))
                         (field-element-prime fe-one))
                 (field-element-prime fe-one))
  )

(define (field-element-subtract fe-one fe-two)
  (field-element-check-prime fe-one fe-two 'field-element-subtract)
  (field-element
                 (modulo (- (field-element-number fe-one) (field-element-number fe-two))
                         (field-element-prime fe-one))
                 (field-element-prime fe-one))
  )

(define (field-element-multiply fe-one fe-two)
  (field-element-check-prime fe-one fe-two 'field-element-multiply)
  (field-element
                 (modulo (* (field-element-number fe-one) (field-element-number fe-two))
                         (field-element-prime fe-one))
                 (field-element-prime fe-one))
  )

(define (field-element-expt fe exponent)
  (define prime (field-element-prime fe))

  ; Some hacks to ensure exponent is positive
  (define (get-positive-exponent n)
    (if (>= n 0)
        n
        (get-positive-exponent (+ n (- prime 1)))))
  (define positive-exponent (get-positive-exponent exponent))

  (field-element
                 (modulo (expt (field-element-number fe) positive-exponent) prime)
                 prime)
  )

(define (field-element-divide fe-one fe-two)
  (field-element-check-prime fe-one fe-two 'field-element-divide)
  (define fe-two-inv (expt (field-element-number fe-two)
                           (- (field-element-prime fe-two) 2)))
  (field-element
                 (modulo (* (field-element-number fe-one) fe-two-inv)
                         (field-element-prime fe-one))
                 (field-element-prime fe-one))
  )

;;; Exports
(provide field-element field-element-add field-element-subtract field-element-multiply
         field-element-expt field-element-divide)
