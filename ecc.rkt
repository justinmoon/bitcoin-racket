#lang racket/base


;;; Field Elements
;;; (Should I use structs instead? https://docs.racket-lang.org/guide/define-struct.html)
(define (field-element number prime) (cons number prime))

(define (fe-number fe) (car fe))

(define (fe-prime fe) (cdr fe))

(define (fe-equal fe-one fe-two)
  (and (= (fe-number fe-one) (fe-number fe-two))
       (= (fe-prime fe-one) (fe-prime fe-two))))

(define (fe-check-prime fe-one fe-two caller)
  ; the "caller" variable is a hack ...
  (if (not (= (fe-prime fe-one) (fe-prime fe-two)))
      (raise-argument-error caller "field elements with matching primes"
                            (cons (fe-prime fe-one) (fe-prime fe-two)))
      "field elements have matching primes"))

(define (fe-plus fe-one fe-two)
  (fe-check-prime fe-one fe-two 'fe-plus)
  (field-element
                 (modulo (+ (fe-number fe-one) (fe-number fe-two))
                         (fe-prime fe-one))
                 (fe-prime fe-one))
  )

(define (fe-minus fe-one fe-two)
  (fe-check-prime fe-one fe-two 'fe-minus)
  (field-element
                 (modulo (- (fe-number fe-one) (fe-number fe-two))
                         (fe-prime fe-one))
                 (fe-prime fe-one))
  )

(define (fe-multiply fe-one fe-two)
  (fe-check-prime fe-one fe-two 'fe-multiply)
  (field-element
                 (modulo (* (fe-number fe-one) (fe-number fe-two))
                         (fe-prime fe-one))
                 (fe-prime fe-one))
  )

;;; Exports
(provide field-element fe-equal fe-number fe-prime fe-plus fe-minus fe-multiply)
