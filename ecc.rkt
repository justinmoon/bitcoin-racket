#lang racket/base


;;; Field Elements
;;; (Should I use structs instead? https://docs.racket-lang.org/guide/define-struct.html)
(define (field-element number prime) (cons number prime))

(define (fe-number fe) (car fe))

(define (fe-prime fe) (cdr fe))

(define (fe-equal fe-one fe-two)
  (and (= (fe-number fe-one) (fe-number fe-two))
       (= (fe-prime fe-one) (fe-prime fe-two))))

(define (fe-plus fe-one fe-two)
  (field-element
                 (modulo (+ (fe-number fe-one) (fe-number fe-two))
                         (fe-prime fe-one))
                 (fe-prime fe-one))
  )


;;; Exports
(provide field-element fe-equal fe-number fe-prime fe-plus)
