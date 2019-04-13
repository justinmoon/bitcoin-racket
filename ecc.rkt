#lang racket/base


;;; Field Elements

(define (make-field-element number prime) (cons number prime))
(define (number field-element) (car field-element))
(define (prime field-element) (cdr field-element))
(define (equal field-element-one field-element-two)
  (and (= (number field-element-one) (number field-element-two))
       (= (prime field-element-one) (prime field-element-two))))


;;; Exports
(provide make-field-element equal number prime)
