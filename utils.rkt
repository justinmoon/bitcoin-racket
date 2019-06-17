#lang racket

;;;
;;; bytes <-> int conversions
;;;

(define (little->int b)
  (define result 0)
  (define index 0)
  (for ([place b])
    (set! result (+ result (* place (expt 256 index))))
    (set! index (+ index 1)))
  result)

(define (big->int b)
  (define reversed (list->bytes (reverse (bytes->list b))))
  (little->int reversed))

(define (int->little int nbytes)
  (define result #"")
  (for ([i nbytes])
         (displayln int)
         (displayln result)
         (set! result (bytes-append result (bytes (modulo int 256))))
         (set! int (quotient int 256)))
  result)

(define (int->big int nbytes)
  (list->bytes (reverse (bytes->list (int->little int nbytes)))))

(provide little->int big->int int->little int->big)
