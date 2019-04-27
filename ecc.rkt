#lang racket/base


;;; Field Elements
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

;;; Points
;;; FIXME: how to enforce values to `point`? A contract?
(struct point (x y a b) #:transparent)

(define (point-add p1 p2)
  (cond
    ; p1 is point-at-infinity
    [(= (point-x p1) +inf.0) p2]

    ; p1 is point-at-infinity
    [(= (point-x p2) +inf.0) p1]

    ; p1 is p2 reflected across x-axis
    [(and (= (point-x p1) (point-x p2))
          (not (= (point-y p1) (point-y p2))))
     (point +inf.0 +inf.0 (point-a p1) (point-b p1))]

    ; p1 and p2 have differnt x values
    [(not (= (point-x p1) (point-x p2)))
     (let ()
       (define s (/ (- (point-y p2) (point-y p1))
                    (- (point-x p2) (point-x p1))))
       (define x (- (expt s 2) (point-x p1) (point-x p2)))
       (define y (- (* s (- (point-x p1) x)) (point-y p1)))
      (point x y (point-a p1) (point-b p2)))]

    ; tangent on x-axis
    ;[(and (= p1 p2)
    ;      (= (point-y p1) (* 0 (point-x p1))))
    ; (point +inf.0 +inf.0 (point-a p1) (point-b p1))]

    ; p1 = p2
    [(equal? p1 p2)
     (let ()
       (define s (/ (+ (* 3 (expt (point-x p1) 2))
                     (point-a p1))
                  (* 2 (point-y p1))))
       (define x (- (expt s 2) (* 2 (point-x p1))))
       (define y (- (* s (- (point-x p1) x))
                    (point-y p1)))
       (point x y (point-a p1) (point-b p1)))]
    ))

;;; Exports
(provide field-element field-element-add field-element-subtract field-element-multiply
         field-element-expt field-element-divide

         point point-add
         )
