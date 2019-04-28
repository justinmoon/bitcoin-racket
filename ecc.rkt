#lang racket
(require racket/generic
         (only-in racket/base [+ racket:+])
         (only-in racket/base [- racket:-])
         (only-in racket/base [* racket:*])
         (only-in racket/base [/ racket:/])
         (only-in racket/base [expt racket:expt])
         )

(define-generics addable
  (add addable _))

(define-generics subtractable
  (subtract subtractable _))

(define-generics multiplicible
  (multiply multiplicible _))

(define-generics exponentiatable
  (exponentiate exponentiatable _))

(define-generics divisible
  (divide divisible _))

(define (enforce-prime-constraint fe1 fe2 caller)
  ; the "caller" variable is a hack ...
  (if (not (= (field-element-prime fe1) (field-element-prime fe2)))
      (raise-argument-error caller "field elements with matching primes"
                            (cons (field-element-prime fe1) (field-element-prime fe2)))
      "field elements have matching primes"))

; FIXME: these functions should all accept arbitrary many inputs
(define (+ x y)
  (cond
    [(and (number? x) (number? y)) (racket:+ x y)]    
    [(or
      (and (field-element? x) (field-element? y))
      (and (point? x) (point? y)))
     (add x y)]
    [else (error 'add "Can't add")]))      ; TODO: better error message

(define (- x y)
  (cond
    [(and (number? x) (number? y)) (racket:- x y)]
    [(or
      (and (field-element? x) (field-element? y))
      (and (point? x) (point? y)))
     (subtract x y)]
    [else (error 'add "Can't subtract")]))      ; TODO: better error message

(define (* x y)
  (cond
    [(and (number? x) (number? y)) (racket:* x y)]
    [(or
      (and (field-element? x) (field-element? y))
      (and (point? x) (number? y)))
     (multiply x y)]
    [else (error 'add "Can't multiply")]))      ; TODO: better error message

(define (expt x exponent)
  (cond
    [(and (number? x) (number? exponent)) (racket:expt x exponent)]
    [(and (field-element? x) (number? exponent))
     (exponentiate x exponent)]
    [else (error 'add "Can't exponentiate")]))      ; TODO: better error message

(define (/ x y)
  (cond
    [(and (number? x) (number? y)) (racket:/ x y)]
    [(and (field-element? x) (field-element? y))
     (divide x y)]
    [else (error 'add "Can't divide")]))      ; TODO: better error message

(struct field-element (number prime)
  #:transparent
  
  #:methods gen:addable
  [(define (add fe1 fe2)
     (enforce-prime-constraint fe1 fe2 "+")
     (field-element (modulo (+ (field-element-number fe1)
                               (field-element-number fe2))
                            (field-element-prime fe1))
                    (field-element-prime fe1)))]
  #:methods gen:subtractable
  [(define (subtract fe1 fe2)
     (enforce-prime-constraint fe1 fe2 "-")
     (field-element (modulo (- (field-element-number fe1)
                               (field-element-number fe2))
                            (field-element-prime fe1))
                    (field-element-prime fe1)))]
  #:methods gen:multiplicible
  [(define (multiply fe1 fe2)
     (enforce-prime-constraint fe1 fe2 "*")
     (field-element (modulo (* (field-element-number fe1)
                               (field-element-number fe2))
                            (field-element-prime fe1))
                    (field-element-prime fe1)))]
  #:methods gen:exponentiatable
  [(define (exponentiate fe exponent)
     (define prime (field-element-prime fe))

     ; Some hacks to ensure exponent is positive
     (define (get-positive-exponent n)
       (if (>= n 0)
           n
           (get-positive-exponent (+ n (- prime 1)))))
     (define positive-exponent (get-positive-exponent exponent))

     (field-element
      (modulo (expt (field-element-number fe) positive-exponent) prime)
      prime))]
  #:methods gen:divisible
  [(define (divide fe1 fe2)
     (enforce-prime-constraint fe1 fe2 "/")
     (define inv (expt (field-element-number fe2)
                              (- (field-element-prime fe2) 2)))
     (field-element
      (modulo (* (field-element-number fe1) inv)
              (field-element-prime fe1))
      (field-element-prime fe1)))]
  )

(struct point (x y a b)
  #:transparent

  #:guard (λ (x y a b name)
            (unless (equal? (* y y) (+ (+ (expt x 3) (* a x)) b))    ; hack: + only takes 2 args ...
              (error 'point "not a valid point"))
            (values x y a b))
  
  #:methods gen:addable
  [(define (add p1 p2)
(cond
    ; p1 is point-at-infinity
    [(equal? (point-x p1) +inf.0) p2]

    ; p1 is point-at-infinity
    [(equal? (point-x p2) +inf.0) p1]

    ; p1 is p2 reflected across x-axis
    [(and (equal? (point-x p1) (point-x p2))
          (not (= (point-y p1) (point-y p2))))
     (point +inf.0 +inf.0 (point-a p1) (point-b p1))]

    ; p1 and p2 have different x values
    [(not (equal? (point-x p1) (point-x p2)))
     (let ()
       (define s (/ (- (point-y p2) (point-y p1))
                    (- (point-x p2) (point-x p1))))
       (define x (- (- (expt s 2) (point-x p1)) (point-x p2)))  ; hack: - only takes 2 args
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
    ))]
  
  
  #:methods gen:multiplicible
  [(define (multiply p1 coefficient)
     (define (multiply-iter coef result)
       (if (= coef 0)
           result
           (multiply-iter (- coef 1) (+ result p1))))
     (multiply-iter coefficient (point 0 0)))]
  )

(provide field-element point + - * / expt)