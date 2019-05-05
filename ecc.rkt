#lang racket
(require racket/generic
         racket/struct                              ; make-contructor-style-printer
         (only-in racket/base [+ racket:+])
         (only-in racket/base [- racket:-])
         (only-in racket/base [* racket:*])
         (only-in racket/base [/ racket:/])
         (only-in racket/base [expt racket:expt])
         )
(require math)

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
    [else (error 'add "
 add")]))      ; TODO: better error message

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
    ; numbers
    [(and (number? x) (number? y)) (racket:* x y)]
    ; field elements
    [(and (field-element? x) (field-element? y)) (multiply x y)]
    [(and (number? x) (field-element? y))
     (multiply (field-element x (field-element-prime y)) y)]
    [(and (field-element? x) (number? y))
     (multiply x (field-element y (field-element-prime x)))]
    ; points
    [(and (point? x) (number? y))
     (multiply x y)]
    [(and (number? x) (point? y))
     (multiply y x)]
    [else (error 'add "Can't multiply"
                 '("args" multi)
                 (list x y))]))      ; TODO: better error message

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
     ;;; FIXME: python has __mul__ and __rmul__ ...
     ;;; we basically only handle __mul__
     ;;; need to implement integer multiplication                    
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
     (define inv (modular-expt (field-element-number fe2)
                               (- (field-element-prime fe2) 2)
                               (field-element-prime fe1)))
     (field-element
      (modulo (* (field-element-number fe1) inv)
              (field-element-prime fe1))
      (field-element-prime fe1)))]
  )

(struct point (x y a b)
  #:transparent

  #:guard (λ (x y a b name)
            (unless (or (and (equal? x +inf.0) (equal? y +inf.0))
                        (equal? (* y y) (+ (+ (expt x 3) (* a x)) b)))    ; hack: + only takes 2 args ...
            (error 'point "not a valid point"))
            (values x y a b))

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
       (lambda (obj) 'point)
       (lambda (obj) (list 'x:
                           (field-element-number (point-x obj))
                           'y:
                           (field-element-number (point-y obj))
                           'a:
                           (field-element-number (point-a obj))
                           'b:
                           (field-element-number (point-b obj))
                           'mod:
                           (field-element-prime (point-y obj))))))]
  
  #:methods gen:addable
  [(define (add p1 p2)
(cond
    ; p1 is point-at-infinity
    [(equal? (point-x p1) +inf.0) p2]

    ; p1 is point-at-infinity
    [(equal? (point-x p2) +inf.0) p1]

    ; p1 is p2 reflected across x-axis
    [(and (equal? (point-x p1) (point-x p2))
          (not (equal? (point-y p1) (point-y p2))))
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
       ; FIXME: this takes forever with 256 bit numbers ...
       (define s (/ (+ (* (expt (point-x p1) 2) 3)
                     (point-a p1))
                  (* (point-y p1) 2)))
       (define x (- (expt s 2) (* 2 (point-x p1))))
       (define y (- (* (- (point-x p1) x) s)
                    (point-y p1)))
       (point x y (point-a p1) (point-b p1)))]
    ))]
  
  
  #:methods gen:multiplicible
  [(define (multiply p coefficient)
     (define zero (point +inf.0 +inf.0 (point-a p) (point-b p)))
     ; TODO: bit flipping trick here ... best shot at speeding this up ...
     (define (fast-multiply coef current result)
       (if (= coef 0)
           result
           (fast-multiply (arithmetic-shift coef -1)
                          (+ current current)
                          (if (= (bitwise-and coef 1) 1)
                              (+ result current)
                              result))))
     (fast-multiply coefficient p zero))]
       
  )

(define P (- (- (expt 2 256)
                (expt 2 32))
             977))
(define N #xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)

; multiplication needs to mod coefficient by N ...
(define (s256-field-element n) (field-element n P))

(define A (s256-field-element 0))
(define B (s256-field-element 7))

(define (s256-point x y)
  (point x y A B))

(define gx #x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)
(define gy #x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)

(define G (s256-point (s256-field-element gx)
                      (s256-field-element gy)))

(define INF (s256-point +inf.0 +inf.0))

;(display (* G 2))


(provide field-element point + - * / expt N G P

         point-x  ; yuck
         )

