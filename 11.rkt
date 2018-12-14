#lang racket

(struct grid
  [serial-number cells])

(define (make-grid serial-number)
  (grid serial-number (make-vector (* 300 300) #f)))

(define (hundreds n)
  (let ([v (quotient n 100)])
    (remainder v 10)))

 (define (power-level serial-number x y)
   (let ([rack-id (+ x 10)])
     (- (hundreds
       (* (+ (* y rack-id)
             serial-number)
          rack-id))
      5)))

(module+ test
  (require rackunit)

  (check-equal? (power-level 8    3   5)  4)
  (check-equal? (power-level 57 122  79) -5)
  (check-equal? (power-level 39 217 196)  0)
  (check-equal? (power-level 71 101 153)  4))

(define (grid-index x y)
  (+ (sub1 x) (* 300 (sub1 y))))

(define (grid-ref grid x y)
  (vector-ref (grid-cells grid) (grid-index x y)))

(define (grid-set! grid x y v)
  (vector-set! (grid-cells grid) (grid-index x y) v)
  v)

(define (grid-cell-power! grid x y)
  (or (grid-ref grid x y)
      (grid-set! grid x y
                 (power-level
                   (grid-serial-number grid) x y))))

(module+ test
  (let ([g (make-grid 8)])
    (check-equal? (grid-cell-power! g 3 5) 4)
    (check-equal? (grid-cell-power! g 3 5) 4)))

(define (grid-block-power! grid x y)
  (for*/sum ([y (in-range y (+ y 3))]
             [x (in-range x (+ x 3))])
    (grid-cell-power! grid x y)))

(module+ test
  (check-equal? (grid-block-power! (make-grid 18) 33 45) 29)
  (check-equal? (grid-block-power! (make-grid 42) 21 61) 30))

(define (grid-find-max-block! grid)
  (for*/fold ([v -1000] [coord #f] #:result coord)
             ([y (in-range 1 299)]
              [x (in-range 1 299)])
    (let ([pv (grid-block-power! grid x y)])
      (if (> pv v)
          (values pv (cons x y))
          (values v coord)))))

(module+ test
  (check-equal? (grid-find-max-block! (make-grid 18)) '(33 . 45))
  (check-equal? (grid-find-max-block! (make-grid 42)) '(21 . 61)))

(module+ main
  (displayln "PART ONE")
  (displayln
    (match-let ([(cons x y) (time (grid-find-max-block! (make-grid 3613)))])
      (~a x "," y))))
