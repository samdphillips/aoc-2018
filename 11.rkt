#lang racket

(struct grid
  [serial-number levels])

(define (make-grid serial-number)
  (grid serial-number
        (build-vector 300 (lambda (i) (make-vector (* 300 300) #f)))))

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

(define (grid-cells grid level)
  (vector-ref (grid-levels grid) (sub1 level)))

(define (grid-ref grid level x y)
  (vector-ref (grid-cells grid level) (grid-index x y)))

(define (grid-set! grid level x y v)
  (vector-set! (grid-cells grid level) (grid-index x y) v)
  v)

(define (in-edge-cells size x y)
  (in-sequences
    (in-parallel
      (in-range x (+ x size))
      (in-cycle (in-value (+ y size -1))))
    (in-parallel
      (in-cycle (in-value (+ x size -1)))
      (in-range y (+ y size -1)))))

(define (grid-region-power! grid size x y)
  (define (calculate-power!)
    (if (= 1 size)
        (power-level (grid-serial-number grid) x y)
        (+ (grid-region-power! grid (sub1 size) x y)
           (for/sum ([(x y) (in-edge-cells size x y)])
             (grid-region-power! grid 1 x y)))))
  (or (grid-ref grid size x y)
      (grid-set! grid size x y (calculate-power!))))

(module+ test
  (check-equal? (grid-region-power! (make-grid 8)  1   3   5) 4)
  (check-equal? (grid-region-power! (make-grid 39) 1 217 196)  0))

(define (grid-cell-power! grid x y)
  (grid-region-power! grid 1 x y))

(module+ test
  (let ([g (make-grid 8)])
    (check-equal? (grid-cell-power! g 3 5) 4)
    (check-equal? (grid-cell-power! g 3 5) 4)))

(define (grid-block-power! grid x y)
  (grid-region-power! grid 3 x y))

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
  (define g (make-grid 3613))
  (displayln "PART ONE")
  (displayln
    (match-let ([(cons x y) (time (grid-find-max-block! g))])
      (~a x "," y))))

(define (grid-find-max-region! grid cycles)
  (for*/fold ([v -1000] [coord #f] #:result coord)
             ([r (in-range 1 (add1 cycles))]
              [y (in-range 1 (- 302 r))]
              [x (in-range 1 (- 302 r))])
    (when (= 1 x y)
      (displayln
        (~a "level: "
            (~a #:width 3 r)
            " best: "
            (~a #:width 5 v)
            " " coord)))
    (let ([v1 (grid-region-power! grid r x y)])
      (if (> v1 v)
          (values v1 (list x y r))
          (values v coord)))))

(module+ main
  ;; only needed to go to size 13
  (displayln "PART TWO")
  (displayln
    (match-let ([(list x y z) (time (grid-find-max-region! g 300))])
      (~a #:separator "," x y z))))
