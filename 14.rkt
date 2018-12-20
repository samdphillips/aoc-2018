#lang racket

(define (digits n [acc null])
  (if (zero? n)
      (if (null? acc)
          '(0)
          acc)
      (let-values ([(q r) (quotient/remainder n 10)])
        (digits q (cons r acc)))))

(define (run num-recipes [seed '(3 7)])
  (define rs (make-vector (+ num-recipes 12)))
  (define lrs 0)
  (define (add-rs! vs)
    (for ([i (in-naturals lrs)]
          [v (in-list vs)])
      (vector-set! rs i v))
    (set! lrs (+ lrs (length vs))))
  (add-rs! seed)

  (let repeat ([i 0] [j 1])
    (unless (> lrs (+ 10 num-recipes))
      (define a (vector-ref rs i))
      (define b (vector-ref rs j))
      (add-rs! (digits (+ a b)))
      (repeat (modulo (+ i a 1) lrs)
              (modulo (+ j b 1) lrs))))

  (for/list ([i (in-range num-recipes (+ 10 num-recipes))])
    (vector-ref rs i)))

(module+ test
  (require rackunit)
  (check-equal? (run    9) '(5 1 5 8 9 1 6 7 7 9))
  (check-equal? (run    5) '(0 1 2 4 5 1 5 8 9 1))
  (check-equal? (run   18) '(9 2 5 1 0 7 1 0 8 5))
  (check-equal? (run 2018) '(5 9 4 1 4 2 9 8 8 2))
  )

(module+ main
  (displayln "PART ONE")
  (displayln
    (apply ~a (time (run 681901)))))

(define (run2 pattern [seed '(3 7)])
  (define plen (length pattern))
  (define rs (make-bytes 10))
  (define lrs 0)
  (define (match?)
    (and (>= lrs plen)
         (for/and ([v (in-list pattern)]
                   [i (in-naturals (- lrs plen))])
           (= v (bytes-ref rs i)))))
  (define (resize!)
    (define new-size (* 2 lrs))
    (define old rs)
    (set! rs (make-bytes new-size))
    (bytes-copy! rs 0 old))
  (define (add-rs! break vs)
    (unless (< (+ lrs (length vs)) (bytes-length rs))
      (resize!))
    (for ([i (in-naturals lrs)]
          [v (in-list vs)])
      (set! lrs (add1 lrs))
      (bytes-set! rs i v)
      (when (match?) (break))))
  (add-rs! void seed)

  (let/ec break
    (let repeat ([i 0] [j 1])
      (define a (bytes-ref rs i))
      (define b (bytes-ref rs j))
      (add-rs! break (digits (+ a b)))
      (repeat (modulo (+ i a 1) lrs)
              (modulo (+ j b 1) lrs))))

  (- lrs plen))

(module+ test
  (check-equal? (run2 '(5 1 5 8 9)) 9)
  (check-equal? (run2 '(0 1 2 4 5)) 5)
  (check-equal? (run2 '(9 2 5 1 0)) 18)
  (check-equal? (run2 '(5 9 4 1 4)) 2018))

(module+ main
  (displayln "PART TWO")
  (displayln
    (time (run2 '(6 8 1 9 0 1)))))
