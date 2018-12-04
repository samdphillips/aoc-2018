#lang racket/base

(require racket/set)

(define (part-one inp)
  (for/fold ([i 0]) ([n (in-port read inp)])
    (+ i n)))

(module+ test
  (require racket/port
           rackunit)

  (define (check f s v)
    (check-equal?
     (call-with-input-string s f) v))

  (check part-one "+1 -2 +3 +1" 3)
  (check part-one "+1 +1 +1" 3)
  (check part-one "+1 +1 -2" 0)
  (check part-one "-1 -2 -3" -6))

(define (part-two inp)
  (let/ec ret
    (define seen (mutable-set 0))
    (define elems
      (in-cycle (sequence->stream (in-port read inp))))
    (for/fold ([i 0]) ([n elems])
      (define j (+ i n))
      (when (set-member? seen j)
        (ret j))
      (set-add! seen j)
      j)
    seen))

(module+ test
  (check part-two "+1 -2 +3 +1" 2)
  (check part-two "+1 -1" 0)
  (check part-two "+3 +3 +4 -2 -4" 10)
  (check part-two "-6 +3 +8 +5 -6" 5)
  (check part-two "+7 +7 -2 -7 -4" 14))

(module* main #f
  (displayln "PART ONE")
  (displayln
   (call-with-input-file "inputs/01.txt" part-one))

  (displayln "PART TWO")
  (displayln
   (call-with-input-file "inputs/01.txt" part-two)))

