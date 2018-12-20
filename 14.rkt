#lang racket

(define (digits n [acc null])
  (if (zero? n)
      (if (null? acc)
          '(0)
          acc)
      (let-values ([(q r) (quotient/remainder n 10)])
        (digits q (cons r acc)))))

(define (run num-recipes [seed '(3 7)])
  (for/fold ([rs seed]
             [i 0]
             [j 1]
             #:result (take (drop rs num-recipes) 10))
            ([x (in-naturals)]
             #:break (> (length rs) (+ 10 num-recipes)))
    (define a (list-ref rs i))
    (define b (list-ref rs j))
    (let* ([rs  (append rs (digits (+ a b)))]
           [lrs (length rs)])
      (values rs
              (modulo (+ i a 1) lrs)
              (modulo (+ j b 1) lrs)))))

(module+ test
  (require rackunit)
  (check-equal? (run    9) '(5 1 5 8 9 1 6 7 7 9))
  (check-equal? (run    5) '(0 1 2 4 5 1 5 8 9 1))
  (check-equal? (run   18) '(9 2 5 1 0 7 1 0 8 5))
  (check-equal? (run 2018) '(5 9 4 1 4 2 9 8 8 2))
  )

#;
(module+ main
  (let ([rs (run 12 '(0 0))])
    (for ([i (in-range 1 (- (length rs) 10))])
      (displayln (~a "[" (~a #:width 4 #:align 'right i) "] "
                     (take (drop rs i) 10))))))
