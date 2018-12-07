#lang racket/base

(require racket/sequence)

(define (count-id s)
  (define dos #f)
  (define tres #f)
  (define seen-tbl (make-vector 26 0))
  (define (seen! c)
    (define i (- (char->integer c) 97))
    (vector-set! seen-tbl i (add1 (vector-ref seen-tbl i))))
  (for ([c (in-string s)]) (seen! c))
  (for ([v (in-vector seen-tbl)])
    (when (= v 2) (set! dos #t))
    (when (= v 3) (set! tres #t)))
  (values (or (and dos 1) 0)
          (or (and tres 1) 0)))

(define (part-one seq)
  (define-values (d t)
    (for/fold ([d 0] [t 0]) ([s seq])
      (call-with-values
       (lambda ()
         (count-id s))
       (lambda (a b)
         (values (+ a d) (+ b t))))))
  (* d t))

(module+ test
  (require racket/port
           rackunit)

  (define (check-counts s a b)
    (test-begin
     (call-with-values
      (lambda () (count-id s))
      (lambda (c d)
        (check-equal? c a "2s count")
        (check-equal? d b "3s count")))))

  (check-counts "abcdef" 0 0)
  (check-counts "bababc" 1 1)
  (check-counts "abbcde" 1 0)
  (check-counts "abcccd" 0 1)
  (check-counts "aabcdd" 1 0)
  (check-counts "abcdee" 1 0)
  (check-counts "ababab" 0 1)

  (check-equal?
   (let ([input (list "abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab")])
     (part-one input))
   12))

(define (close-enough s t)
  (define (close fk c* s* t*)
    (cond
      [(and (null? s*) (null? t*)) (list->string (reverse c*))]
      [(or (null? s*) (null? t*)) #f]
      [(char=? (car s*) (car t*))
       (close fk (cons (car s*) c*) (cdr s*) (cdr t*))]
      [else
       (fk c* s* t*)]))
  (define (fail c* s* t*)
    (close (lambda (c* s* t*) #f) c* (cdr s*) (cdr t*)))
  (close fail null (string->list s) (string->list t)))

(module+ test
  (check-equal? (close-enough "abcde" "fghij") #f)
  (check-equal? (close-enough "abcde" "axcye") #f)
  (check-equal? (close-enough "fghij" "fguij") "fgij"))

(define (part-two lines)
  (let ([lines (sort (sequence->list lines) string<?)])
    (for*/or ([a (in-list lines)]
              [b (in-list lines)]
              #:when (not (string=? a b)))
      (close-enough a b))))

(module+ test
  (check-equal?
   (let ([input (list "abcde"
                      "fghij"
                      "klmno"
                      "pqrst"
                      "fguij"
                      "axcye"
                      "wvxyz")])
     (part-two input))
   "fgij"))

(module* main #f
  (displayln "PART ONE")
  (displayln
   (call-with-input-file "inputs/02.txt"
     (lambda (inp)
       (part-one (in-port read-line inp)))))

  (displayln "PART TWO")
  (displayln
   (call-with-input-file "inputs/02.txt"
     (lambda (inp)
       (part-two (in-port read-line inp))))))
