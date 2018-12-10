#lang racket

(struct node
  [children metadata]
  #:transparent)

(define (read-node seq-gen)
  (let ([num-children (seq-gen)]
        [num-metadata (seq-gen)])
    (node (for/list ([n num-children]) (read-node seq-gen))
          (for/list ([n num-metadata]) (seq-gen)))))

(define (sum-metadata n)
  (match-let ([(node children metadata) n])
    (apply + (apply + metadata) (map sum-metadata children))))

(define (load-tree seq)
  (define-values (more? gen) (sequence-generate seq))
  (read-node gen))

(define (part-one tree)
  (sum-metadata tree))

(module+ test
  (require rackunit)

  (define test-tree
    (load-tree '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))
  (check-equal? (part-one test-tree) 138))

(module+ main
  (define tree
    (call-with-input-file "inputs/08.txt"
      (lambda (inp)
        (load-tree (in-port read inp)))))

  (displayln "PART ONE")
  (displayln (part-one tree)))

(define (child-ref c* i)
  (cond
    [(null? c*) #f]
    [(zero? i) (car c*)]
    [else (child-ref (cdr c*) (sub1 i))]))

(define (node-child n i)
  (if (zero? i)
      #f
      (child-ref (node-children n) (sub1 i))))

(define (node-value n)
  (cond
    [(null? (node-children n)) (apply + (node-metadata n))]
    [else
     (for/sum ([i (in-list (node-metadata n))])
       (let ([c (node-child n i)])
         (if c
             (node-value c)
             0)))]))

(define (part-two tree)
  (node-value tree))

(module+ test
  (check-equal? (part-two test-tree) 66))

(module+ main
  (displayln "PART TWO")
  (displayln (part-two tree)))