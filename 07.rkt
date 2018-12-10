#lang racket

(require data/heap
         unstable/sequence
         threading)

; Step C must be finished before step A can begin.
(define LINE-PAT
  #px"Step (.) must be finished before step (.) can begin.")

(define (parse-line line)
  (match (regexp-match LINE-PAT line)
    [(list _ a b) (cons (string->symbol a)
                        (string->symbol b))]))

(define load-input
  (lambda~>>
   (in-port read-line)
   (sequence-map parse-line)
   sequence->list))

(define (next-node links)
  (for/fold ([n #f]) ([(succ preds) (in-hash links)])
    (match* (preds n)
      [((? set-empty?) #f) succ]
      [((? set-empty?) _)
       (if (symbol<? succ n) succ n)]
      [(_ _) n])))

(define (visit-node n links)
  (for/fold ([t (hasheq)]) ([(k v) (in-hash links)])
    (cond
      [(eq? k n) t]
      [else (hash-set t k (set-remove v n))])))

(define (walk links)
  (cond
    [(hash-empty? links) null]
    [else
     (let* ([n (next-node links)]
            [links (visit-node n links)])
       (cons n (walk links)))]))

(define ((solver walk) g)
  (define links
    (for/fold ([ht (hasheq)]) ([(pred succ) (in-pairs g)])
      (~> (hash-update ht pred values seteq)
          (hash-update succ
                       (lambda~> (set-add pred))
                       seteq))))
  (walk links))

(define part-one (solver walk))

(module+ test
  (require rackunit)

  (define test-input #<<TEST
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
TEST
    )

  (check-equal? (part-one (call-with-input-string test-input load-input))
                '(C A B D F E)))

(module+ main
  (displayln "PART ONE")
  (displayln
   (~>> (time
         (part-one
          (call-with-input-file "inputs/07.txt" load-input)))
        (map (lambda~> symbol->string))
        (apply string-append))))


(define running-in-test (make-parameter #f))
(define number-of-workers (make-parameter 5))

(define task-time
  (lambda~>
   symbol->string
   (string-ref 0)
   char->integer
   (- 64)
   (+ (if (running-in-test) 0 60))))

(define (heap-empty? h)
  (zero? (heap-count h)))

(define (task<? a b)
  (match-define (cons v0 t0) a)
  (match-define (cons v1 t1) b)
  (cond
    [(= v0 v1) (symbol<? t0 t1)]
    [else (< v0 v1)]))

(define (heap-min-time h)
  (car (heap-min h)))

(define (walk2 links)
  (define pending (make-heap task<?))
  (define (walk time workers links)
    (cond
      [(and (heap-empty? pending) (hash-empty? links) time)]
      [(> workers 0)
       (cond
         [(next-node links)
          =>
          (lambda (n)
            (let ([links (hash-remove links n)])
              (heap-add! pending (cons (+ time (task-time n)) n))
              (walk time
                    (sub1 workers)
                    links)))]
         [(= time (heap-min-time pending))
          (let* ([n (heap-min pending)]
                 [time  (car n)]
                 [links (visit-node (cdr n) links)])
            (heap-remove-min! pending)
            (walk time (min (number-of-workers) (add1 workers)) links))]
         [else
          (walk (add1 time) workers links)])]
      [else
       (let* ([n (heap-min pending)]
              [time  (car n)]
              [links (visit-node (cdr n) links)])
         (heap-remove-min! pending)
         (walk time (min (number-of-workers) (add1 workers)) links))]))
  (walk 0 (number-of-workers) links))

(define part-two (solver walk2))

(module+ test
  (check-equal?
   (parameterize [(running-in-test #t)
                  (number-of-workers 2)]
     (part-two (call-with-input-string test-input load-input)))
   15))

(module+ main
  (displayln "PART TWO")
  (displayln
   (time
    (part-two
     (call-with-input-file "inputs/07.txt" load-input)))))
