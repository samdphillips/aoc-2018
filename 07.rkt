#lang racket

(require unstable/sequence
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

(define (part-one g)
  (define links
    (for/fold ([ht (hasheq)]) ([(pred succ) (in-pairs g)])
      (~> (hash-update ht pred values seteq)
          (hash-update succ
                       (lambda~> (set-add pred))
                       seteq))))
  (walk links))

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

(module* main #f
  (displayln "PART ONE")
  (displayln
   (~>> (time
         (part-one
          (call-with-input-file "inputs/07.txt" load-input)))
        (map (lambda~> symbol->string))
        (apply string-append))))

