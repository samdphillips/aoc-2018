#lang racket/base

(require racket/generator
         racket/match
         racket/sequence
         racket/stream
         srfi/67
         threading)

(require racket/list)

(struct entry
  [year month day hour minute message]
  #:transparent)

;; [1518-04-30 00:46] wakes up
(define MESSAGE-PAT
  #px"\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] (.*)")

(define GUARD-PAT
  #px"Guard #(\\d+)")

(define s->i string->number)

(define (parse-entry s)
  (match-let ([(list _ y m d h mn msg) (regexp-match MESSAGE-PAT s)])
    (entry (s->i y) (s->i m) (s->i d) (s->i h) (s->i mn)
           (cond
             [(regexp-match GUARD-PAT msg) => (lambda~> cadr s->i)]
             [(string=? msg "wakes up") 'wake]
             [(string=? msg "falls asleep") 'sleep]
             [else #f]))))

(module+ test
  (require rackunit)

  (check-match (parse-entry "[1518-04-30 00:46] wakes up")
               (entry 1518 4 30 0 46 'wake))

  (check-match (parse-entry "[1518-10-28 23:58] Guard #277 begins shift")
               (entry 1518 10 28 23 58 277))

  (check-match (parse-entry "[1518-10-05 00:23] falls asleep")
               (entry 1518 10 5 0 23 'sleep)))


(define (entry-compare e1 e2)
  (match-let ([(entry y1 m1 d1 h1 n1 _) e1]
              [(entry y2 m2 d2 h2 n2 _) e2])
    (refine-compare (integer-compare y1 y2)
                    (integer-compare m1 m2)
                    (integer-compare d1 d2)
                    (integer-compare h1 h2)
                    (integer-compare n1 n2))))

(module+ test
  (check-equal? (entry-compare
                 (parse-entry "[1518-04-30 00:46] wakes up")
                 (parse-entry "[1518-04-30 00:46] wakes up"))
                0)
  (check-equal? (entry-compare
                 (parse-entry "[1518-04-30 00:45] wakes up")
                 (parse-entry "[1518-04-30 00:46] wakes up"))
                -1)
  (check-equal? (entry-compare
                 (parse-entry "[1518-04-30 00:46] wakes up")
                 (parse-entry "[1518-04-30 00:45] wakes up"))
                1))

(define entry<? (<? entry-compare))

(struct nap
  [guard start end]
  #:transparent)

(struct nap-stream
  [current-guard entries]
  #:methods gen:stream
  [(define (stream-empty? s)
     (match s
       [(nap-stream _ '()) #t]
       [_ #f]))

   (define (stream-first s)
     (match s
       [(nap-stream _
                    (list (entry _ _ _ _ _ (? integer?)) ...
                          (entry _ _ _ _ _ (? integer? guard-id))
                          (entry _ _ _ _ s 'sleep)
                          (entry _ _ _ _ w 'wake)
                          _ ...))
        (nap guard-id s w)]
       [(nap-stream (? integer? guard-id)
                    (list (entry _ _ _ _ s 'sleep)
                          (entry _ _ _ _ w 'wake)
                          _ ...))
        (nap guard-id s w)]))

   (define (stream-rest s)
     (match s
       [(nap-stream _
                    (list (entry _ _ _ _ _ (? integer?)) ...
                          (entry _ _ _ _ _ (? integer? guard-id))
                          (entry _ _ _ _ _ 'sleep)
                          (entry _ _ _ _ _ 'wake)
                          rest ...))
        (nap-stream guard-id rest)]
       [(nap-stream (? integer? guard-id)
                    (list (entry _ _ _ _ _ 'sleep)
                          (entry _ _ _ _ _ 'wake)
                          rest ...))
        (nap-stream guard-id rest)]))])
                    
(define (entries->nap-stream es)
  (nap-stream #f es))

(module+ test
  (let ([es (list (entry 1518 3 6 0 0  242)
                  (entry 1518 3 6 0 10 'sleep)
                  (entry 1518 3 6 0 20 'wake))])
    (check-equal?
     (stream->list (entries->nap-stream es))
     (list (nap 242 10 20))))
  
  (let ([es (list (entry 1518 3 6 0 0  242)
                  (entry 1518 3 6 0 10 'sleep)
                  (entry 1518 3 6 0 20 'wake)
                  (entry 1518 3 6 0 0  244)
                  (entry 1518 3 6 0 15 'sleep)
                  (entry 1518 3 6 0 25 'wake))])
    (check-equal?
     (stream->list (entries->nap-stream es))
     (list (nap 242 10 20)
           (nap 244 15 25))))

  (let ([es (list (entry 1518 3 6 0 0  242)
                  (entry 1518 3 6 0 10 'sleep)
                  (entry 1518 3 6 0 20 'wake)                  
                  (entry 1518 3 6 0 15 'sleep)
                  (entry 1518 3 6 0 25 'wake))])
    (check-equal?
     (stream->list (entries->nap-stream es))
     (list (nap 242 10 20)
           (nap 242 15 25))))

  (let ([es (list (entry 1518 3 6 0 0  242)
                  (entry 1518 3 6 0 0  243)
                  (entry 1518 3 6 0 10 'sleep)
                  (entry 1518 3 6 0 20 'wake)                  
                  (entry 1518 3 6 0 15 'sleep)
                  (entry 1518 3 6 0 25 'wake))])
    (check-equal?
     (stream->list (entries->nap-stream es))
     (list (nap 243 10 20)
           (nap 243 15 25))))

  (let ([es (list (entry 1518 3 6 0 0  242)
                  (entry 1518 3 6 0 0  245)
                  (entry 1518 3 6 0 0  243)
                  (entry 1518 3 6 0 10 'sleep)
                  (entry 1518 3 6 0 20 'wake)                  
                  (entry 1518 3 6 0 15 'sleep)
                  (entry 1518 3 6 0 25 'wake))])
    (check-equal?
     (stream->list (entries->nap-stream es))
     (list (nap 243 10 20)
           (nap 243 15 25)))))

(struct guard-sleep-schedule
  [minutes])

(define (make-guard-sleep-schedule)
  (guard-sleep-schedule (make-vector 60 0)))

(define (guard-sleep-schedule-update! s n)
  (match-define (nap _ sleep wake) n)
  (define v (guard-sleep-schedule-minutes s))
  (for ([i (in-range sleep wake)])
    (vector-set! v i (add1 (vector-ref v i)))))

(module+ test
  (require racket/format)
  
  (let ([n (nap 243 10 20)]
        [s (make-guard-sleep-schedule)])
    (guard-sleep-schedule-update! s n)
    (let ([v (guard-sleep-schedule-minutes s)])
      (for ([i (in-range 10 20)])
        (check-equal? 1 (vector-ref v i) (~a i)))
      (for ([i (in-range 0 10)])
        (check-equal? 0 (vector-ref v i) (~a i)))
      (for ([i (in-range 20 60)])
        (check-equal? 0 (vector-ref v i) (~a i)))))

  (let ([s (make-guard-sleep-schedule)])
    (guard-sleep-schedule-update! s (nap 243 10 20))
    (guard-sleep-schedule-update! s (nap 243 15 25))
    (let ([v (guard-sleep-schedule-minutes s)])
      (for ([i (in-range 15 20)])
        (check-equal? 2 (vector-ref v i) (~a i)))
      (for ([i (in-range 10 15)])
        (check-equal? 1 (vector-ref v i) (~a i)))
      (for ([i (in-range 20 25)])
        (check-equal? 1 (vector-ref v i) (~a i)))
      (for ([i (in-range 0 10)])
        (check-equal? 0 (vector-ref v i) (~a i)))
      (for ([i (in-range 25 60)])
        (check-equal? 0 (vector-ref v i) (~a i))))))

(define (collect-naps ns)
  (define ht (make-hasheq))
  (for ([n (in-stream ns)])
    (hash-update! ht (nap-guard n)
                  (lambda (s)
                    (guard-sleep-schedule-update! s n)
                    s)
                  make-guard-sleep-schedule))
  ht)

(define (guard-minutes-slept s)
  (for/sum ([t (in-vector (guard-sleep-schedule-minutes s))]) t))

(define (guard-most-slept-minute s)
  (for/fold ([n #f] [v 0]) ([m (in-naturals)]
                            [t (in-vector (guard-sleep-schedule-minutes s))])
    (if (> t v) (values m t) (values n v))))

(define (calculate-part-one-result ht)
  (define guard
    (call-with-values
     (lambda ()
       (for/fold ([g #f] [m 0]) ([(i s) (in-hash ht)])
         (let ([n (guard-minutes-slept s)])
           (if (> n m) (values i n) (values g m)))))
     (lambda (g m) g)))
  (define minute
    (call-with-values
     (lambda ()
       (guard-most-slept-minute (hash-ref ht guard)))
     (lambda (m n) m)))
  (* guard minute))

(define (part-one)
  (~> (call-with-input-file "inputs/04.txt"
        (lambda (inp)
          (for/list ([line (in-port read-line inp)])
            (parse-entry line))))
      (sort entry<?)
      entries->nap-stream
      collect-naps
      calculate-part-one-result))

(define (enumerate-guards-minutes ht)
  (in-generator
          (for ([(g s) (in-hash ht)])
            (for ([i (in-naturals)]
                  [m (in-vector (guard-sleep-schedule-minutes s))])
              (yield (list m g i))))))

(define (part-two)
  (~> (call-with-input-file "inputs/04.txt"
        (lambda (inp)
          (for/list ([line (in-port read-line inp)])
            (parse-entry line))))
      (sort entry<?)
      entries->nap-stream
      collect-naps
      enumerate-guards-minutes
      sequence->list
      (sort > #:key car)
      ((match-lambda
         [(cons (list _ a b) _) (* a b)]))))
