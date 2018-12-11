#lang racket

(require threading)

(struct clist
  [before after]
  #:transparent)

(define (make-clist)
  (clist null null))

(define (clist-insert cl v)
  (match-let ([(clist b a) cl])
    (clist b (cons v a))))

(define (clist-current cl)
  (match cl
    [(clist _ (list cur _ ...)) cur]))

(define (clist-remove cl)
  (match cl
    [(clist before (list cur after ...)) (clist before after)]))

(define ((clist-mover move1) cl n)
  (define-values (before after)
    (for/fold ([before (clist-before cl)]
               [after (clist-after cl)])
              ([i (in-range n)])
      (move1 before after)))
  (clist before after))

(define (right-move1 before after)
  (match* (before after)
    [('() '()) (values null null)]
    [(before '()) (right-move1 '() (reverse before))]
    [(before (list cur after ...)) (values (cons cur before) after)]))

(define clist-move-right (clist-mover right-move1))

(define (left-move1 before after)
  (match* (before after)
    [('() '()) (values null null)]
    [('() after) (left-move1 (reverse after) '())]
    [((list before0 before ...) after)
     (values before (cons before0 after))]))

(define clist-move-left (clist-mover left-move1))

(module+ test
  (require rackunit)

  (check-match (clist-insert (make-clist) 'a)
               (clist '() '(a)))

  (check-match
   (for/fold ([cl (make-clist)]) ([v (in-list '(a b c d e))])
     (clist-insert cl v))
   (clist '() '(e d c b a)))

  (check-match
   (~> (make-clist)
       (clist-insert 'a)
       (clist-insert 'b)
       (clist-insert 'c)
       clist-current)
   'c)

  (check-match
   (~> (make-clist)
       (clist-insert 'a)
       (clist-insert 'b)
       (clist-insert 'c)
       (clist-insert 'd)
       clist-remove)
   (clist '() '(c b a)))

  (check-match
   (~> (make-clist)
       (clist-insert 'a)
       (clist-insert 'b)
       (clist-insert 'c)
       (clist-insert 'd)
       (clist-move-right 2))
   (clist '(c d) '(b a)))

  (check-match
   (~> (clist '(c b a) null)
       (clist-move-right 2))
   (clist '(b a) '(c)))

  (check-match
   (~> (clist '() '(c b a))
       (clist-move-left 1))
   (clist '(b c) '(a)))

  (check-match
   (for/fold ([cl (clist-insert (make-clist) 0)]) ([i (in-range 1 22)])
     (~> (clist-move-right cl 2)
         (clist-insert i)))
   (clist '(10 20 2 19 9 18 4 17 8 16 0) '(21 5 11 1 12 6 13 3 14 7 15))))

(define (part-one num-players rounds)
  (define scores (make-vector num-players 0))
  (for/fold ([cl (make-clist)])
            ([i (in-range (add1 rounds))]
             [s (in-cycle (in-range 23))]
             [p (in-cycle (in-range num-players))])
    (if (and (zero? s) (not (zero? i)))
        (let ([cl (clist-move-left cl 7)])
          (vector-set! scores p (+ (vector-ref scores p) i (clist-current cl)))
          (clist-remove cl))
        (let ([cl (clist-move-right cl 2)])
          (clist-insert cl i))))
  (for/fold ([m 0]) ([s (in-vector scores)]) (max m s)))

(module+ test
  (check-equal? (part-one 9 25) 32)
  (check-equal? (part-one 10 1618) 8317)
  (check-equal? (part-one 13 7999) 146373)
  (check-equal? (part-one 17 1104) 2764)
  (check-equal? (part-one 21 6111) 54718)
  (check-equal? (part-one 30 5807) 37305))

