#lang racket/base

(require racket/match
         racket/sequence)

(require threading)

(struct loc
  [x y (size #:mutable) (boundary? #:mutable)]
  #:transparent)

(define (incr-loc-size! l)
  (set-loc-size! l (add1 (loc-size l))))

(define COORD-PAT
  #px"(\\d+)\\D+(\\d+)")

(define (parse-coord line)
  (match (regexp-match COORD-PAT line)
    [(list _ x y) (loc (string->number x)
                       (string->number y)
                       0
                       #f)]))

(define (distance x y l)
  (match-define (loc lx ly _ _) l)
  (+ (abs (- lx x)) (abs (- ly y))))

(define (closest locs x y)
  (define-values (l d)
    (for/fold ([l #f] [d 1000000]) ([cur (in-list locs)])
      (define nd (distance x y cur))
      (cond
        [(< nd d) (values cur nd)]
        [(= nd d) (values #f nd)]
        [else (values l d)])))
  l)

(define (loc-bounds locs)
  (for/fold ([minx 1000] [maxx -1000] [miny 1000] [maxy -1000])
            ([l (in-list locs)])
    (match-define (loc x y _ _) l)
    (values (min minx x)
            (max maxx x)
            (min miny y)
            (max maxy y))))

(define (update-locs! locs)
  (define-values (minx maxx miny maxy) (loc-bounds locs))
  (for* ([y (in-range miny (add1 maxy))]
         [x (in-range minx (add1 maxx))])
    (let ([l (closest locs x y)])
      (when l
        (incr-loc-size! l)
        (when (or (= x 1) (= x 8)
                  (= y 1) (= y 9))
          (set-loc-boundary?! l #t))))))

(define (biggest-loc-size locs)
  (for/fold ([a 0]) ([l (in-list locs)]
                     #:when (not (loc-boundary? l)))
    (max a (loc-size l))))

(module* main #f
  (define locs
    (call-with-input-file "inputs/06.txt"
      (lambda~>>
       (in-port read-line)
       (sequence-map parse-coord)
       sequence->list)))

  (displayln "PART ONE")
  (displayln
   (time
    (update-locs! locs)
    (biggest-loc-size locs)))

  (displayln "PART TWO")
  (define-values (minx maxx miny maxy) (loc-bounds locs))
  (displayln
   (time
    (for*/sum ([y (in-range miny maxy)]
               [x (in-range minx maxx)])
      (define total
        (for/sum ([l (in-list locs)])
          (distance x y l)))
      (if (< total 10000) 1 0)))))


