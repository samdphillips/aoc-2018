#lang racket

(require threading)

(struct star
  [x y dx dy]
  #:transparent)

(define-syntax-rule (define-loader name pat ([var conv] ...) maker)
  (define name
    (lambda~>>
      (in-port read-line)
      (sequence-map
        (lambda (line)
          (match (regexp-match pat line)
            [(list _ (app conv var) ...) maker]))))))

(define-loader load-data
  ; position=<-10229,  31405> velocity=< 1, -3>
  #px"position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>"
  ([x string->number]
   [y string->number]
   [dx string->number]
   [dy string->number])
  (star x y dx dy))

(struct bbox
  [left right top bottom]
  #:transparent)

(define (update-star s)
  (match-define (star x y dx dy) s)
  (star (+ x dx) (+ y dy) dx dy))

(module+ test
  (require rackunit)

  (check-equal? (update-star (star 10 10 0 0))
                (star 10 10 0 0))
  (check-equal? (update-star (star 10 10 10 10))
                (star 20 20 10 10)))

(define (bbox-calculate stars)
  (define-syntax-rule (mm/or v1 v2 f)
    (if v1 (f v1 v2) v2))
  (define-syntax-rule (min/or v1 v2)
    (mm/or v1 v2 min))
  (define-syntax-rule (max/or v1 v2)
      (mm/or v1 v2 max))
  (define-values (left right top bottom)
    (for/fold ([l #f] [r #f] [t #f] [b #f]) ([s (in-list stars)])
      (match-define (star x y _ _) s)
      (values (min/or l x)
              (max/or r x)
              (min/or t y)
              (max/or b y))))
  (bbox left right top bottom))

(module+ test
  (check-equal? (bbox-calculate (list (star 100 -100 0 0)
                                      (star 100 100 0 0)
                                      (star -1000 -500 0 0)))
                (bbox -1000 100 -500 100)))

(define (bbox-size b)
  (match-define (bbox left right top bottom) b)
  (values (abs (- left right)) (abs (- top bottom))))

(module+ test
  (let-values ([(w h) (bbox-size (bbox -100 100 -500 500))])
    (check-equal? w 200)
    (check-equal? h 1000)))

(define (screen-bounds? stars)
  (let-values ([(w h) (bbox-size (bbox-calculate stars))])
    (and (< w 80) (< h 25))))

(define (step* stars [i 100000])
  (let ([stars (map update-star stars)])
    (cond
     [(or (zero? i) (screen-bounds? stars))
      (displayln (- 100000 i))
      stars]
     [else
       (step* stars (sub1 i))])))

(define (part-one)
  (define stars
    (call-with-input-file "inputs/10.txt" (lambda~> load-data sequence->list)))
  (step* stars))

(define (render-stars ox oy stars)
  (define field
    (make-vector (* 80 25) #\space))
  (define (plot! x y)
    (vector-set! field (+ x (* y 80)) #\#))
  (for ([s (in-list stars)])
    (plot! (+ ox (star-x s)) (+ oy (star-y s))))
  (call-with-output-string
    (lambda (outp)
      (for ([o1 (in-range 0  (* 80 26) 80)]
            [o2 (in-range 80 (* 80 26) 80)])
        (~> (vector-copy field o1 o2)
            vector->list
            list->string
            (displayln outp))))))

(module+ main
  (define stars (part-one))
  (for/fold ([stars stars] #:result (void)) ([i 10])
    (displayln "---")
    (match-define (bbox l _ t _) (bbox-calculate stars))
    (displayln
      (render-stars (- l) (- t) stars))
    (step* stars)))
