#lang racket

(struct cart
  [dir turn (move #:mutable)]
  #:transparent)

(struct layout
  [width height cells])

(define (layout-offset l x y)
  (+ x (* y (layout-width l))))

(define (layout-set! l x y v)
  (vector-set! (layout-cells l) (layout-offset l x y) v))

(define (make-track-layout w h cs*)
  (define size (* w h))
  (define cells (make-vector size #f))
  (define (fill! i r cs cs*)
    (cond
     [(and (null? cs) (null? cs*))
      (layout w h cells)]
     [(null? cs)
      (let ([i   (* w (add1 r))]
            [r   (add1 r)]
            [cs  (car cs*)]
            [cs* (cdr cs*)])
        (fill! i r cs cs*))]
     [else
      (vector-set! cells i (car cs))
      (fill! (add1 i) r (cdr cs) cs*)]))
  (fill! 0 0 (car cs*) (cdr cs*)))

(define (make-cart-layout w h carts)
  (define cart-layout (layout w h (make-vector (* w h) #f)))
  (for ([cinfo (in-list carts)])
    (match-define (list x y dir) cinfo)
    (layout-set! cart-layout x y (cart dir 'L 0)))
  cart-layout)

(define (read-track-description inp)
  (define (read-track-char x y longest-row current-row layout carts)
    (define (add-cell c)
      (read-track-char
        (add1 x) y longest-row (cons c current-row) layout carts))
    (define (add-cart dir)
      (read-track-char
        (add1 x) y longest-row
        (cons (if (or (eq? dir 'N) (eq? dir 'S)) 'NS 'WE) current-row)
        layout
        (cons (list x y dir) carts)))
    (define (add-row) (cons (reverse current-row) layout))
    (define (finish-read y layout)
      (let ([longest-row (max longest-row x)])
        (values (make-track-layout longest-row y (reverse layout))
                (make-cart-layout longest-row y carts))))
    (match (read-char inp)
      [(? eof-object?)
       (finish-read (add1 y) (add-row))]
      [#\newline
       (if (eof-object? (peek-char inp))
           (finish-read y layout)
           (read-track-char
             0 (add1 y) (max longest-row x) null (add-row) carts))]
      [#\space (add-cell #f)]
      [#\|     (add-cell 'NS)]
      [#\-     (add-cell 'WE)]
      [#\/     (add-cell 'SE)]
      [#\\     (add-cell 'SW)]
      [#\+     (add-cell '+)]
      [#\v (add-cart 'S)]
      [#\^ (add-cart 'N)]
      [#\> (add-cart 'E)]
      [#\< (add-cart 'W)]))
  (read-track-char 0 0 0 null null null))

(module+ main
  (define straight-test #<<INPUT
|
v
|
|
|
^
|
INPUT
)
  (call-with-input-string straight-test read-track-description)



)
#;
(call-with-input-file "inputs/13.txt" read-track-description)
