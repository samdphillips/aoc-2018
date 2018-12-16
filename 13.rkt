#lang racket

(require srfi/67)

(struct cart
  [x y dir turn]
  #:transparent)

(define-syntax-rule (refine-compare-parts a b (extract compare) ...)
  (refine-compare
    (compare (extract a) (extract b)) ...))

(define (cart-compare a b)
  (refine-compare-parts a b
    [cart-y number-compare]
    [cart-x number-compare]))

(define cart<? (<? cart-compare))

(struct layout
  [width height cells])

(struct state
  [carts layout])

(define (make-layout w h cs*)
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
        (cons (cart x y dir 'L) carts)))
    (define (add-row) (cons (reverse current-row) layout))
    (define (finish-read y layout)
      (values (make-layout (max longest-row x) y (reverse layout))
              (sort carts cart<?)))
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
