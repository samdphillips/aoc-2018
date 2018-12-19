#lang racket

(struct cart
  [id dir turn move]
  #:transparent)

(struct layout
  [width height cells])

(define (layout-offset l x y)
  (match-define (layout w h _) l)
  (unless (and (< x w) (< y h))
    (error 'layout-offset "coord out of bounds (~a ~a) (~a ~a)"
           x y w h))
  (+ x (* y w)))

(define (layout-position l i)
  (call-with-values
    (lambda () (quotient/remainder i (layout-width l)))
    (lambda (y x) (values x y))))

(define (layout-set! l x y v)
  (vector-set! (layout-cells l) (layout-offset l x y) v))

(define (layout-ref l x y)
  (vector-ref (layout-cells l) (layout-offset l x y)))

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
    (layout-set! cart-layout x y (cart (gensym 'cart) dir 'L 0)))
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

(define (compute-move dir)
  (match dir
    ['S (values  0  1)]
    ['N (values  0 -1)]
    ['E (values  1  0)]
    ['W (values -1  0)]))

(define (intersect-dir dir turn)
  (match* (dir turn)
    [(dir 'F) dir]
    [('W 'L) 'S]
    [('W 'R) 'N]
    [('E 'L) 'N]
    [('E 'R) 'S]
    [('N 'L) 'W]
    [('N 'R) 'E]
    [('S 'L) 'E]
    [('S 'R) 'W]
    ))

(define (intersect-turn turn)
  (match turn
    ['L 'F]
    ['F 'R]
    ['R 'L]))

(define (compute-turn track cart)
  (define turn (cart-turn cart))
  (define (ret v) (values v turn))
  (match* (track (cart-dir cart))
    [((or 'NS 'WE) dir) (ret dir)]
    [('SW 'W) (ret 'N)]
    [('SW 'E) (ret 'S)]
    [('SW 'N) (ret 'W)]
    [('SW 'S) (ret 'E)]
    [('SE 'E) (ret 'N)]
    [('SE 'W) (ret 'S)]
    [('SE 'N) (ret 'E)]
    [('SE 'S) (ret 'W)]
    [('+ dir)
     (values (intersect-dir dir turn)
             (intersect-turn turn))]

    ))

(module+ test
  (define straight-test #<<INPUT
|
v
|
|
|
^
|
INPUT
))

(module* part-one #f
  (define-values (tracks carts)
    (call-with-input-file "inputs/13.txt" read-track-description))

  (define (run move)
    (for ([i (in-naturals)]
          [cur-cart (in-vector (layout-cells carts))]
          #:when (and cur-cart (= (cart-move cur-cart) move)))
      (define-values (x0 y0) (layout-position carts i))
      (define-values (dx dy) (compute-move (cart-dir cur-cart)))
      (define x1 (+ x0 dx))
      (define y1 (+ y0 dy))
      (define t1 (layout-ref tracks x1 y1))
      (unless t1
        (error 'run "~a fell off of the tracks: (~a,~a)"
               (cart-id cur-cart) x1 y1))
      (define-values (dir turn) (compute-turn t1 cur-cart))
      (layout-set! carts x0 y0 #f)
      (when (layout-ref carts x1 y1)
        (error 'occupied "~a ~a is occupied" x1 y1))
      (define new-cart (cart (cart-id cur-cart) dir turn (add1 move)))
      (layout-set! carts x1 y1 new-cart)
      (displayln
        (let ([~a3 (lambda (v) (~a #:width 3 #:align 'right v))])
          (~a "(" (~a3 x0) "," (~a3 y0) ") -> (" (~a3 x1) "," (~a3 y1) ")"
              "   " (~a3 t1) " " new-cart)))))

    (for ([m (in-naturals)] [k 10000]) (run m)))

(module* viz #f
  (define-values (tracks carts)
    (call-with-input-file "inputs/13a.txt" read-track-description))

  (define (cart-dir-char c)
    (match (cart-dir c)
     ['N #\^]
     ['S #\v]
     ['E #\>]
     ['W #\<]))

  (define (track-char t)
    (match t
     [#f  #\space]
     ['SW #\\]
     ['SE #\/]
     ['NS #\|]
     ['WE #\-]
     ['+  #\+]
     ))

  (for ([y (in-range 0 (layout-height tracks))])
    (for ([x (in-range 0 (layout-width tracks))])
      (let ([cart (layout-ref carts x y)])
        (if cart
            (write-char (cart-dir-char cart))
            (write-char (track-char (layout-ref tracks x y))))))
    (write-char #\newline)))

(module* main #f
  (require (submod ".." viz)))
