#lang racket

(define << arithmetic-shift)
(define (>> n m) (<< n (- m)))
(define bit/or bitwise-ior)
(define bit/and bitwise-and)

(define (state->integer s)
  (define s*
    (sequence-map (lambda (c) (match c [#\# 1] [#\. 0])) s))
  (for/fold ([i 0])
            ([n (in-naturals)]
             [t s*])
    (bit/or (<< t n) i)))

(module+ test
  (require rackunit)

  (check-equal? (state->integer "...#..#.#..##......###...###")
                #b1110001110000001100101001000)
  (check-equal? (state->integer "...##") #b11000)
  (check-equal? (state->integer "..#..") #b100)
  (check-equal? (state->integer "####.") #b1111)
  (check-equal? (state->integer "###.#") #b10111))

(define (integer->state i)
  (define (loop cs i)
    (cond
      [(zero? i) (list->string (reverse cs))]
      [(zero? (bit/and i 1)) (loop (cons #\. cs) (>> i 1))]
      [else (loop (cons #\# cs) (>> i 1))]))
  (loop null i))

(module+ test
  (check-equal? (integer->state #b10111) "###.#"))

(define (pot-neighbors i n)
  (cond
    [(> n 1) (bitwise-bit-field i (- n 2) (+ n 3))]
    [else
     (<< (bitwise-bit-field i 0 (+ n 3)) (- 2 n))]))

(module+ test
  (check-equal? (pot-neighbors #b11011 2) #b11011)
  (check-equal? (pot-neighbors #b11011 3) #b01101)
  (check-equal? (pot-neighbors #b101011 1) #b10110)
  (check-equal? (pot-neighbors #b101011 0) #b01100))

(define (in-pot-neighbors i)
  (define (rest-stream n)
    (cond
      [(zero? n) empty-stream]
      [else
        (stream-cons (pot-neighbors i (sub1 n))
                     (rest-stream (sub1 n)))]))
  (rest-stream (+ 2 (integer-length i))))

(module+ test
  (define (check-sequence s es [message ""])
    (check-equal? (sequence->list s) es message))

  (check-sequence (in-pot-neighbors #b1)
                  '(#b00001 #b00010 #b00100))
  (check-sequence (in-pot-neighbors #b110101)
                  '(#b00001 #b00011 #b00110 #b01101
                    #b11010 #b10101 #b01010 #b10100)))

(define ((make-bit-step r*) i)
  (if (for/or ([r (in-list r*)]) (= r i)) 1 0))

(define ((make-step bit-step) i)
  (for/fold ([z 0]) ([n (in-pot-neighbors i)])
    (bit/or (bit-step n) (<< z 1))))

(module+ test
  (require threading)

  (let* ([bit-step (make-bit-step (list (state->integer ".##.#")))]
         [step (make-step bit-step)])
    (check-equal? (step (state->integer ".##.#")) #b00100))

  (let* ([bit-step (make-bit-step (list (state->integer "....#")))]
         [step (make-step bit-step)])
    (check-equal? (step (state->integer "....#"))
                  (state->integer "..#..")))

  (let* ([rules (list "...##" "..#.." ".#..." ".#.#." ".#.##" ".##.." ".####"
                      "#.#.#" "#.###" "##.#." "##.##" "###.." "###.#" "####.")]
         [bit-step (make-bit-step (map state->integer rules))]
         [step (make-step bit-step)])
    (check-equal? (~> "...#..#.#..##......###...###"
                      state->integer
                      step
                      integer->state)
                  "...#...#....#.....#..#..#..#"))

  (test-case "check multiple steps"
    (define rules
      (list "...##" "..#.." ".#..." ".#.#." ".#.##" ".##.." ".####"
            "#.#.#" "#.###" "##.#." "##.##" "###.." "###.#" "####."))
    (define expected
      (list "...#...#....#.....#..#..#..#..........."
            "...##..##...##....#..#..#..##.........."
            "..#.#...#..#.#....#..#..#...#.........."
            "...#.#..#...#.#...#..#..##..##........."
            "....#...##...#.#..#..#...#...#........."
            "....##.#.#....#...#..##..##..##........"
            "...#..###.#...##..#...#...#...#........"
            "...#....##.#.#.#..##..##..##..##......."
            "...##..#..#####....#...#...#...#......."
            "..#.#..#...#.##....##..##..##..##......"
            "...#...##...#.#...#.#...#...#...#......"
            "...##.#.#....#.#...#.#..##..##..##....."
            "..#..###.#....#.#...#....#...#...#....."
            "..#....##.#....#.#..##...##..##..##...."
            "..##..#..#.#....#....#..#.#...#...#...."
            ".#.#..#...#.#...##...#...#.#..##..##..."
            "..#...##...#.#.#.#...##...#....#...#..."
            "..##.#.#....#####.#.#.#...##...##..##.."
            ".#..###.#..#.#.#######.#.#.#..#.#...#.."
            ".#....##....#####...#######....#.#..##."))
    (define bit-step (make-bit-step (map state->integer rules)))
    (define step (make-step bit-step))
    (define i (state->integer "...#..#.#..##......###...###..........."))
    (for/fold ([i i] #:result (void))
              ([e (in-list expected)]
               [n (in-naturals)])
      (let ([i (step i)])
        (check-equal? i (state->integer e)
                      (~a "step " (add1 n) "    "
                          (integer->state i) "    " e))
        i)))
)
