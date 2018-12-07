#lang racket/base

(require racket/match
         racket/stream)

(require racket/sequence
         threading)

;; #1 @ 1,3: 4x4

(define RECT-PAT
  #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)")

(define FABRIC-SIZE
  (* 1000 1000))

(define (parse-rect s)
  (match (regexp-match RECT-PAT s)
    [(list _ id x y w h)
     (list (string->number id)
           (string->number x)
           (string->number y)
           (string->number w)
           (string->number h))]))

(struct project
  [fabric claims])

(define (make-project)
  (project
   (make-vector FABRIC-SIZE null)
   (let ([v (make-vector 1410 #t)])
     (vector-set! v 0 #f)
     v)))

(define (rect-fill x y w h)
  (for*/stream ([j (in-range y (+ y h))]
                [i (in-range x (+ x w))])
    (cons i j)))

(define (fabric-idx coord)
  (match-define (cons x y) coord)
  (+ x (* y 1000)))

(define (fabric-ref project coord)
  (let ([i (fabric-idx coord)])
    (vector-ref (project-fabric project) i)))

(define (fabric-set! project coord val)
  (let ([i (fabric-idx coord)])
    (vector-set! (project-fabric project) i val)))

(module+ test
  (require rackunit)

  (check-equal? (fabric-idx '(0 . 0)) 0)
  (check-equal? (fabric-idx '(0 . 1)) 1000)
  
  (let ([project (make-project)])
    (check-equal? (fabric-ref project '(0 . 0)) null)
    (check-equal? (fabric-ref project '(100 . 100)) null)
    (fabric-set! project '(100 . 100) 'gray)
    (check-eq? (fabric-ref project '(100 . 100)) 'gray)))

(define (fabric-touch! project coord claim-id)
  (define (update! old)
    (fabric-set! project coord (cons claim-id old)))

  (define (update-claim! c)
    (vector-set! (project-claims project) c #f))
  
  (let ([old-claims (fabric-ref project coord)])
    (match old-claims
      [(list)   (update! old-claims) 0]
      [(list x) (update! old-claims)
                (update-claim! claim-id)
                (update-claim! x)
                1]
      [(list x* ...) (update! old-claims)
                     (update-claim! claim-id)
                     (for ([x (in-list x*)])
                       (update-claim! x))
                     0]
      )))

(module+ test
  (test-case "touch fabric"
             (let* ([project (make-project)]          
                    [v (fabric-touch! project '(0 . 0) 1)])
               (check-equal? v 0)
               (check-equal? (fabric-ref project '(0 . 0))
                             '(1))
               (check-equal? (vector-ref
                              (project-claims project) 1)
                             #t)))
  
  (check-equal?
   (for/sum ([coord (rect-fill 3 2 5 4)])  
     (fabric-touch! (make-project) coord 1))
   0)

  (let ([project (make-project)])
    (check-equal?
     (for*/sum ([n 2]
                [coord (rect-fill 3 2 5 4)])
       (fabric-touch! project coord (add1 n)))
     20))

  (let ([project (make-project)])
    (check-equal?
     (for*/sum ([n 3]
                [coord (rect-fill 3 2 5 4)])
       (fabric-touch! project coord (add1 n)))
     20))
  
  (let ([project (make-project)])
    (check-equal?
     (for/sum ([rect-points (list (rect-fill 1 3 4 4)
                                  (rect-fill 3 1 4 4)
                                  (rect-fill 5 5 2 2))]
               [n 3])
       (for/sum ([coord rect-points])
         (fabric-touch! project coord (add1 n))))
     4)
    (check-equal? (vector-ref (project-claims project) 1) #f)
    (check-equal? (vector-ref (project-claims project) 2) #f)
    (check-equal? (vector-ref (project-claims project) 3) #t)))

(module* main #f
  (define project (make-project))
  
  (displayln "PART ONE")
  (displayln   
   (call-with-input-file "inputs/03.txt"
     (lambda (inp)  
       (for/sum ([line  (in-port read-line inp)])
         (match-define (list id x y w h)
           (parse-rect line))
         (for/sum ([coord (rect-fill x y w h)])
           (fabric-touch! project coord id))))))

  (displayln "PART TWO")
  (displayln
   (for/or ([i (in-naturals)]
           [e (in-vector (project-claims project))])
    (and e i))))
