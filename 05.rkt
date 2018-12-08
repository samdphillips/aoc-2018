#lang racket/base

(require racket/match)

(define (unit-polarity c)
  (if (char-lower-case? c) '- '+))

(define (unit-polarity-opposed? a b)
  (match* ((unit-polarity a) (unit-polarity b))
    [('+ '-) #t]
    [('- '+) #t]
    [(_ _)   #f]))

(module+ test
  (require rackunit)

  (check-true (unit-polarity-opposed? #\a #\A))
  (check-true (unit-polarity-opposed? #\a #\B))
  (check-true (unit-polarity-opposed? #\A #\b))
  (check-false (unit-polarity-opposed? #\A #\B)))

(define (unit-type c)
  (char-upcase c))

(define (unit-same-type? a b)
  (char=? (unit-type a) (unit-type b)))

(module+ test
  (check-true (unit-same-type? #\A #\A))
  (check-true (unit-same-type? #\A #\a))
  (check-false (unit-same-type? #\B #\A))
  (check-false (unit-same-type? #\B #\a)))

(define (react ac us)
  (match us
    [(list) (reverse ac)]
    [(list u1 u2 us ...)
     (=> resume)
     (if (and (unit-same-type? u1 u2)
              (unit-polarity-opposed? u1 u2))
         (match ac
           [(list a b ...) (react b (cons a us))]
           [_              (react ac us)])
         (resume))]
    [(list u us ...)
     (react (cons u ac) us)]))

(module+ test
  (require rackunit)

  (define (react-s s)
    (list->string
     (react null (string->list s))))

  (check-equal? (react-s "aA") "")
  (check-equal? (react-s "ab") "ab")
  (check-equal? (react-s "aB") "aB")
  (check-equal? (react-s "AaBb") "")
  (check-equal? (react-s "bAaB") "")
  (check-equal? (react-s "bAaBb") "b")
  (check-equal? (react-s "dabAcCaCBAcCcaDA")
                "dabCBAcaDA"))

(define (remove-unit c us)
  (filter (lambda (u) (not (unit-same-type? c u))) us))

(module+ test

  (define (react2-s s c)
    (list->string (react null (remove-unit c (string->list s)))))

  (check-equal? (react2-s "dabAcCaCBAcCcaDA" #\a) "dbCBcD")
  (check-equal? (react2-s "dabAcCaCBAcCcaDA" #\b) "daCAcaDA")
  (check-equal? (react2-s "dabAcCaCBAcCcaDA" #\c) "daDA")
  (check-equal? (react2-s "dabAcCaCBAcCcaDA" #\d) "abCBAc"))

(module* main #f
  (require racket/format
           racket/port
           racket/sequence)

  (define (read-alpha-char input-port)
    (let ([c (read-char input-port)])
      (cond
        [(eof-object? c) c]
        [(char-alphabetic? c) c]
        [else (read-alpha-char input-port)])))

  (displayln "PART ONE")
  (displayln
   (length
    (time
     (react null
            (call-with-input-file "inputs/05.txt"
              (lambda (inp)
                (port->list read-alpha-char inp)))))))

  (define (in-alpha-chars)
    (sequence-map (lambda (i) (integer->char (+ i 97)))
                  (in-range 26)))

  (displayln "PART TWO")
  (let ([us (call-with-input-file "inputs/05.txt"
              (lambda (inp)
                (port->list read-alpha-char inp)))])
    (time
     (for ([c (in-alpha-chars)])
       (displayln
        (~a c "  " (length
                    (react null (remove-unit c us)))))))))
