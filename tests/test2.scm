
(define my-name "Alex")

(define (proc2 n)
  (if (equal? my-name n)
      'equal
      'different))

(display (proc2 "Ramirez"))
(newline)

(display (proc2 "Alex"))
(newline)

