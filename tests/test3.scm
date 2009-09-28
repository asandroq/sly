
(define (fact n)
      (define blah 1)
      (define (fact-iter n a)
        (if (= n blah)
            a
            (fact-iter (- n 1) (* a n))))
      (fact-iter n 1))

(display (fact 12))
(newline)
