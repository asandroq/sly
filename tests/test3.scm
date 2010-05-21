
(define (fact n)
      (define blah 1)
      (define bleh (+ 123 (car '(3 . 4))))
      (define blih (- 456 (cdr '(6 . 7))))
      (define bloh (call/cc (lambda (k) (cons k 45))))
      (define blooh (not blah))
      (define (fact-iter n a)
        (set! blih a)
        (if (and (= n blah)
                 (or bloh blooh))
            a
            (fact-iter (- n 1) (* a n))))
      (fact-iter n 1))

(display (fact 12))
(newline)
