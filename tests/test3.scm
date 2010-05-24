
(define (fact n)
      (define blah 1)
      (define bleh (+ 123 (car '(3 . 4))))
      (define blih (- 456 (cdr '(6 . 7))))
      (define bloh (call/cc (lambda (k) (cons k 45))))
      (define blooh (not blah))

      (define (bluh a b)
        (define (foo x)
          (+ a x))
        (define (bar y)
          (* b y))
        (/ (foo 1000) (bar 10)))

      (define bluah (letrec ((even? (lambda (n)
                                      (if (zero? n)
                                          #t
                                          (odd? (- n 1)))))
                             (odd? (lambda (n)
                                     (if (zero? n)
                                         #f
                                         (even? (- n 1)))))
                             (tested 15678)
                             (lame (begin (set! blih 0) (+ (cdr bloh) 10))))
                      (even? tested)))

      (define (fact-iter n a)
        (set! blih (bluh bluah a))
        (if (and (= n blah)
                 (or bloh blooh))
            a
            (fact-iter (- n 1) (* a n))))
      (fact-iter n 1))

(display (fact 12))
(newline)
