
(load "compiler")

(define test1
  '((let ((a (char->integer #\A))
          (b (* 24 3))
          (c (add1 (sub1 (add1 1024))))
          (d 15000000)
          (e (let ((z (+ 20 20)))
               (zero? (- (* 10 4) z)))))
      (set! d (+ a d))
      (let ((x (let* ((m (* b (- b a)))
                      (n (* c (- b a))))
                 (lambda (a b)
                   (let ((i m)
                         (k (lambda (m z)
                              (set! b (+ m 1))
                              (+ (+ m (- n z)) b))))
                     (k a i)))))
            (y (call/cc (lambda (cont)
                          (cons (cont 34) c)))))
        (cond
         ((not c)
          (integer->char c))
         ((not e)
          (x a (cdr y)))
         (else
          (add1 c)
          (- c b)
          (and c (x (- d a) b))))))))

(define test2
  '(let loop ((a (cons 1 (cons 2 (cons 3 '())))))
     (if (null? a)
	 #t
	 (loop (cdr a)))))

(define test3
  '((define (fact n)
      (define blah 1)
      (define (fact-iter n a)
        (if (= n blah)
            a
            (fact-iter (- n 1) (* a n))))
      (fact-iter n 1))
    (fact 10)))

(compile-to-file "test.fasl" test1)

