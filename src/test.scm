
(load "compiler")

(define test1
  '((define (proc1 a b c)
      (let ((d 15000000)
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
            (and c (x (- d a) b)))))))
    (proc1 (char->integer #\A) (* 24 3) (add1 (sub1 (add1 1024))))))

(define test2
  '((define my-name "Alex")
    (define (proc2 n)
      (if (equal? my-name n)
          'equal
          'different))
    (proc2 "Ramirez")
    (proc2 "Alex")))

(define test3
  '((define (fact n)
      (define blah 1)
      (define (fact-iter n a)
        (if (= n blah)
            a
            (fact-iter (- n 1) (* a n))))
      (fact-iter n 1))
    (fact 12)))

(define test4
  '((define ugly-data '(0 (a . 34) #(1 2 "house" #\y casa #\z 3 4) "hah"))
    (cond
     ((null? ugly-data)
      (cons 0 ugly-data))
     ((memq 0 ugly-data) =>
      (lambda (val) (+ (car val) 42)))
     ((pair? ugly-data)
      (car ugly-data))
     (else
      'not-found!))))

(compile-to-file "test.fasl" test1)

