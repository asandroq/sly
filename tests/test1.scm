(define (proc1 a b c)
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

(display (proc1 (char->integer #\A) (* 24 3) (add1 (sub1 (add1 1024)))))
(newline)
