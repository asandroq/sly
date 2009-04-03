
(load "compiler")

(define test1
  '(let ((a (char->integer #\A))
	 (b (* 24 3))
	 (c (add1 (sub1 (add1 1024))))
	 (d 15000000)
	 (e (let ((z (+ 20 20)))
	      (zero? (- (* 10 4) z)))))
     (set! d (+ a d))
     (let ((x (let* ((m (* b (- b a)))
		     (n (* c (- b a))))
		(lambda (f g)
		  (let ((i m)
			(k (lambda (m z)
			     (set! g (+ m 1))
			     (+ (+ m (- n z)) g))))
		    (k f i)))))
           (y (call/cc (lambda (cont)
			 (cons (cont 34) c)))))
       (cond
	((not c)
	 (integer->char c))
	((not e)
	 (x a (cdr y)))
	(else
	 (add1 c)
	 (and c (x (- d a) b)))))))

(define test2
  '(let loop ((a (cons 1 (cons 2 (cons 3 '())))))
     (if (null? a)
	 #t
	 (loop (cdr a)))))

(compile-to-file "test.fasl" test1)

