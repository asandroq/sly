
(load "compiler")

(define test1
  '(let ((a (char->integer #\A))
         (b (* 24 3))
         (c (add1 (sub1 (add1 1024))))
         (d 15000000)
         (e (let ((z (+ 20 20)))
              (zero? (- (* 10 4) z)))))
     (+ a d)
     (let ((x (let ((m (* b (- b a)))
		    (n (* c (- b a))))
		(lambda (f g)
		  (+ (+ m (- n f)) g))))
           (y (cons 34 c)))
       (cond
	((not c)
	 (add1 c)
	 (integer->char c))
	(e
	 (x a (cdr y)))
	(else
	 (x (- d a) b))))))

(define test2
  '(let ((a (* 23 67))
	 (b (sub1 1)))
     (cond
      ((char? a)
       (+ a b))
      ((zero? b)
       (lambda (x y)
	 (+ a (* x y))))
      (else
       (integer->char a)))))

(define free1
  '(lambda (x y)
     (+ a (* x y))))

(define free2
  '(lambda (x y)
     (if x
         (lambda (m n)
           (- x (+ y m)))
         (lambda (m o)
           (* x (- m y))))))

(compile-to-file "test.fasl" test1)

