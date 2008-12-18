
(load "compiler")

(define test1
  '(let ((a (char->integer #\A))
         (b (* 24 3))
         (c (add1 (sub1 (add1 1024))))
         (d 20000000)
         (e (let ((z (+ 20 20)))
              (zero? (- (* 10 4) z)))))
     (+ a d)
     ((lambda (f) (* a f)) (- c b))))

(compile-to-file "test.fasl" test1)

