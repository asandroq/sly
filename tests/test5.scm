
(define (proc a b . c)
  (case (+ a b)
    ((0 alex "test")
     (car c))
    ((ramirez 76 a b c) =>
     (lambda (p)
       (- (car p) 34)))
    (else
     (cdr c))))

(display (proc 34 42 'blah 'bleh))
(newline)
