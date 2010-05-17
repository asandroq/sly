
(define (proc a b . c)
  (case (+ a b)
    ((0 alex "test")
     (car c))
    ((ramirez 76 a b c)
     (- 76 34))
    (else
     (cdr c))))

(display (proc 34 42 'blah 'bleh))
(newline)
