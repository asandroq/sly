(define (proc . args)
  
  (define (iproc1 a b c)
    (set! b 500)
    (+ a b c))

  (define (iproc2 x . y)
    (if (< x 100)
        (set! x 100)
        (set! (car y) 100))
    (* x (cadr y))
    (cdr y))

  (let ((m (car args))
        (n (cdr args)))
    (begin
      (define (jproc1 . list)
        (iproc1 (or m (car args))
                (or n (car args))
                (and m n)))

      (define (jproc2 blah blih . bleh)
        (if (not (null? bleh))
            (iproc2 n blah)
            (iproc2 n bleh))))

    (jproc1 (jproc2 (car args) (cadr args) (caddr args)))))
