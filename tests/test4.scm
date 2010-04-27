
(define ugly-data '(0 (a . 34) #(1 2 "house" #\y casa #\z 3 4) "hah"))

(display (let ((begin memq))
           (cond
            ((null? ugly-data)
             (cons 0 ugly-data))
            ((or (begin 'blob ugly-data)
                 (begin 'blig ugly-data)))
            ((memq 0 ugly-data) =>
             (lambda (val) (+ (car val) 42)))
            ((pair? ugly-data)
             (car ugly-data))
            (else
             'not-found!))))
(newline)
