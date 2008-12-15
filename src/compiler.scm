
;; bytecode instructions
(define *opcodes*
  '((LOAD-NIL    . 1)
    (LOAD-FALSE  . 2)
    (LOAD-TRUE   . 3)
    (LOAD-ZERO   . 4)
    (LOAD-ONE    . 5)
    (LOAD-FIXNUM . 6)
    (LOAD-CHAR   . 7)
    (INC         . 8)
    (DEC         . 9)))

(define (instr i . args)
  (display " ")
  (display (cdr (assv i *opcodes*)))
  (or (null? args)
      (for-each (lambda (x)
                  (display " ")
                  (display x))
                args)))

(define (immediate? x)
  (or (char? x)
      (boolean? x)
      (integer? x)
      (null? x)))

;; emit code for immediate values
(define (emit-immediate x)
  (cond
   ((null? x)
    (instr 'LOAD-NIL))
   ((boolean? x)
    (if x
        (instr 'LOAD-TRUE)
        (instr 'LOAD-FALSE)))
   ((char? x)
    (instr 'LOAD-CHAR (char->integer x)))
   ((integer? x)
    (let* ((b4 (quotient  x  16777216))
           (x4 (remainder x  16777216))
           (b3 (quotient  x4 65536))
           (x3 (remainder x4 65536))
           (b2 (quotient  x3 256))
           (b1 (remainder x3 256)))
      (instr 'LOAD-FIXNUM b1 b2 b3 b4)))
   (else
    (error "unknon immediate"))))

(define primitive-call?
  (let ((primitives '(add1 sub1)))
    (lambda (x)
      (and (pair? x)
           (memq (car x) primitives)))))

(define (compile-exp x)
  (cond
   ((immediate? x)
    (emit-immediate x))
   ((primitive-call? x)
    (case (car x)
      ((add1)
       (compile-exp (cadr x))
       (instr 'INC))
      ((sub1)
       (compile-exp (cadr x))
       (instr 'DEC))))
   (else
    (error "this expression is not yet supported"))))

(define (compile x)
  (display "(")
  (compile-exp x)
  (display " )")
  (newline))

(define (compile-to-file file x)
  (with-output-to-file file
    (lambda ()
      (compile x))))

