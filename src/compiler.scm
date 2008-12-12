
;; bytecode instructions
(define *opcodes*
  '((LOAD-NIL    . 1)
    (LOAD-FALSE  . 2)
    (LOAD-TRUE   . 3)
    (LOAD-ZERO   . 4)
    (LOAD-ONE    . 5)
    (LOAD-FIXNUM . 6)))

(define (instr i)
  (display (cdr (assv i *opcodes*))))

(define (immediate? x)
  (or (char? x)
      (boolean? x)
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
   (else
    (error "unknon immediate"))))

(define (compile-exp x)
  (cond
   ((immediate? x)
    (emit-immediate x))
   (else
    (error "this expression is not yet supported"))))

(define (compile x)
  (display "(")
  (compile-exp x)
  (display ")")
  (newline))

