;;;
;;; The Duna Scheme compiler
;;; Copyright (c) 2009 Alex Queiroz <asandroq@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;

;; Transform 'cond' into nested 'ifs'
(define (transform-cond exp)
  (let collect ((code '())
		(clauses (reverse (cdr exp)))
		(last? #t))
    (if (null? clauses)
	(if (null? code)
	    (error "Empty COND")
	    code)
	(let ((clause (car clauses)))
	  (if (pair? clause)
	      (let ((test (transform-exp (car clause)))
		    (body (transform-exp (cdr clause))))
		(if (eqv? test 'else)
		    (if last?
			(collect (cons 'begin body)
				 (cdr clauses)
				 #f)
			(error "ELSE must be last clause in COND"))
		    (collect (list 'if
				   test
				   (cons 'begin body)
				   code)
			     (cdr clauses)
			     #f)))
	      (error "Ill-formed COND clause"))))))

;; Transform 'let' into immediate lambda application
(define (transform-let exp)
  (let ((bindings (cadr exp))
	(body (transform-exp (cddr exp))))
    (let loop ((vars '())
	       (args '())
	       (bindings bindings))
      (if (null? bindings)
	  (cons (cons 'lambda (cons vars body)) args)
	  (let ((binding (car bindings)))
	    (let ((var (car binding))
		  (arg (transform-exp (cadr binding))))
	      (if (symbol? var)
		  (loop (cons var vars)
			(cons arg args)
			(cdr bindings))
		  (error "Ill-formed LET"))))))))

;; Transforms derived syntax into primitive syntax
(define (transform-exp exp)
  (if (pair? exp)
      (let ((op (car exp)))
	(case op
	  ((cond)
	   (transform-cond exp))
	  ((let)
	   (transform-let exp))
	  (else
	   (map transform-exp exp))))
      exp))

;; bytecode instructions
(define *opcodes*
  ;; basic VM instructions
  '((LOAD-NIL       . 1)
    (LOAD-FALSE     . 2)
    (LOAD-TRUE      . 3)
    (LOAD-ZERO      . 4)
    (LOAD-ONE       . 5)
    (LOAD-FIXNUM    . 6)
    (LOAD-CHAR      . 7)
    (PUSH           . 8)
    (POP            . 9)
    (LOAD0          . 10)
    (LOAD1          . 11)
    (LOAD2          . 12)
    (LOAD3          . 13)
    (LOAD           . 14)
    (SET-FP         . 15)
    (SAVE-FP        . 16)
    (REST-FP        . 17)
    (CREATE-CLOSURE . 18)
    (CALL           . 19)
    (RETURN         . 20)
    (SAVE-PROC      . 21)
    (SET-PROC       . 22)
    (JMP-IF         . 23)
    (JMP            . 24)
    (LOAD-FREE      . 25)
    (SAVE-CONT      . 26)
    (REST-CONT      . 27)

    ;; type predicates
    (NULL-P         . 81)
    (BOOL-P         . 82)
    (CHAR-P         . 83)
    (FIXNUM-P       . 84)

    ;; primitives optimised as instructions
    (INC            . 101)
    (DEC            . 102)
    (FIXNUM-TO-CHAR . 103)
    (CHAR-TO-FIXNUM . 104)
    (ZERO-P         . 105)
    (NOT            . 106)
    (PLUS           . 107)
    (MINUS          . 108)
    (MULT           . 109)
    (CONS           . 110)
    (CAR            . 111)
    (CDR            . 112)))

(define (make-compiler-state)
  (vector
   0                          ;; Index of next instruction
   (make-vector 32 0)))       ;; code vector

(define (code-capacity cs)
  (vector-length (vector-ref cs 1)))

(define (code-size cs)
  (vector-ref cs 0))

(define (write-fixnum x)
  (let* ((b4 (quotient  x  16777216))
         (x4 (remainder x  16777216))
         (b3 (quotient  x4 65536))
         (x3 (remainder x4 65536))
         (b2 (quotient  x3 256))
         (b1 (remainder x3 256)))
    (display b1)
    (display " ")
    (display b2)
    (display " ")
    (display b3)
    (display " ")
    (display b4)
    (display " ")))

(define (write-code-vector cs)
  (display "#( ")
  (let ((code (vector-ref cs 1))
	(code-size (code-size cs)))
    (let loop ((i 0))
      (if (= i code-size)
	  (display ")")
	  (let ((instr (vector-ref code i)))
	    (let ((op   (vector-ref instr 0))
		  (arg1 (vector-ref instr 1))
		  (arg2 (vector-ref instr 2)))
	      (display (cdr (assv op *opcodes*)))
	      (display " ")
	      (if arg1 (write-fixnum arg1))
	      (if arg2 (write-fixnum arg2)))
	    (loop (+ i 1)))))))

(define (extend-code-vector! cs)
  (let* ((len (vector-length (vector-ref cs 1)))
         (new-len (round (/ (* 3 len) 2)))
         (new-vec (make-vector new-len 0)))
    (let loop ((i 0))
      (if (= i len)
          (vector-set! cs 1 new-vec)
          (begin
            (vector-set! new-vec i (vector-ref (vector-ref cs 1) i))
            (loop (+ i 1)))))))

(define (add-to-code! cs instr)
  (let ((i (code-size cs)))
    (and (= i (code-capacity cs))
         (extend-code-vector! cs))
    (vector-set! cs 0 (+ i 1))
    (vector-set! (vector-ref cs 1) i instr)))

(define (instr cs op)
  (add-to-code! cs (vector op #f #f)))

(define (instr1 cs op arg)
  (add-to-code! cs (vector op arg #f)))

(define (instr2 cs op arg1 arg2)
  (add-to-code! cs (vector op arg1 arg2)))

;; used by code that need to patch previously emitted
;; instructions
(define (patch-instr! cs i arg)
  (vector-set! (vector-ref (vector-ref cs 1) i) 1 arg))

(define (immediate? x)
  (or (char? x)
      (boolean? x)
      (integer? x)
      (null? x)))

;; emit code for immediate values
(define (emit-immediate cs x)
  (cond
   ((null? x)
    (instr cs 'LOAD-NIL))
   ((boolean? x)
    (if x
        (instr cs 'LOAD-TRUE)
        (instr cs 'LOAD-FALSE)))
   ((char? x)
    (instr1 cs 'LOAD-CHAR (char->integer x)))
   ((integer? x)
    (case x
      ((0)
       (instr cs 'LOAD-ZERO))
      ((1)
       (instr cs 'LOAD-ONE))
      (else
       (instr1 cs 'LOAD-FIXNUM x))))
   (else
    (error "unknown immediate"))))

(define *primitives*
  '((add1 INC 1)
    (sub1 DEC 1)
    (char->integer CHAR-TO-FIXNUM 1)
    (integer->char FIXNUM-TO-CHAR 1)
    (null? NULL-P 1)
    (zero? ZERO-P 1)
    (not NOT 1)
    (boolean? BOOL-P 1)
    (char? CHAR-P 1)
    (integer? FIXNUM-P 1)
    (+ PLUS 2)
    (- MINUS 2)
    (* MULT 2)
    (cons CONS 2)
    (car CAR 1)
    (cdr CDR 1)))

(define (primitive-call? x)
  (and (pair? x)
       (let ((op (car x)))
         (and (assv op *primitives*) #t))))

(define (compile-primitive-call cs x free env)
  (let* ((prim (car x))
         (prim-rec (assv prim *primitives*)))
    (if prim-rec
        (let ((code (cadr prim-rec))
              (arity (caddr prim-rec)))
          (cond
           ((= arity 1)
            (compile-exp cs (cadr x) free env)
            (instr cs code))
           ((= arity 2)
            (compile-exp cs (caddr x) free env)
            (instr cs 'PUSH)
            (compile-exp cs (cadr x) free env)
            (instr cs code))
           (else
            (error "Primitive with unknown arity"))))
        (error "Unknown primitive"))))

(define (index-of item list)
  (let loop ((i 0)
             (list list))
    (if (null? list)
        #f
        (let ((ele (car list)))
          (if (eq? item ele)
              i
              (loop (+ i 1) (cdr list)))))))
 
(define (lookup var free env ret)
  (let ((k (index-of var free)))
    (if k
        (ret #f #f k)
        (let loop ((j 0)
                   (env env))
          (if (null? env)
              (ret #f #f #f)
              (let ((sec (car env)))
                (let ((i (index-of var sec)))
                  (if i
                      (ret i j #f)
                      (loop (+ j 1) (cdr env))))))))))

(define (not-in-env? var env)
  (let ((cont (lambda (i j k)
                (not i))))
    (lookup var '() env cont)))

(define (compile-seq cs exps free env)
  (if (null? exps)
      (instr cs 'LOAD-NIL)
      (for-each (lambda (x)
                  (compile-exp cs x free env))
                exps)))

(define (compile-conditional cs test then else free env)
  (compile-exp cs test free env)
  (let ((i (code-size cs)))
    ;; this will be back-patched later
    (instr1 cs 'JMP-IF 0)
    (compile-exp cs else free env)
    (let ((j (code-size cs)))
      ;; this will be back-patched later
      (instr1 cs 'JMP 0)
      (let ((k (code-size cs)))
        ;; back-patching if jump
        (patch-instr! cs i (- k i 1))
        (compile-exp cs then free env)
        (let ((m (code-size cs)))
          ;; back-patching else jmp
          (patch-instr! cs j (- m j 1)))))))

;; Produces the union of two sets
(define (set-union set1 set2)
  (let loop ((set1 set1)
	     (set2 set2))
    (if (null? set2)
	set1
	(let ((item (car set2)))
	  (if (memq item set1)
	      (loop set1 (cdr set2))
	      (loop (cons item set1) (cdr set2)))))))

;; Produces the difference of two sets
(define (set-minus set1 set2)
  (let loop ((set '())
	     (set1 set1)
	     (set2 set2))
    (if (null? set1)
	set
	(let ((item (car set1)))
	  (if (memq item set2)
	      (loop set (cdr set1) set2)
	      (loop (cons item set) (cdr set1) set2))))))

;; Collect free variables in expression
;; if wannabe free var is not in env, it's actually a global
(define (collect-free exp bound env)
  (if (pair? exp)
      ;; unfortunately I need to re-enumerate
      ;; all forms accepted by the compiler
      (case (car exp)
	((begin)
	 (collect-all-free (cdr exp) bound env))
	((if)
	 (set-union (collect-free (cadr exp) bound env)
		    (set-union (collect-free (caddr exp) bound env)
			       (collect-free (cadddr exp) bound env))))
	((lambda)
	 (let ((vars (cadr exp))
	       (body (cddr exp)))
	   (collect-all-free body
			     (set-union bound vars) env)))
	((call/cc)
	 (collect-free (cadr exp) bound env))
	(else
	 (if (primitive-call? exp)
	     (collect-all-free (cdr exp) bound env)
	     (collect-all-free exp bound env))))
      (if (symbol? exp)
	  (if (or (memq exp bound)
                  (not-in-env? exp env))
	      '()
	      (list exp))
	  '())))

;; Collect free variables in lambda body
(define (collect-all-free body bound env)
  (let collect ((vars '())
		(body body))
    (if (null? body)
	vars
	(let ((exp (car body)))
	  (collect (set-union vars
			      (collect-free exp bound env))
		   (cdr body))))))

;; Compiles a lambda into code that create closures
;; The jumps are offset by one because when the JMP
;; instruction is executed, the PC is already pointing to
;; the next instruction
(define (compile-closure cs vars body free env)
  (let ((new-env (cons (reverse vars) env))
        (new-free (collect-all-free body vars env)))
    (let loop ((loop-free new-free))
      (if (null? loop-free)
	  (let ((len (length new-free)))
	    (instr1 cs 'CREATE-CLOSURE len)
	    (let ((i (code-size cs)))
	      ;; this will be back-patched later
	      (instr1 cs 'JMP 0)
	      (compile-seq cs body new-free new-env)
	      (instr cs 'POP)
	      (instr cs 'REST-FP)
	      (instr cs 'RETURN)
	      (let ((j (code-size cs)))
		;; back patching jump over closure code
		(patch-instr! cs i (- j i 1)))))
	  (let ((var (car loop-free)))
	    (compile-exp cs var free env)
	    (instr cs 'PUSH)
	    (loop (cdr loop-free)))))))

(define (compile-closed-application cs vars args body free env)
  (instr cs 'SAVE-FP)
  (let loop ((new-env '())
             (vars vars)
             (args args))
    (if (null? vars)
        (let ((len (length new-env)))
          (instr cs 'SET-FP)
          (emit-immediate cs len)
          (instr cs 'PUSH)
          (compile-seq cs body free (cons new-env env))
          (instr cs 'POP)
          (instr cs 'REST-FP))
        (let ((var (car vars))
              (exp (car args)))
          (compile-exp cs exp free env)
          (instr cs 'PUSH)
          (loop (cons var new-env) (cdr vars) (cdr args))))))

(define (compile-application cs proc args free env)
  (instr cs 'SAVE-PROC)
  (let ((i (code-size cs)))
    ;; this is the return address, will be back-patched later
    (instr1 cs 'LOAD-FIXNUM 0)
    (instr cs 'PUSH)
    (instr cs 'SAVE-FP)
    (let ((len (length args)))
      (let loop ((args args))
        (if (null? args)
            (begin
              (compile-exp cs proc free env)
              (instr cs 'SET-PROC)
              (instr cs 'SET-FP)
              (emit-immediate cs len)
              (instr cs 'PUSH)
              (instr cs 'CALL)
              ;; back-patching return address
	      ;; this not a position-independent value
              (patch-instr! cs i (code-size cs)))
            (let ((arg (car args)))
              (compile-exp cs arg free env)
              (instr cs 'PUSH)
              (loop (cdr args))))))))

;; creates a closure that will restore a saved
;; continuation if called - the continuation
;; is stored in the closure as a free variable
(define (create-continuation-closure cs)
  (instr1 cs 'CREATE-CLOSURE 1)
  ;; jump over closure code
  (instr1 cs 'JMP 6)
  ;; value given to the continuation
  (instr cs 'LOAD0)
  (instr cs 'PUSH)
  (instr1 cs 'LOAD-FREE 0)
  (instr cs 'REST-CONT)
  ;; there is no need to pop args, there are none
  ;; but we must restore the frame pointer and the
  ;; return address saved in call/cc
  (instr cs 'REST-FP)
  (instr cs 'RETURN))

(define (compile-call/cc cs exp free env)
  (instr cs 'SAVE-PROC)
  (let ((i (code-size cs)))
    ;; this is the return address, will be back-patched later
    (instr1 cs 'LOAD-FIXNUM 0)
    (instr cs 'PUSH)
    (instr cs 'SAVE-FP)
    (instr cs 'SAVE-CONT)
    (instr cs 'PUSH)
    (create-continuation-closure cs)
    (instr cs 'PUSH)    
    (compile-exp cs exp free env)
    (instr cs 'SET-PROC)
    (instr cs 'SET-FP)
    (instr cs 'LOAD-ONE)
    (instr cs 'PUSH)
    (instr cs 'CALL)
    ;; back-patching return address
    (patch-instr! cs i (code-size cs))))

(define (compile-exp cs x free env)
  (cond
   ((immediate? x)
    (emit-immediate cs x))
   ((symbol? x)
    (let ((cont (lambda (i j k)
                  (if i
                      (if (zero? j)
                          (case i
                            ((0)
                             (instr cs 'LOAD0))
                            ((1)
                             (instr cs 'LOAD1))
                            ((2)
                             (instr cs 'LOAD2))
                            ((3)
                             (instr cs 'LOAD3))
                            (else
                             (instr2 cs 'LOAD i 0)))
			  (instr2 cs 'LOAD i j))
                      (if k
			  (instr1 cs 'LOAD-FREE k)
                          (error "Unknown binding!"))))))
      (lookup x free env cont)))
   ((primitive-call? x)
    (compile-primitive-call cs x free env))
   ((pair? x)
    (case (car x)
      ((begin)
       (compile-seq cs (cdr x) free env))
      ((if)
       (compile-conditional cs (cadr x) (caddr x) (cadddr x) free env))
      ((lambda)
       (compile-closure cs (cadr x) (cddr x) free env))
      ((call/cc)
       (compile-call/cc cs (cadr x) free env))
      (else
       (let ((op (car x)))
         (if (and (pair? op)
                  (eq? (car op) 'lambda))
             (compile-closed-application cs
					 (cadr op)
					 (cdr x)
					 (cddr op)
                                         free
					 env)
             (compile-application cs op (cdr x) free env))))))
   (else
    (error "Cannot compile atom"))))

(define (compile cs* x*)
  (let ((cs (or cs* (make-compiler-state)))
	(x (transform-exp x*)))
    (compile-exp cs x '() '())
    (write-code-vector cs)))

(define (compile-to-file file x)
  (with-output-to-file file
    (lambda ()
      (compile #f x))))

