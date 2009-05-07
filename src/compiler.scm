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

;;
;; Utilities
;;

;; Produces the union of two sets
(define (set-union set1 set2)
  (let loop ((set1 set1)
	     (set2 set2))
    (if (null? set2)
	set1
	(let ((item (car set2)))
	  (if (member item set1)
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

;; Produces the intersection of two sets
(define (set-intersection set1 set2)
  (let loop ((set '())
	     (set1 set1)
	     (set2 set2))
    (if (null? set1)
	set
	(let ((item (car set1)))
	  (if (memq item set2)
	      (loop (cons item set) (cdr set1) set2)
	      (loop set (cdr set1) set2))))))

;; Gives the numerical index of an item in a list
(define (index-of item list)
  (let loop ((i 0)
             (list list))
    (if (null? list)
        #f
        (let ((ele (car list)))
          (if (eq? item ele)
              i
              (loop (+ i 1) (cdr list)))))))

;; left folds

(define (foldl f s l)
  (let loop ((s s)
	     (l l))
    (if (null? l)
	s
	(loop (f s (car l)) (cdr l)))))

(define (foldl* f l)
  (if (null? l)
      #f
      (let ((s (car l)))
	(foldl f s (cdr l)))))

;;
;; Preprocessing
;;

;; Transform 'and' into series of 'ifs'
(define (transform-and exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#t
	(let ((test (car exps))
	      (rest (cdr exps)))
	  (if (null? rest)
	      (let ((var (gensym)))
		(transform-exp
		 (list 'let (list (list var test))
		       (list 'if var var '#f))))
	      (list 'if test (transform-exp (cons 'and rest)) '#f))))))

(define (transform-or exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#f
	(let ((test (car exps))
	      (rest (cdr exps)))
	  (if (null? rest)
	      test
	      (let ((var (gensym)))
		(list 'let (list (list var test))
		      (list 'if var var (transform-exp (cons 'or rest))))))))))

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

;; Transform let* into cascade of 'lets'
(define (transform-let* exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (if (null? bindings)
	(transform-exp (cons 'begin body))
	(let ((first (car bindings))
	      (rest (cdr bindings)))
	  (if (null? rest)
	      (transform-exp (cons 'let
				   (cons bindings body)))
	      (transform-exp (list 'let
				   (list first)
				   (cons 'let* (cons rest body)))))))))

;; Transform 'letrec' into 'lambda' plus assignments
(define (transform-letrec exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (if (null? bindings)
	(cons 'begin body)
	(let loop ((vars '())
		   (mocks '())
		   (body (transform-exp body))
		   (bindings bindings))
	  (if (null? bindings)
	      (cons (cons 'lambda (cons vars body)) mocks)
	      (let ((binding (car bindings)))
		(let ((var (car binding))
		      (exp (transform-exp (cadr binding))))
		  (loop (cons var vars)
			(cons '#f mocks)
			(cons (list 'set! var exp) body)
			(cdr bindings)))))))))

;; Transform named 'let' into 'letrec'
(define (transform-named-let exp)
  (let ((name (cadr exp))
	(bindings (caddr exp))
	(body (cdddr exp)))
    (let loop ((vars '())
	       (exps '())
	       (bindings bindings))
      (if (null? bindings)
	  (transform-exp
	   (list 'letrec (list (list name
				     (cons 'lambda
					   (cons vars body))))
		 (cons name exps)))
	  (let ((binding (car bindings)))
	    (let ((var (car binding))
		  (exp (cadr binding)))
	      (if (symbol? var)
		  (loop (cons var vars)
			(cons exp exps)
			(cdr bindings))
		  (error "Ill-formed named LET"))))))))

;; Transforms derived syntax into primitive syntax
(define (transform-exp exp)
  (if (pair? exp)
      (let ((op (car exp)))
	(case op
	  ((and)
	   (transform-and exp))
	  ((or)
	   (transform-or exp))
	  ((cond)
	   (transform-cond exp))
	  ((let)
	   (if (symbol? (cadr exp))
	       (transform-named-let exp)
	       (transform-let exp)))
	  ((let*)
	   (transform-let* exp))
	  ((letrec)
	   (transform-letrec exp))
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
    (LOAD0          . 9)
    (LOAD1          . 10)
    (LOAD2          . 11)
    (LOAD3          . 12)
    (LOAD           . 13)
    (MAKE-CLOSURE   . 14)
    (CALL           . 15)
    (RETURN         . 16)
    (JMP-IF         . 17)
    (JMP            . 18)
    (LOAD-FREE      . 19)
    (SAVE-CONT      . 20)
    (REST-CONT      . 21)
    (ASSIGN         . 22)
    (ASSIGN-FREE    . 23)
    (BOX            . 24)
    (OPEN-BOX       . 25)
    (FRAME          . 26)
    (TAIL-CALL      . 27)
    (HALT           . 28)
    (LOAD-LOCAL     . 29)
    (INSERT-BOX     . 30)
    (ASSIGN-LOCAL   . 31)
    (POP            . 32)

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
		  (arg1 (vector-ref instr 1)))
	      (display (cdr (assv op *opcodes*)))
	      (display " ")
	      (if arg1 (write-fixnum arg1)))
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
  (add-to-code! cs (vector op #f)))

(define (instr1 cs op arg)
  (add-to-code! cs (vector op arg)))

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

(define (lookup var free locals env ret)
  (let ((k (index-of var free)))
    (if k
        (ret 'free k)
	;; the index of the last var of locals
	;; that matches must be the one returned
	(let ((len (length locals))
	      (locals-rev (reverse locals)))
	  (let ((j (index-of var locals-rev)))
	  (if j
	      (ret 'local (- len j 1))
	      (if (null? env)
		  (ret #f #f)
		  (let ((i (index-of var (car env))))
		    (if i
			(ret 'bound i)
			(ret #f #f))))))))))

(define (bound? var locals env)
  (or (index-of var locals)
      (let loop ((b env))
	(if (null? b)
	    #f
	    (let ((sec (car b)))
	      (if (index-of var sec)
		  #t
		  (loop (cdr b))))))))

;; if wannabe free var is nowhere in env
;; it's actually a global, if it is in the
;; *first* section of env, it is bound
;; otherwise it is free
(define (free? var bound env)
  (and (not (index-of var bound))
       (if (null? env)
	   #f
	   (let loop ((e (cdr env)))
	     (if (null? e)
		 #f
		 (let ((sec (car e)))
		   (if (index-of var sec)
		       #t
		       (loop (cdr e)))))))))

(define (primitive-call? x locals env)
  (and (pair? x)
       (let ((op (car x)))
         (and (not (bound? op locals env))
	      (assv op *primitives*) #t))))

(define (compile-primitive-call cs x sets free locals env)
  (let* ((prim (car x))
         (prim-rec (assv prim *primitives*)))
    (if prim-rec
        (let ((code (cadr prim-rec))
              (arity (caddr prim-rec)))
          (cond
           ((= arity 1)
            (compile-exp cs (cadr x) sets free locals env #f)
            (instr cs code))
           ((= arity 2)
            (compile-exp cs (caddr x) sets free locals env #f)
            (instr cs 'PUSH)
            (compile-exp cs (cadr x) sets free locals env #f)
            (instr cs code))
           (else
            (error "Primitive with unknown arity"))))
        (error "Unknown primitive"))))

(define (compile-seq cs exps sets free locals env tail?)
  (if (null? exps)
      (error "Empty BEGIN")
      (let loop ((x (car exps))
		 (exps (cdr exps)))
	(if (null? exps)
	    (compile-exp cs x sets free locals env tail?)
	    (begin
	      (compile-exp cs x sets free locals env #f)
	      (loop (car exps) (cdr exps)))))))
	
(define (compile-conditional cs test then else sets free locals env tail?)
  (compile-exp cs test sets free locals env #f)
  (let ((i (code-size cs)))
    ;; this will be back-patched later
    (instr1 cs 'JMP-IF 0)
    (compile-exp cs else sets free locals env tail?)
    (let ((j (code-size cs)))
      ;; this will be back-patched later
      (instr1 cs 'JMP 0)
      (let ((k (code-size cs)))
        ;; back-patching if jump
        (patch-instr! cs i (- k i 1))
        (compile-exp cs then sets free locals env tail?)
        (let ((m (code-size cs)))
          ;; back-patching else jmp
          (patch-instr! cs j (- m j 1)))))))

;; Collect free variables in expression
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
	   (collect-all-free body (set-union bound vars) env)))
	((call/cc)
	 (collect-free (cadr exp) bound env))
	((set!)
	 (let ((var (cadr exp))
	       (exp (caddr exp)))
	   (set-union (collect-free exp bound env)
		      (if (free? var bound env) (list var) '()))))
	(else
	 (if (primitive-call? exp bound env)
	     (collect-free (cdr exp) bound env)
	     (collect-all-free exp bound env))))
      (if (symbol? exp)
	  (if (free? exp bound env)
	      (list exp)
	      '())
	  '())))

;; Collect assigned variables in expression
(define (collect-sets exp bound env)
  (if (pair? exp)
      ;; unfortunately I need to re-enumerate
      ;; all forms accepted by the compiler
      (case (car exp)
	((begin)
	 (collect-all-sets (cdr exp) bound env))
	((if)
	 (set-union (collect-sets (cadr exp) bound env)
		    (set-union (collect-sets (caddr exp) bound env)
			       (collect-sets (cadddr exp) bound env))))
	((lambda)
	 (let ((vars (cadr exp))
	       (body (cddr exp)))
	   (collect-all-sets body (set-minus bound vars) env)))
	((call/cc)
	 (collect-sets (cadr exp) bound env))
	((set!)
	 (let ((var (cadr exp))
	       (exp (caddr exp)))
	   (set-union (collect-sets exp bound env)
		      (if (memq var bound) (list var) '()))))
	(else
	 (collect-all-sets exp bound env)))
      '()))

(define (create-collect-all f)
  (lambda (exps bound env)
    (foldl* set-union (map (lambda (x)
			     (f x bound env))
			   exps))))

(define collect-all-free
  (create-collect-all collect-free))

(define collect-all-sets
  (create-collect-all collect-sets))

;; Create instructions to box arguments to closure
;; that are assigned somewhere in the code
(define (make-boxes cs vars sets)
  (for-each (lambda (v)
	      (and (memq v sets)
		   (instr1 cs 'INSERT-BOX (index-of v vars))))
	    vars))

(define (compile-binding cs var free locals env)
  (let ((cont (lambda (t i)
		(if t
		    (case t
		      ((free)
		       (instr1 cs 'LOAD-FREE i))
		      ((local)
		       (instr1 cs 'LOAD-LOCAL i))
		      ((bound)
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
			  (instr1 cs 'LOAD i))))
		      (else
		       (error "Unknown binding type!")))
		    (error "Unknown binding!")))))
    (lookup var free locals env cont)))

;; Compiles a lambda into code that create closures
;; The jumps are offset by one because when the JMP
;; instruction is executed, the PC is already pointing to
;; the next instruction
(define (compile-closure cs vars body sets free locals env)
  (let ((new-env (cons (reverse vars)
		       (cons locals env))))
    (let ((new-free (collect-all-free body vars new-env))
	  (new-sets (collect-all-sets body vars new-env)))
      (let loop ((loop-free new-free))
	(if (null? loop-free)
	    (let ((len (length new-free)))
	      (instr1 cs 'MAKE-CLOSURE len)
	      (let ((i (code-size cs)))
		;; this will be back-patched later
		(instr1 cs 'JMP 0)
		(make-boxes cs (reverse vars) new-sets)
		(compile-seq cs
			     body
			     (set-union new-sets
					(set-intersection sets new-free))
			     (reverse new-free)
			     '()
			     new-env
			     #t)
		(instr cs 'RETURN)
		(let ((j (code-size cs)))
		  ;; back patching jump over closure code
		  (patch-instr! cs i (- j i 1)))))
	    (let ((var (car loop-free)))
	      (compile-binding cs var free locals env)
	      (instr cs 'PUSH)
	      (loop (cdr loop-free))))))))

(define (compile-closed-application cs vars args body sets free locals env tail?)
  (let ((new-sets (collect-all-sets body vars env)))
    (let loop ((new-vars vars)
	       (args args))
      (if (null? new-vars)
	  (begin
	    (compile-seq
	     cs
	     body
	     (set-union new-sets sets)
	     free
	     (append locals vars)
	     env
	     tail?)
	    (or tail? (instr1 cs 'POP (length vars))))
	  (let ((var (car new-vars))
		(exp (car args)))
	    (compile-exp cs exp sets free locals env #f)
	    (and (memq var new-sets)
		 (instr cs 'BOX))
	    (instr cs 'PUSH)
	    (loop (cdr new-vars)
		  (cdr args)))))))

(define (compile-application cs proc args sets free locals env tail?)
  (let ((i (code-size cs)))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (let ((len (length args)))
      (let loop ((args args))
        (if (null? args)
            (begin
              (emit-immediate cs len)
	      (instr cs 'PUSH)
              (compile-exp cs proc sets free locals env #f)
              (instr cs (if tail? 'TAIL-CALL 'CALL))
              ;; back-patching return address
	      ;; this not a position-independent value
              (if (not tail?)
		  (patch-instr! cs i (code-size cs))))
            (let ((arg (car args)))
              (compile-exp cs arg sets free locals env #f)
              (instr cs 'PUSH)
              (loop (cdr args))))))))

;; creates a closure that will restore a saved
;; continuation if called - the continuation
;; is stored in the closure as a free variable
(define (create-continuation-closure cs)
  (instr1 cs 'MAKE-CLOSURE 1)
  ;; jump over closure code
  (instr1 cs 'JMP 5)
  ;; value given to the continuation
  (instr cs 'LOAD0)
  (instr cs 'PUSH)
  (instr1 cs 'LOAD-FREE 0)
  (instr cs 'REST-CONT)
  (instr cs 'RETURN))

(define (compile-call/cc cs exp sets free locals env tail?)
  (let ((i (code-size cs)))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (instr cs 'LOAD-ZERO)
    (instr cs 'PUSH)
    (instr cs 'SAVE-CONT)
    (instr cs 'PUSH)
    (create-continuation-closure cs)
    (instr cs 'PUSH)
    ;; calling closure given to call/cc with
    ;; continuation-restoring closure as sole argument
    (instr cs 'LOAD-ONE)
    (instr cs 'PUSH)
    (compile-exp cs exp sets free locals env #f)
    (instr cs (if tail? 'TAIL-CALL 'CALL))
    ;; back-patching return address
    (or tail? (patch-instr! cs i (code-size cs)))))

(define (compile-assignment cs var exp sets free locals env)
  (let ((cont (lambda (t i)
		(if t
		    (case t
		      ((free)
		       (instr1 cs 'ASSIGN-FREE i))
		      ((local)
		       (instr1 cs 'ASSIGN-LOCAL i))
		      ((bound)
		       (instr1 cs 'ASSIGN i))
		      (else
		       (error "Unknown binding type")))
		    (error "Unknown binding")))))
    (compile-exp cs exp sets free locals env #f)
    (lookup var free locals env cont)))

(define (compile-reference cs var sets free locals env)
  (compile-binding cs var free locals env)
  (and (memq var sets)
       (instr cs 'OPEN-BOX)))

(define (compile-exp cs x sets free locals env tail?)
  (cond
   ((immediate? x)
    (emit-immediate cs x))
   ((symbol? x)
    (compile-reference cs x sets free locals env))
   ((primitive-call? x locals env)
    (compile-primitive-call cs x sets free locals env))
   ((pair? x)
    (case (car x)
      ((begin)
       (compile-seq cs (cdr x) sets free locals env tail?))
      ((if)
       (compile-conditional cs
			    (cadr x)
			    (caddr x)
			    (cadddr x)
			    sets
			    free
			    locals
			    env
			    tail?))
      ((lambda)
       (compile-closure cs (cadr x) (cddr x) sets free locals env))
      ((call/cc)
       (compile-call/cc cs (cadr x) sets free locals env tail?))
      ((set!)
       (compile-assignment cs (cadr x) (caddr x) sets free locals env))
      (else
       (let ((op (car x)))
;         (if (and (pair? op)
;                  (eq? (car op) 'lambda))
         (if #f
             (compile-closed-application cs
					 (cadr op)
					 (cdr x)
					 (cddr op)
					 sets
                                         free
					 locals
					 env
					 tail?)
             (compile-application cs op (cdr x) sets free locals env tail?))))))
   (else
    (error "Cannot compile atom"))))

;; compiles a top-level expression
(define (compile cs* x*)
  (let ((cs (or cs* (make-compiler-state)))
	(x (transform-exp x*)))
    (compile-exp cs x '() '() '() '() #t)
    ;; this RETURN jumps to the code that
    ;; loaded this expression
    (instr cs 'RETURN)
    (write-code-vector cs)))

(define (compile-to-file file x)
  (with-output-to-file file
    (lambda ()
      (compile #f x))))

;;;
;;; new implementation using ASTs
;;;

(define (compile2 e)
  (let* ((t (transform-exp e))
         (m (meaning t '() #t))
         (b (flag-boxes m))
         (u (update-lexical-addresses b)))
    u))

;; transforms expressions into a syntax tree,
;; calculates lexical addresses, collect free
;; and assigned bindings
;; this is the compiler first pass
(define (meaning e r tail?)
  (cond
   ((immediate? e)
    (meaning-immediate e))
   ((symbol? e)
    (meaning-reference e r))
   ((pair? e)
    (case (car e)
      ((quote)
       (if (= (length e) 2)
           (meaning-quote (cadr e))))
      ((begin)
       (if (> (length e) 1)
           (meaning-sequence (cdr e) r tail?)
           (error "empty 'begin'" e)))
      ((call/cc)
       (if (= (length e) 2)
           (meaning-call/cc (cadr e) r tail?)
           (error "empty 'call/cc'" e)))
      ((if)
       (case (length e)
         ((3)
          (meaning-alternative (cadr e) (caddr e) *undef* r tail?))
         ((4)
          (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
         (else
          (error "ill-formed 'if'" e))))
      ((lambda)
       (if (lambda-exp? e)
           (meaning-lambda (cadr e) (cddr e) r)
           (error "ill-formed 'lambda'" e)))
      ((set!)
       (if (and (= (length e) 3)
                (symbol? (cadr e)))
           (meaning-assignment (cadr e) (caddr e) r)
           (error "ill-formed 'set!'" e)))
      (else
       (meaning-application (car e) (cdr e) r tail?))))))

;; updates lexical address to 

(define (meaning-immediate e)
  (vector 'immed e))

(define (meaning-reference e r)
  (vector 'refer e
                 (lookup2 e r)
                 '()))

(define (meaning-quote e)
  (if (immediate? e)
      (vector 'immed e)
      (vector 'undef)))

(define (meaning-sequence e+ r tail?)
  (let loop ((e+ e+)
             (m+ '()))
    (if (null? (cdr e+))
        (let ((m (meaning (car e+) r tail?)))
          (vector 'sequence (reverse (cons m m+))))
        (loop (cdr e+)
              (cons (meaning (car e+) r #f) m+)))))

(define (meaning-call/cc e r tail?)
  (if (lambda-exp? e)
      (if (one-symbol-list? (cadr e))
          (vector 'call/cc (meaning e r tail?))
          (error "procedure must take one argument" e))
      (error "expression is not a procedure" e)))

(define (meaning-alternative test then else r tail?)
  (vector 'alternative
          (meaning test r #f)
          (meaning then r tail?)
          (if (eq? else *undef*)
              (vector 'undef)
              (meaning else r tail?))))

(define (meaning-lambda n* e+ r)
  (let* ((r2 (cons n* r))
         (m (meaning-sequence e+ r2 #t))
         (free (collect-free2 m 0)))
    (vector 'lambda
            n*
            (collect-sets2 m 0)
            (map (lambda (n)
                    (meaning-reference n r))
                 free)
            m)))

(define (meaning-assignment n e r)
  (let ((address (lookup2 n r))
        (prim? (assq n *primitives*)))
    (if (and prim?
             (not address))
        (error "assignment to primitive not allowed" n)
        (vector 'assign
                n
                address
                (meaning e r #f)))))

(define (meaning-application e e* r tail?)
  (let ((m  (meaning e r #f))
        (m* (map (lambda (e)
                   (meaning e r #f))
                 e*)))
    (let* ((prim (if (and (eq? (vector-ref m 0)
                               'refer)
                          (assq (vector-ref m 1)
                                *primitives*))
                     (list 'prim)
                     '()))
           (flags (if tail?
                      (cons 'tail prim)
                      prim)))
      (vector 'apply
              flags
              m
              m*))))

(define *undef* (list 'undef))

(define (lookup2 n r)
  (if (null? r)
      #f
      (let loop ((i 0)
                 (j 0)
                 (s (car r))
                 (r (cdr r)))
        (if (null? s)
            (if (null? r)
                #f
                (loop 0 (+ j 1) (car r) (cdr r)))
            (let ((v (car s)))
              (if (eq? v n)
                  (cons i j)
                  (loop (+ i 1) j (cdr s) r)))))))

;; tests is an expression is a valid lambda expression
(define (lambda-exp? e)
  (and (pair? e)
       (eq? (car e) 'lambda)
       (> (length e) 2)
       (symbol-exp? (cadr e))))

(define (symbol-exp? e)
  (or (symbol? e)
      (null? e)
      (and (pair? e)
           (foldl* (lambda (a b)
                     (and a b))
                   (map symbol? e)))))

(define (one-symbol-list? e)
  (and (pair? e)
       (symbol? (car e))
       (null? (cdr e))))

(define (collect-free2 m level)
  (case (vector-ref m 0)
    ((refer)
     (set-if-test m > level))
    ((sequence)
     (foldl (lambda (a b)
              (set-union a
                         (collect-free2 b level)))
            '()
            (vector-ref m 1)))
    ((call/cc)
     (collect-free2 (vector-ref m 1) level))
    ((alternative)
     (set-union (collect-free2 (vector-ref m 1) level)
                (set-union (collect-free2 (vector-ref m 2) level)
                           (collect-free2 (vector-ref m 3) level))))
    ((lambda)
     (collect-free2 (vector-ref m 4) (+ level 1)))
    ((assign)
     (set-union (set-if-test m > level)
                (collect-free2 (vector-ref m 3) level)))
    ((apply)
     (set-union (collect-free2 (vector-ref m 2) level)
                (foldl (lambda (a b)
                         (set-union a
                                    (collect-free2 b level)))
                       '()
                       (vector-ref m 3))))
    (else
     '())))

(define (collect-sets2 m level)
  (case (vector-ref m 0)
    ((sequence)
     (foldl (lambda (a b)
              (set-union a
                         (collect-sets2 b level)))
            '()
            (vector-ref m 1)))
    ((call/cc)
     (collect-sets2 (vector-ref m 1) level))
    ((alternative)
     (set-union (collect-sets2 (vector-ref m 1) level)
                (set-union (collect-sets2 (vector-ref m 2) level)
                           (collect-sets2 (vector-ref m 3) level))))
    ((lambda)
     (collect-sets2 (vector-ref m 4) (+ level 1)))
    ((assign)
     (set-if-test m = level))
    ((apply)
     (set-union (collect-sets2 (vector-ref m 2) level)
                (foldl (lambda (a b)
                         (set-union a
                                    (collect-sets2 b level)))
                       '()
                       (vector-ref m 3))))
    (else
     '())))

(define (set-if-test m test level)
  (let ((var     (vector-ref m 1))
        (address (vector-ref m 2)))
    (if (pair? address)
        (let ((frame (cdr address)))
          (if (test frame level)
              (list var)
              '()))
        '())))

;; flags references to assigned variables as boxes
(define (flag-boxes m)

  (define (flag-boxes* m sets level)
    (case (vector-ref m 0)
      ((refer)
       (let ((address (vector-ref m 2)))
         (if address
             (let ((var (vector-ref m 1))
                   (frame (cdr address)))
               (if (and (= frame level)
                        (memq var sets))
                   (vector 'refer
                           var
                           address
                           (cons 'box (vector-ref m 3)))
                   m))
             m)))
      ((sequence)
       (vector 'sequence
               (map (lambda (m)
                      (flag-boxes* m sets level))
                    (vector-ref m 1))))
      ((call/cc)
       (vector 'call/cc
               (flag-boxes* (vector-ref m 1) sets level)))
      ((alternative)
       (vector 'alternative
               (flag-boxes* (vector-ref m 1) sets level)
               (flag-boxes* (vector-ref m 2) sets level)
               (flag-boxes* (vector-ref m 3) sets level)))
      ((lambda)
       (vector 'lambda
               (vector-ref m 1)
               (vector-ref m 2)
               (map (lambda (m)
                      (flag-boxes* m sets level))
                    (vector-ref m 3))
               (flag-boxes* (vector-ref m 4) sets (+ level 1))))
      ((assign)
       (vector 'assign
                (vector-ref m 1)
                (vector-ref m 2)
                (flag-boxes* (vector-ref m 3) sets level)))
      ((apply)
       (vector 'apply
               (vector-ref m 1)
               (flag-boxes* (vector-ref m 2) sets level)
               (map (lambda (m)
                      (flag-boxes* m sets level))
                    (vector-ref m 3))))
      (else
        m)))

  (case (vector-ref m 0)
    ((sequence)
     (vector 'sequence
             (map flag-boxes
                  (vector-ref m 1))))
    ((call/cc)
     (vector 'call/cc
             (flag-boxes (vector-ref m 1))))
    ((alternative)
     (vector 'alternative
             (flag-boxes (vector-ref m 1))
             (flag-boxes (vector-ref m 2))
             (flag-boxes (vector-ref m 3))))
    ((lambda)
     (let* ((sets (vector-ref m 2))
            (boxed (flag-boxes* (vector-ref m 4) sets 0)))
       (vector 'lambda
               (vector-ref m 1)
               sets
               (vector-ref m 3)
               (flag-boxes boxed))))
    ((assign)
     (vector 'assign
             (vector-ref m 1)
             (vector-ref m 2)
             (flag-boxes (vector-ref m 3))))
    ((apply)
     (vector 'apply
             (vector-ref m 1)
             (flag-boxes (vector-ref m 2))
             (map flag-boxes
                  (vector-ref m 3))))
    (else
     m)))

;; updates lexical addresses to remove multi-frame references
(define (update-lexical-addresses m)
  
  (define (update-for-lambda m bound free)

    (define (new-address)
      (let ((var (vector-ref m 1))
            (address (vector-ref m 2)))
        (if address
            (let ((frame (cdr address)))
              (if (zero? frame)
                  (list 'bound (index-of var bound))
                  (list 'free (index-of var free))))
            #f)))

    (case (vector-ref m 0)
      ((refer)
       (vector 'refer
               (vector-ref m 1)
               (new-address)
               (vector-ref m 3)))
      ((sequence)
       (vector 'sequence
               (map (lambda (m)
                      (update-for-lambda m bound free))
                    (vector-ref m 1))))
      ((call/cc)
       (vector 'call/cc
               (update-for-lambda (vector-ref m 1) bound free)))
      ((alternative)
       (vector 'alternative
               (update-for-lambda (vector-ref m 1) bound free)
               (update-for-lambda (vector-ref m 2) bound free)
               (update-for-lambda (vector-ref m 3) bound free)))
      ((lambda)
       (vector 'lambda
               (vector-ref m 1)
               (vector-ref m 2)
               (map (lambda (m)
                      (update-for-lambda m bound free))
                    (vector-ref m 3))
               (vector-ref m 4)))
      ((assign)
       (vector 'assign
               (vector-ref m 1)
               (new-address)
               (update-for-lambda (vector-ref m 3) bound free)))
      ((apply)
       (vector 'apply
               (vector-ref m 1)
               (update-for-lambda (vector-ref m 2) bound free)
               (map (lambda (m)
                      (update-for-lambda m bound free))
                    (vector-ref m 3))))
      (else
       m)))

  (case (vector-ref m 0)
    ((sequence)
     (vector 'sequence
             (map update-lexical-addresses (vector-ref m 1))))
    ((call/cc)
     (vector 'call/cc
             (update-lexical-addresses (vector-ref m 1))))
    ((alternative)
     (vector 'alternative
             (update-lexical-addresses (vector-ref m 1))
             (update-lexical-addresses (vector-ref m 2))
             (update-lexical-addresses (vector-ref m 3))))
    ((lambda)
     (let ((bound (vector-ref m 1))
           (free (map (lambda (m)
                        (vector-ref m 1))
                      (vector-ref m 3))))
       (let ((updated (update-for-lambda (vector-ref m 4)
                                         bound
                                         free)))
         (vector 'lambda
                 bound
                 (vector-ref m 2)
                 (vector-ref m 3)
                 (update-lexical-addresses updated)))))
    ((assign)
     (vector 'assign
             (vector-ref m 1)
             (vector-ref m 2)
             (update-lexical-addresses (vector-ref m 3))))
    ((apply)
     (vector 'apply
             (vector-ref m 1)
             (update-lexical-addresses (vector-ref m 2))
             (map update-lexical-addresses (vector-ref m 3))))
    (else
     m)))


