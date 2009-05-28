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

(define (duna-load file)
  (with-input-from-file (string-append file ".scm")
    (lambda ()
      (let loop ((exps '()))
        (let ((e (read)))
          (if (eof-object? e)
              (compile-to-file (string-append file ".fasl")
                               (reverse exps))
              (loop (cons e exps))))))))

(define (compile-to-file file e+)
  (let ((cs (compile-toplevel e+)))
    (with-output-to-file file
      (lambda ()
        (write-code-vector cs)))))

;; generates code for module,
;; a list of toplevel expressions
(define (compile-toplevel e+)
  (let* ((t+ (map simplify e+))
         (def-exps (filter define-exp? t+))
         (defs (map cadr def-exps))
         (cs (make-compiler-state)))
    (set! *defined-globals*
          (append defs *defined-globals*))
    (let  ((m (meaning-toplevel t+)))
      (generate-toplevel-code cs m)
      (instr cs 'RETURN)
      cs)))

;; compiles toplevel
(define (meaning-toplevel e+)
  (if (null? (cdr e+))
      (meaning (car e+) '() #t)
      (let ((m (meaning (car e+) '() #f))
            (m+ (meaning-toplevel (cdr e+))))
        (vector 'sequence m m+))))

;; already seen globals
(define *defined-globals* '())

;;
;; Preprocessing
;;

;; Transforms derived syntax into primitive syntax
(define (simplify exp)
  (if (pair? exp)
      (let ((op (car exp)))
	(case op
	  ((and)
	   (simplify-and exp))
	  ((or)
	   (simplify-or exp))
          ((begin)
           (simplify-begin exp))
          ((case)
           (simplify-case exp))
	  ((cond)
	   (simplify-cond exp))
          ((define)
           (simplify-define exp))
          ((lambda)
           (simplify-lambda exp))
	  ((let)
	   (if (symbol? (cadr exp))
	       (simplify-named-let exp)
	       (simplify-let exp)))
	  ((let*)
	   (simplify-let* exp))
	  ((letrec)
	   (simplify-letrec exp))
          ((quote)
           exp)
	  (else
	   (map simplify exp))))
      exp))

;; Transform 'and' into series of 'ifs'
(define (simplify-and exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#t
	(let ((test (simplify (car exps)))
	      (rest (cdr exps)))
	  (if (null? rest)
	      (let ((var (gensym)))
		(simplify
		 (list 'let (list (list var test))
		       (list 'if var var '#f))))
	      (list 'if test (simplify (cons 'and rest)) '#f))))))

(define (simplify-or exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#f
	(let ((test (simplify (car exps)))
	      (rest (cdr exps)))
	  (if (null? rest)
	      test
	      (let ((var (gensym)))
		(list 'let (list (list var test))
		      (list 'if var var (simplify (cons 'or rest))))))))))

(define (simplify-begin exp)
  (if (not (null? (cdr exp)))
      (simplify-sequence (cdr exp))
      (error "empty 'begin'" exp)))

(define (simplify-case exp)
  (if (or (null? (cdr exp))
          (null? (cddr exp)))
      (error "invalid 'case' expression" exp)
      (let ((key (simplify (cadr exp)))
            (clauses (reverse (cddr exp)))
            (key-var (gensym)))
        (let collect ((code '())
                      (clauses clauses)
                      (last? #t))
          (if (null? clauses)
              (if (null? code)
                  (error "empty 'case'" exp)
                  (simplify (list 'let
                                  (list (list key-var key))
                                  code)))
              (let ((clause (car clauses)))
                (if (pair? clause)
                    (let ((test (car clause))
                          (rest (cdr clause)))
                      (cond
                      ((eq? test 'else)
                       (if last?
                           (collect (simplify-sequence rest)
                                    (cdr clauses)
                                    #f)
                           (error "'else' must be last clause in 'case'" exp)))
                      ((pair? test)
                       (if (and (pair? rest)
                                (eq? (car rest) '=>))
                           (let ((proc (cadr rest))
                                 (var (gensym)))
                             (collect (simplify (list 'let
                                                      (list (list var
                                                                  (list 'memq
                                                                        key-var
                                                                        (list 'quote
                                                                              test))))
                                                      (list 'if
                                                            var
                                                            (list proc var)
                                                            code)))
                                      (cdr clauses)
                                      #f))
                           (let ((body (simplify-sequence rest)))
                             (collect (list 'if
                                            (list 'memq key-var
                                                  (list 'quote test))
                                            body
                                            code)
                                      (cdr clauses)
                                      #f))))
                      (else
                       (error "ill-formed 'case' clause" clause))))
                    (error "ill-formed 'case' clause" clause))))))))

;; Transform 'cond' into nested 'ifs'
;; a cond clause may use the symbol =>
;; which indicates that the body is actually
;; a lambda
(define (simplify-cond exp)
  (let collect ((code '())
		(clauses (reverse (cdr exp)))
		(last? #t))
    (if (null? clauses)
	(if (null? code)
	    (error "Empty 'cond'" exp)
	    code)
	(let ((clause (car clauses)))
	  (if (pair? clause)
	      (let ((test (simplify (car clause)))
                    (rest (cdr clause)))
                (if (eqv? test 'else)
		    (if last?
			(collect (simplify-sequence rest)
				 (cdr clauses)
				 #f)
			(error "'else' must be last clause in 'cond'" exp))
                    (if (and (pair? rest)
                             (eq? (car rest) '=>))
                        (let ((proc (simplify (cadr rest)))
                              (var (gensym)))
                          (collect (simplify (list 'let
                                                   (list (list var test))
                                                   (list 'if
                                                         var
                                                         (list proc var)
                                                         code)))
                                   (cdr clauses)
                                   #f))
                        (let ((body (simplify-sequence rest)))
                          (collect (list 'if
                                         test
                                         body
                                         code)
                                   (cdr clauses)
                                   #f)))))
              (error "Ill-formed 'cond' clause" clause))))))

(define (simplify-define exp)
  (if (> (length exp) 2)
      (let ((definee (cadr exp))
            (body (cddr exp)))
        (cond
         ((symbol? definee)
          (cons 'define
                (cons definee
                      (simplify body))))
         ((pair? definee)
          (if (and (not (null? definee))
                   (symbol-exp? definee))
              (let ((name (car definee))
                    (args (cdr definee)))
                (list 'define
                      name
                      (simplify
                       (cons 'lambda
                             (cons args
                                   body)))))
              (error "ill-formed 'define'" exp)))
         (else
          (error "ill-formed 'define'" exp))))
      (error "ill-formed 'define'" exp)))

(define (simplify-lambda exp)
  (if (or (null? (cdr exp))
          (null? (cddr exp)))
      (error "ill-formed 'lambda'" exp)
      (let ((args (cadr exp))
            (body (cddr exp)))
        (if (symbol-exp? args)
            (cons 'lambda
                  (list args
                        (simplify-sequence body)))
            (error "ill-formed 'lambda'" exp)))))

;; Transform 'let' into immediate lambda application
(define (simplify-let exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (let loop ((vars '())
	       (args '())
	       (bindings bindings))
      (if (null? bindings)
	  (cons (simplify-lambda (cons 'lambda (cons vars body))) args)
	  (let ((binding (car bindings)))
	    (let ((var (car binding))
		  (arg (simplify (cadr binding))))
	      (if (symbol? var)
		  (loop (cons var vars)
			(cons arg args)
			(cdr bindings))
		  (error "Ill-formed 'let'" exp))))))))

;; Transform let* into cascade of 'lets'
(define (simplify-let* exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (if (null? bindings)
	(simplify (cons 'begin body) #f)
	(let ((first (car bindings))
	      (rest (cdr bindings)))
	  (if (null? rest)
	      (simplify (cons 'let
                              (cons bindings body)))
	      (simplify (list 'let
                              (list first)
                              (cons 'let* (cons rest body)))))))))

;; Transform 'letrec' into 'lambda' plus assignments
(define (simplify-letrec exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (if (null? bindings)
	(cons 'begin body)
	(let loop ((vars '())
		   (mocks '())
                   (body body)
		   (bindings bindings))
	  (if (null? bindings)
	      (cons (simplify (cons 'lambda (cons vars body))) mocks)
	      (let ((binding (car bindings)))
		(let ((var (car binding))
		      (exp (simplify (cadr binding))))
		  (loop (cons var vars)
			(cons '#f mocks)
			(cons (list 'set! var exp) body)
			(cdr bindings)))))))))

;; Transform named 'let' into 'letrec'
(define (simplify-named-let exp)
  (let ((name (cadr exp))
	(bindings (caddr exp))
	(body (cdddr exp)))
    (let loop ((vars '())
	       (exps '())
	       (bindings bindings))
      (if (null? bindings)
	  (simplify
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
		  (error "Ill-formed 'named let'" exp))))))))

(define (simplify-sequence exps)

  (define (rest defs body)
    (cond
     ((any? define-exp? body)
      (error "internal defines must come first" exps))
     ((null? defs)
      (if (null? (cdr body))
          (simplify (car body))
          (cons 'begin (simplify body))))
     (else
      (let ((vars (map cadr defs))
            (exps (map caddr defs)))
        (simplify (cons 'letrec
                        (cons (map list vars exps)
                              body)))))))

  ;; gather internal defines
  (let loop ((defs '())
             (es exps))
    (if (null? es)
        (error "empty sequence" exps)
        (let ((e (car es)))
          (if (define-exp? e)
              (loop (cons e defs) (cdr es))
              (rest (map simplify-define (reverse defs)) es))))))

(define (define-exp? e)
  (and (pair? e)
       (eq? (car e) 'define)))

;;
;; transforms expressions into a syntax tree,
;; calculates lexical addresses, collect free
;; and assigned bindings
;;
(define (meaning e r tail?)
  (if (pair? e)
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
        ((define)
         (meaning-define (cadr e) (caddr e)))
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
             (meaning-lambda (cadr e) (caddr e) r)
             (error "ill-formed 'lambda'" e)))
        ((set!)
         (if (and (= (length e) 3)
                  (symbol? (cadr e)))
             (meaning-assignment (cadr e) (caddr e) r)
             (error "ill-formed 'set!'" e)))
        (else
         (meaning-application (car e) (cdr e) r tail?)))
      (if (symbol? e)
          (meaning-reference e r)
          (meaning-quote e))))

;; updates lexical address to 

(define (meaning-reference n r)
  (vector 'refer
          n
          (lookup n r)
          #f
          '()))

(define (meaning-quote e)
  (vector 'const e))

(define (meaning-sequence e+ r tail?)
  (if (null? (cdr e+))
      (meaning (car e+) r tail?)
      (vector 'sequence
              (meaning (car e+) r #f)
              (meaning-sequence (cdr e+) r tail?))))

(define (meaning-call/cc e r tail?)
  (if (lambda-exp? e)
      (if (one-symbol-list? (cadr e))
          (vector 'call/cc
                  (meaning e r tail?)
                  (if tail?
                      (list 'tail)
                      '()))
          (error "procedure must take one argument" e))
      (error "expression is not a procedure" e)))

(define (meaning-define n e)
  (meaning-assignment n e '()))

(define (meaning-alternative test then else r tail?)
  (vector 'alternative
          (meaning test r #f)
          (meaning then r tail?)
          (if (eq? else *undef*)
              (vector 'undef)
              (meaning else r tail?))))

(define (meaning-lambda n* e r)

  (define (parse n* regular)
    (cond
     ((null? n*)
      (meaning-internal (reverse regular) #f e r))
     ((symbol? n*)
      (meaning-internal (reverse regular) n* e r))
     ((pair? n*)
      (parse (cdr n*) (cons (car n*) regular)))
     (else
      (error "ill-formed 'lambda' arguments" n*))))

  ;; the 'reverse' is here to account for the order
  ;; the arguments are pushed on the stack, moving
  ;; the responsibility to the VM would slow us down
  (define (meaning-internal n* n e r)
    (let* ((an* (reverse (map (lambda (n)
                                (cons n (rename-var n)))
                              (if n
                                  (append n* (list n))
                                  n*))))
           (r2 (cons an* r))
           (m (meaning e r2 #t))
           (bound (map cdr an*))
           (free (collect-free m bound))
           (sets (collect-sets m bound))
           (m2 (flag-boxes m sets))
           (m3 (calculate-addresses m2 bound free)))
      (vector 'lambda
              (cons (if n '>= '=)
                    (length n*))
              bound
              sets
              (map (lambda (n)
                     (meaning-reference n r))
                   free)
              m3)))

  (parse n* '()))

(define (meaning-assignment n e r)
  (let ((n2 (lookup n r))
        (prim? (assq n *primitives*)))
    (if (and prim?
             (eq? n n2))
        (error "assignment to primitive not allowed" n)
        (vector 'assign
                n
                n2
                #f
                (meaning e r #f)))))

(define (meaning-application e e* r tail?)
  (let ((m  (meaning e r #f))
        (m* (meaning-args e* r)))
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

(define (meaning-args e* r)
  (if (null? e*)
      (vector 'arg-null)
      (vector 'arg-list
              (meaning (car e*) r #f)
              (meaning-args (cdr e*) r))))

(define *undef* (list 'undef))

(define (immediate? x)
  (or (char? x)
      (boolean? x)
      (integer? x)
      (null? x)))

(define (lookup n r)
  (if (null? r)
      n
      (let loop ((s (car r))
                 (r (cdr r)))
        (if (null? s)
            (if (null? r)
                n
                (loop (car r) (cdr r)))
            (let ((v (car s)))
              (if (eq? n (car v))
                  (cdr v)
                  (loop (cdr s) r)))))))

(define rename-var
  (let ((c 0))
    (lambda (n)
      (set! c (+ c 1))
      (string->symbol
       (string-append (symbol->string n)
                      (number->string c))))))

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
           (symbol? (car e))
           (symbol-exp? (cdr e)))))

(define (one-symbol-list? e)
  (and (pair? e)
       (symbol? (car e))
       (null? (cdr e))))

(define (collect-free m bound)
  (case (vector-ref m 0)
    ((refer)
     (let ((n (vector-ref m 1))
           (an (vector-ref m 2)))
       (if (or (eq? n an)
               (memq an bound))
           '()
           (list an))))
    ((sequence)
     (set-union (collect-free (vector-ref m 1) bound)
                (collect-free (vector-ref m 2) bound)))
    ((call/cc)
     (collect-free (vector-ref m 1) bound))
    ((alternative)
     (set-union (collect-free (vector-ref m 1) bound)
                (set-union (collect-free (vector-ref m 2) bound)
                           (collect-free (vector-ref m 3) bound))))
    ((lambda)
     (collect-free (vector-ref m 5)
                   (append (vector-ref m 2) bound)))
    ((assign)
     (set-union (let ((n (vector-ref m 1))
                      (an (vector-ref m 2)))
                  (if (or (eq? n an)
                          (memq an bound))
                      '()
                      (list an)))
                (collect-free (vector-ref m 4) bound)))
    ((apply)
     (set-union (collect-free (vector-ref m 2) bound)
                (collect-free (vector-ref m 3) bound)))
    ((arg-list)
     (set-union (collect-free (vector-ref m 1) bound)
                (collect-free (vector-ref m 2) bound)))
    (else
     '())))

(define (collect-sets m bound)
  (case (vector-ref m 0)
    ((sequence)
     (set-union (collect-sets (vector-ref m 1) bound)
                (collect-sets (vector-ref m 2) bound)))
    ((call/cc)
     (collect-sets (vector-ref m 1) bound))
    ((alternative)
     (set-union (collect-sets (vector-ref m 1) bound)
                (set-union (collect-sets (vector-ref m 2) bound)
                           (collect-sets (vector-ref m 3) bound))))
    ((lambda)
     (collect-sets (vector-ref m 5) bound))
    ((assign)
     (let ((n (vector-ref m 1))
           (an (vector-ref m 2)))
       (if (and (not (eq? n an))
                (memq an bound))
           (list an)
           '())))
    ((apply)
     (set-union (collect-sets (vector-ref m 2) bound)
                (collect-sets (vector-ref m 3) bound)))
    ((arg-list)
     (set-union (collect-sets (vector-ref m 1) bound)
                (collect-sets (vector-ref m 2) bound)))
    (else
     '())))

;; flags references to assigned variables as boxes
(define (flag-boxes m sets)

  (define (refer-handler visitor m sets)
    (let ((an (vector-ref m 2))
          (flags (vector-ref m 4)))
      (vector 'refer
              (vector-ref m 1)
              an
              (vector-ref m 3)
              (if (memq an sets)
                  (cons 'box flags)
                  flags))))

  (define (lambda-handler visitor m sets)
    (vector 'lambda
            (vector-ref m 1)
            (vector-ref m 2)
            (vector-ref m 3)
            (map visitor (vector-ref m 4))
            (visitor (vector-ref m 5))))

  (visit (list (cons 'refer
                     refer-handler)
               (cons 'lambda
                     lambda-handler))
         m
         sets))

(define (calculate-addresses m bound free)

  (define (refer-handler visitor m bound free)
    (let ((an (vector-ref m 2)))
      (vector 'refer
              (vector-ref m 1)
              an
              (cond
               ((index-of an bound) =>
                (lambda (i)
                  (cons 'bound i)))
               ((index-of an free) =>
                (lambda (i)
                  (cons 'free i)))
               (else #f))
              (vector-ref m 4))))

  (define (assign-handler visitor m bound free)
    (let ((an (vector-ref m 2)))
      (vector 'assign
              (vector-ref m 1)
              an
              (cond
               ((index-of an bound) =>
                (lambda (i)
                  (cons 'bound i)))
               ((index-of an free) =>
                (lambda (i)
                  (cons 'free i)))
               (else #f))
              (visitor (vector-ref m 4)))))

  (define (lambda-handler visitor m bound free)
    (vector 'lambda
            (vector-ref m 1)
            (vector-ref m 2)
            (vector-ref m 3)
            (map visitor (vector-ref m 4))
            (vector-ref m 5)))

  (visit (list (cons 'refer
                     refer-handler)
               (cons 'assign
                     assign-handler)
               (cons 'lambda
                     lambda-handler))
         m
         bound
         free))

;; this pass generates the code for the virtual machine
(define (generate-toplevel-code cs m)
  (let ((c (collect-constants! cs m)))

    ;; emit code to initialise constants before
    ;; entering real code
    (let loop ((i 0)
               (consts (vector-ref cs 1)))
      (if (not (null? consts))
          (let ((const (car consts)))
            (emit-constant cs const)
            (instr1 cs 'CONST-INIT i)
            (loop (+ i 1) (cdr consts)))))

    ;; generate rest of code
    (generate-code cs c)))

(define (collect-constants! cs m)

  (define (const-handler visitor m)
    (let ((c (vector-ref m 1)))
      (if (immediate? c)
          (vector 'immed c)
          (let ((index (install-constant! cs c)))
            (vector 'const index)))))

  (visit (list (cons 'const
                     const-handler))
         m))

(define (generate-code cs m)
  (case (vector-ref m 0)
    ((undef)
     (generate-undef cs))
    ((const)
     (generate-constant cs m))
    ((immed)
     (generate-immediate cs m))
    ((refer)
     (generate-reference cs m #t))
    ((sequence)
     (generate-sequence cs m))
    ((call/cc)
     (generate-call/cc cs m))
    ((define)
     (generate-define cs m))
    ((alternative)
     (generate-alternative cs m))
    ((lambda)
     (generate-lambda cs m))
    ((assign)
     (generate-assignment cs m))
    ((apply)
     (generate-apply cs m))
    (else
     (error "unknown AST node" m))))

(define (generate-undef cs)
  (instr cs 'LOAD-UNDEF))

(define (generate-constant cs m)
  (instr1 cs 'CONST (vector-ref m 1)))

(define (generate-immediate cs m)
  (emit-constant cs (vector-ref m 1)))

(define (generate-reference cs m unbox?)
  (let ((address (vector-ref m 3)))
    (if address
        (let ((kind (car address))
              (pos  (cdr address)))
          (case kind
            ((bound)
             (case pos
               ((0)
                (instr cs 'LOAD0))
               ((1)
                (instr cs 'LOAD1))
               ((2)
                (instr cs 'LOAD2))
               ((3)
                (instr cs 'LOAD3))
               (else
                (instr1 cs 'LOAD pos))))
            ((free)
             (instr1 cs 'LOAD-FREE pos))
            (else
             (error "unknown binding type" m)))
          (and unbox?
               (memq 'box (vector-ref m 4))
               (instr cs 'OPEN-BOX)))
        (let* ((var (vector-ref m 1))
               (index (install-global! cs var)))
          (if (memq var *defined-globals*)
              (instr1 cs 'GLOBAL-REF index)
              (instr1 cs 'CHECKED-GLOBAL-REF index))))))

(define (generate-sequence cs m)
  (generate-code cs (vector-ref m 1))
  (generate-code cs (vector-ref m 2)))

(define (generate-call/cc cs m)
  
  ;; creates a closure that will restore a saved
  ;; continuation if called - the continuation
  ;; is stored in the closure as a free variable
  (define (generate-continuation-closure cs)
    (instr1 cs 'MAKE-CLOSURE 1)
    ;; jump over closure code
    (instr1 cs 'JMP 5)
    ;; value given to the continuation
    (instr cs 'LOAD0)
    (instr cs 'PUSH)
    (instr1 cs 'LOAD-FREE 0)
    (instr cs 'REST-CONT)
    (instr cs 'RETURN))

  (let ((i (code-size cs))
        (tail? (memq 'tail (vector-ref m 2))))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (instr cs 'LOAD-ZERO)
    (instr cs 'PUSH)
    (instr cs 'SAVE-CONT)
    (instr cs 'PUSH)
    (generate-continuation-closure cs)
    (instr cs 'PUSH)
    ;; calling closure given to call/cc with
    ;; continuation-restoring closure as sole argument
    (instr cs 'LOAD-ONE)
    (instr cs 'PUSH)
    (generate-lambda cs (vector-ref m 1))
    (instr cs (if tail? 'TAIL-CALL 'CALL))
    ;; back-patching return address
    (or tail? (patch-instr! cs i (code-size cs)))))

(define (generate-alternative cs m)
  (let ((test (vector-ref m 1))
        (then (vector-ref m 2))
        (else (vector-ref m 3)))
    (generate-code cs test)
    (let ((i (code-size cs)))
      ;; this will be back-patched later
      (instr1 cs 'JMP-IF 0)
      (generate-code cs else)
      (let ((j (code-size cs)))
        ;; this will be back-patched later
        (instr1 cs 'JMP 0)
        (let ((k (code-size cs)))
          ;; back-patching if jump
          (patch-instr! cs i (- k i 1))
          (generate-code cs then)
          (let ((m (code-size cs)))
            ;; back-patching else jmp
            (patch-instr! cs j (- m j 1))))))))

(define (generate-lambda cs m)
  (let ((arity (vector-ref m 1))
        (bound (vector-ref m 2))
        (sets  (vector-ref m 3))
        (free  (vector-ref m 4)))
    (let loop ((f (reverse free)))
      (if (null? f)
          (let ((len (length free)))
            (instr1 cs 'MAKE-CLOSURE len)
            (let ((i (code-size cs)))
              ;; this will be back-patched later
              (instr1 cs 'JMP 0)
              (case (car arity)
                ((=)
                 (instr1 cs 'ARITY= (cdr arity)))
                ((>=)
                 (instr1 cs 'ARITY>= (cdr arity))
                 (instr1 cs 'LISTIFY (cdr arity))))
              (make-boxes cs bound sets)
              (generate-code cs (vector-ref m 5))
              (instr cs 'RETURN)
              (let ((j (code-size cs)))
                ;; back-patching jump over closure code
                (patch-instr! cs i (- j i 1)))))
          (let ((ref (car f)))
            (generate-reference cs ref #f)
            (instr cs 'PUSH)
            (loop (cdr f)))))))

(define (generate-assignment cs m)
  (generate-code cs (vector-ref m 4))
  (let ((address (vector-ref m 3)))
    (if address
        (let ((kind (car address))
              (pos  (cdr address)))
          (case kind
            ((bound)
             (instr1 cs 'ASSIGN pos))
            ((free)
             (instr1 cs 'ASSIGN-FREE pos))
            (else
             (error "unknown binding type" m))))
        (let* ((var (vector-ref m 1))
               (index (install-global! cs var)))
          (if (memq var *defined-globals*)
              (instr1 cs 'GLOBAL-SET index)
              (instr1 cs 'CHECKED-GLOBAL-SET index))))))

(define (generate-apply cs m)
  (if (memq 'prim (vector-ref m 1))
      (generate-primitive-apply cs m)
      (generate-common-apply cs m)))

(define (generate-primitive-apply cs m)
  (let* ((ref (vector-ref m 2))
         (args (vector-ref m 3))
         (prim (vector-ref ref 1))
         (prim-rec (assv prim *primitives*)))
    (if prim-rec
        (let ((code (cadr prim-rec))
              (arity (caddr prim-rec)))
          (case arity
            ((0)
             (instr cs code))
            ((1)
             (generate-code cs (vector-ref args 1))
             (instr cs code))
            ((2)
             (generate-code cs (vector-ref args 1))
             (instr cs 'PUSH)
             (generate-code cs (vector-ref (vector-ref args 2) 1))
             (instr cs code))
            (else
             (error "Primitive with unknown arity"))))
        (error "Unknown primitive"))))

(define (generate-common-apply cs m)
  (let ((i (code-size cs))
        (tail? (memq 'tail (vector-ref m 1))))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (let ((len (generate-push-arguments cs (vector-ref m 3))))
      (emit-immediate cs len)
      (instr cs 'PUSH)
      (generate-code cs (vector-ref m 2))
      (instr cs (if tail? 'TAIL-CALL 'CALL))
      ;; back-patching return address
      ;; this not a position-independent value
      (or tail? (patch-instr! cs i (code-size cs))))))

(define (generate-push-arguments cs m)
  (let loop ((len 0)
             (m m))
    (let ((kind (vector-ref m 0)))
      (if (eq? kind 'arg-null)
          len
          (let ((arg (vector-ref m 1)))
            (generate-code cs arg)
            (instr cs 'PUSH)
            (loop (+ len 1) (vector-ref m 2)))))))

;; Create instructions to box arguments to closure
;; that are assigned somewhere in the code
(define (make-boxes cs vars sets)
  (for-each (lambda (v)
	      (and (memq v sets)
		   (instr1 cs 'INSERT-BOX (index-of v vars))))
	    vars))

;; procs is an alist of (tag . proc)
;; the procedure gets the node and the visitor
;; to apply it again
(define (visit procs m . args)

  (define (visitor m)
    (let* ((tag (vector-ref m 0))
           (tag-proc (assq tag procs)))
      (if tag-proc
          (apply (cdr tag-proc) visitor m args)
          (case tag
            ((sequence)
             (vector 'sequence
                     (visitor (vector-ref m 1))
                     (visitor (vector-ref m 2))))
            ((call/cc)
             (vector 'call/cc
                     (visitor (vector-ref m 1))
                     (vector-ref m 2)))
            ((alternative)
             (vector 'alternative
                     (visitor (vector-ref m 1))
                     (visitor (vector-ref m 2))
                     (visitor (vector-ref m 3))))
            ((lambda)
             (vector 'lambda
                     (vector-ref m 1)
                     (vector-ref m 2)
                     (vector-ref m 3)
                     (map visitor (vector-ref m 4))
                     (visitor (vector-ref m 5))))
            ((assign)
             (vector 'assign
                     (vector-ref m 1)
                     (vector-ref m 2)
                     (vector-ref m 3)
                     (visitor (vector-ref m 4))))
            ((apply)
             (vector 'apply
                     (vector-ref m 1)
                     (visitor (vector-ref m 2))
                     (visitor (vector-ref m 3))))
            ((arg-list)
             (vector 'arg-list
                     (visitor (vector-ref m 1))
                     (visitor (vector-ref m 2))))
            (else
             m)))))

  (visitor m))

;;
;; low-level code generation
;;

;; bytecode instructions
(define *opcodes*
  ;; basic VM instructions
  '((LOAD-NIL           . 1)
    (LOAD-FALSE         . 2)
    (LOAD-TRUE          . 3)
    (LOAD-ZERO          . 4)
    (LOAD-ONE           . 5)
    (LOAD-FIXNUM        . 6)
    (LOAD-CHAR          . 7)
    (PUSH               . 8)
    (LOAD0              . 9)
    (LOAD1              . 10)
    (LOAD2              . 11)
    (LOAD3              . 12)
    (LOAD               . 13)
    (MAKE-CLOSURE       . 14)
    (CALL               . 15)
    (RETURN             . 16)
    (JMP-IF             . 17)
    (JMP                . 18)
    (LOAD-FREE          . 19)
    (SAVE-CONT          . 20)
    (REST-CONT          . 21)
    (ASSIGN             . 22)
    (ASSIGN-FREE        . 23)
    (BOX                . 24)
    (OPEN-BOX           . 25)
    (FRAME              . 26)
    (TAIL-CALL          . 27)
    (HALT               . 28)
    (LOAD-LOCAL         . 29)
    (INSERT-BOX         . 30)
    (ASSIGN-LOCAL       . 31)
    (POP                . 32)
    (GLOBAL-REF         . 33)
    (CHECKED-GLOBAL-REF . 34)
    (GLOBAL-SET         . 35)
    (CHECKED-GLOBAL-SET . 36)
    (LOAD-UNDEF         . 37)
    (CONST              . 38)
    (CONST-INIT         . 39)
    (ARITY=             . 40)
    (ARITY>=            . 41)
    (LISTIFY            . 42)
    (ABORT              . 43)

    ;; type predicates
    (NULL-P             . 81)
    (BOOL-P             . 82)
    (CHAR-P             . 83)
    (FIXNUM-P           . 84)
    (PAIR-P             . 85)

    ;; primitives optimised as instructions
    (INC                . 101)
    (DEC                . 102)
    (FIXNUM-TO-CHAR     . 103)
    (CHAR-TO-FIXNUM     . 104)
    (ZERO-P             . 105)
    (NOT                . 106)
    (PLUS               . 107)
    (MINUS              . 108)
    (MULT               . 109)
    (CONS               . 110)
    (CAR                . 111)
    (CDR                . 112)
    (NUM-EQ             . 113)
    (EQ                 . 114)
    (EQV                . 115)
    (MAKE-STRING        . 116)
    (STRING-SET         . 117)
    (STRING-TO-SYMBOL   . 118)
    (MAKE-VECTOR        . 119)
    (VECTOR-SET         . 120)
    (WRITE              . 121)
    (DEBUG              . 122)))

(define (make-compiler-state)
  (vector
   '()                        ;; globals
   '()                        ;; constants
   0                          ;; Index of next instruction
   (make-vector 32 0)))       ;; code vector

(define (code-capacity cs)
  (vector-length (vector-ref cs 3)))

(define (code-size cs)
  (vector-ref cs 2))

(define (instr cs op)
  (add-to-code! cs (vector op #f)))

(define (instr1 cs op arg)
  (add-to-code! cs (vector op arg)))

;; used by code that need to patch previously emitted
;; instructions
(define (patch-instr! cs i arg)
  (vector-set! (vector-ref (vector-ref cs 3) i) 1 arg))

(define (install-constant! cs val)
  (let* ((consts (vector-ref cs 1))
         (index (index-of val consts)))
    (or index
        (let ((new-index (length consts)))
          (vector-set! cs 1 (append consts (list val)))
          new-index))))

(define (install-global! cs var)
  (let* ((globals (vector-ref cs 0))
         (index (index-of var globals)))
    (or index
        (let ((new-index (length globals)))
          (vector-set! cs 0 (append globals (list var)))
          new-index))))

(define (emit-constant cs c)
  (cond
   ((immediate? c)
    (emit-immediate cs c))
   ((pair? c)
    (emit-pair cs c))
   ((string? c)
    (emit-string cs c))
   ((symbol? c)
    (emit-string cs (symbol->string c))
    (instr cs 'STRING-TO-SYMBOL))
   ((vector? c)
    (emit-vector cs c))
   (else
    (error "unimplemented complex constant" c))))

(define (emit-pair cs p)
  (emit-constant cs (car p))
  (instr cs 'PUSH)
  (emit-constant cs (cdr p))
  (instr cs 'CONS))

(define (emit-string cs str)
  (let ((len (string-length str)))
    (emit-immediate cs len)
    (instr cs 'MAKE-STRING)
    (let loop ((i 0))
      (if (< i len)
          (begin
            (instr cs 'PUSH)
            (emit-immediate cs i)
            (instr cs 'PUSH)
            (emit-immediate cs (string-ref str i))
            (instr cs 'STRING-SET)
            (loop (+ i 1)))))))

(define (emit-vector cs vec)
  (let ((len (vector-length vec)))
    (emit-immediate cs len)
    (instr cs 'MAKE-VECTOR)
    (let loop ((i 0))
      (if (< i len)
          (begin
            (instr cs 'PUSH)
            (emit-immediate cs i)
            (instr cs 'PUSH)
            (emit-constant cs (vector-ref vec i))
            (instr cs 'VECTOR-SET)
            (loop (+ i 1)))))))

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

(define (write-code-vector cs)
  (display "#( ")
  (let ((globals (vector-ref cs 0))
        (consts (vector-ref cs 1))
        (code (vector-ref cs 3))
	(code-size (code-size cs)))
    
    ;; globals
    (write-fixnum (length globals))
    (for-each write-global globals)

    ;; constants
    (write-fixnum (length consts))

    ;; code
    (write-fixnum code-size)
    (let loop ((i 0))
      (if (= i code-size)
	  (display ")")
	  (let ((instr (vector-ref code i)))
	    (let ((op   (vector-ref instr 0))
		  (arg1 (vector-ref instr 1)))
	      (display (cdr (assv op *opcodes*)))
	      (display " ")
	      (and arg1 (write-fixnum arg1)))
	    (loop (+ i 1)))))))

(define (write-global sym)
  (let* ((str (symbol->string sym))
         (len (string-length str)))
    (write-fixnum len)
    (let loop ((i 0))
      (if (< i len)
          (begin
            (write-fixnum (char->integer (string-ref str i)))
            (loop (+ i 1)))))))

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

(define (extend-code-vector! cs)
  (let* ((len (vector-length (vector-ref cs 3)))
         (new-len (round (/ (* 3 len) 2)))
         (new-vec (make-vector new-len 0)))
    (let loop ((i 0))
      (if (= i len)
          (vector-set! cs 3 new-vec)
          (begin
            (vector-set! new-vec i (vector-ref (vector-ref cs 3) i))
            (loop (+ i 1)))))))

(define (add-to-code! cs instr)
  (let ((i (code-size cs)))
    (and (= i (code-capacity cs))
         (extend-code-vector! cs))
    (vector-set! cs 2 (+ i 1))
    (vector-set! (vector-ref cs 3) i instr)))

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
    (cdr CDR 1)
    (= NUM-EQ 2)
    (eq? EQ 2)
    (eqv? EQV 2)
    (string->symbol STRING-TO-SYMBOL 1)
    (pair? PAIR-P 1)
    (write WRITE 1)
    (abort ABORT 0)
    (debug DEBUG 1)))

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

;; Gives the numerical index of an item in a list
(define (index-of item list)
  (let loop ((i 0)
             (list list))
    (if (null? list)
        #f
        (let ((ele (car list)))
          (if (equal? item ele)
              i
              (loop (+ i 1) (cdr list)))))))

(define (all? pred coll)
  (if (null? coll)
      #t
      (and (pred (car coll))
           (all? pred (cdr coll)))))

(define (any? pred coll)
  (if (null? coll)
      #f
      (or (pred (car coll))
          (any? pred (cdr coll)))))

(define (filter pred coll)
  (let loop ((ret '())
             (coll coll))
    (if (null? coll)
        (reverse ret)
        (let ((a (car coll)))
          (if (pred a)
              (loop (cons a ret) (cdr coll))
              (loop ret (cdr coll)))))))

