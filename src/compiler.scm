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

(define (compile-to-file file e)
  (let ((cs (make-compiler-state)))
    (with-output-to-file file
      (lambda ()
        (compile cs e)))))

(define (compile cs e)
  (let* ((t (simplify e #t))
         (m (meaning t '() #t))
         (b (flag-boxes m))
         (u (update-lexical-addresses b)))
    (generate-code cs u)
    (instr cs 'RETURN)
    (write-code-vector cs)))

;;
;; Preprocessing
;;

;; Transforms derived syntax into primitive syntax
(define (simplify exp top?)
  (if (pair? exp)
      (let ((op (car exp)))
	(case op
	  ((and)
	   (simplify-and exp))
	  ((or)
	   (simplify-or exp))
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
	  (else
	   (map (lambda (e)
                  (simplify e #f))
                exp))))
      exp))

;; Transform 'and' into series of 'ifs'
(define (simplify-and exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#t
	(let ((test (simplify (car exps) #f))
	      (rest (cdr exps)))
	  (if (null? rest)
	      (let ((var (gensym)))
		(simplify
		 (list 'let (list (list var test))
		       (list 'if var var '#f))
                 #f))
	      (list 'if test (simplify (cons 'and rest) #f) '#f))))))

(define (simplify-or exp)
  (let ((exps (cdr exp)))
    (if (null? exps)
	'#f
	(let ((test (simplify (car exps) #f))
	      (rest (cdr exps)))
	  (if (null? rest)
	      test
	      (let ((var (gensym)))
		(list 'let (list (list var test))
		      (list 'if var var (simplify (cons 'or rest))))))))))

;; Transform 'cond' into nested 'ifs'
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
	      (let ((test (simplify (car clause) #f))
		    (body (simplify (cdr clause) #f)))
		(if (eqv? test 'else)
		    (if last?
			(collect (cons 'begin body)
				 (cdr clauses)
				 #f)
			(error "'else' must be last clause in 'cond'" exp))
		    (collect (list 'if
				   test
				   (cons 'begin body)
				   code)
			     (cdr clauses)
			     #f)))
	      (error "Ill-formed 'cond' clause" clause))))))

(define (simplify-define exp)
  (if (> (length exp) 2)
      (let ((definee (cadr exp))
            (body (cddr exp)))
        (cond
         ((symbol? definee)
          (cons 'define
                (cons definee
                      (simplify body #f))))
         ((pair? definee)
          (if (and (not (null? definee))
                   (all? symbol? definee))
              (let ((name (car definee))
                    (args (cdr definee)))
                (list 'define
                      name
                      (simplify
                       (cons 'lambda
                             (cons args
                                   body))
                             #f)))
              (error "ill-formed 'define'" exp)))
         (else
          (error "ill-formed 'define'" exp))))
      (error "ill-formed 'define'" exp)))

;; transforms internal defines
(define (simplify-lambda exp)
  (cons 'lambda
        (cons (cadr exp)
              (simplify (cddr exp) #f))))

;; Transform 'let' into immediate lambda application
(define (simplify-let exp)
  (let ((bindings (cadr exp))
	(body (simplify (cddr exp) #f)))
    (let loop ((vars '())
	       (args '())
	       (bindings bindings))
      (if (null? bindings)
	  (cons (cons 'lambda (cons vars body)) args)
	  (let ((binding (car bindings)))
	    (let ((var (car binding))
		  (arg (simplify (cadr binding) #f)))
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
                              (cons bindings body))
                        #f)
	      (simplify (list 'let
                              (list first)
                              (cons 'let* (cons rest body)))
                        #f))))))

;; Transform 'letrec' into 'lambda' plus assignments
(define (simplify-letrec exp)
  (let ((bindings (cadr exp))
	(body (cddr exp)))
    (if (null? bindings)
	(cons 'begin body)
	(let loop ((vars '())
		   (mocks '())
		   (body (simplify body #f))
		   (bindings bindings))
	  (if (null? bindings)
	      (cons (cons 'lambda (cons vars body)) mocks)
	      (let ((binding (car bindings)))
		(let ((var (car binding))
		      (exp (simplify (cadr binding) #f)))
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
		 (cons name exps))
           #f)
	  (let ((binding (car bindings)))
	    (let ((var (car binding))
		  (exp (cadr binding)))
	      (if (symbol? var)
		  (loop (cons var vars)
			(cons exp exps)
			(cdr bindings))
		  (error "Ill-formed 'named let'" exp))))))))

;;
;; transforms expressions into a syntax tree,
;; calculates lexical addresses, collect free
;; and assigned bindings
;;
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
                 (lookup e r)
                 '()))

(define (meaning-quote e)
  (if (immediate? e)
      (vector 'immed e)
      (vector 'undef)))

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
         (free (collect-free m 0)))
    (vector 'lambda
            n*
            (collect-sets m 0)
            (map (lambda (n)
                    (meaning-reference n r))
                 free)
            m)))

(define (meaning-assignment n e r)
  (let ((address (lookup n r))
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

(define (lookup n r)
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
           (all? symbol? e))))

(define (one-symbol-list? e)
  (and (pair? e)
       (symbol? (car e))
       (null? (cdr e))))

(define (collect-free m level)
  (case (vector-ref m 0)
    ((refer)
     (set-if-test m > level))
    ((sequence)
     (set-union (collect-free (vector-ref m 1) level)
                (collect-free (vector-ref m 2) level)))
    ((call/cc)
     (collect-free (vector-ref m 1) level))
    ((alternative)
     (set-union (collect-free (vector-ref m 1) level)
                (set-union (collect-free (vector-ref m 2) level)
                           (collect-free (vector-ref m 3) level))))
    ((lambda)
     (collect-free (vector-ref m 4) (+ level 1)))
    ((assign)
     (set-union (set-if-test m > level)
                (collect-free (vector-ref m 3) level)))
    ((apply)
     (set-union (collect-free (vector-ref m 2) level)
                (collect-free (vector-ref m 3) level)))
    ((arg-list)
     (set-union (collect-free (vector-ref m 1) level)
                (collect-free (vector-ref m 2) level)))
    (else
     '())))

(define (collect-sets m level)
  (case (vector-ref m 0)
    ((sequence)
     (set-union (collect-sets (vector-ref m 1) level)
                (collect-sets (vector-ref m 2) level)))
    ((call/cc)
     (collect-sets (vector-ref m 1) level))
    ((alternative)
     (set-union (collect-sets (vector-ref m 1) level)
                (set-union (collect-sets (vector-ref m 2) level)
                           (collect-sets (vector-ref m 3) level))))
    ((lambda)
     (collect-sets (vector-ref m 4) (+ level 1)))
    ((assign)
     (set-if-test m = level))
    ((apply)
     (set-union (collect-sets (vector-ref m 2) level)
                (collect-sets (vector-ref m 3) level)))
    ((arg-list)
     (set-union (collect-sets (vector-ref m 1) level)
                (collect-sets (vector-ref m 2) level)))
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
               (flag-boxes* (vector-ref m 1) sets level)
               (flag-boxes* (vector-ref m 2) sets level)))
      ((call/cc)
       (vector 'call/cc
               (flag-boxes* (vector-ref m 1) sets level)
               (vector-ref m 2)))
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
               (flag-boxes* (vector-ref m 3) sets level)))
      ((arg-list)
       (vector 'arg-list
               (flag-boxes* (vector-ref m 1) sets level)
               (flag-boxes* (vector-ref m 2) sets level)))
      (else
        m)))

  (case (vector-ref m 0)
    ((sequence)
     (vector 'sequence
             (flag-boxes (vector-ref m 1))
             (flag-boxes (vector-ref m 2))))
    ((call/cc)
     (vector 'call/cc
             (flag-boxes (vector-ref m 1))
             (vector-ref m 2)))
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
             (flag-boxes (vector-ref m 3))))
    ((arg-list)
     (vector 'arg-list
             (flag-boxes (vector-ref m 1))
             (flag-boxes (vector-ref m 2))))
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
                  (cons 'bound (index-of var bound))
                  (cons 'free (index-of var free))))
            #f)))

    (case (vector-ref m 0)
      ((refer)
       (vector 'refer
               (vector-ref m 1)
               (new-address)
               (vector-ref m 3)))
      ((sequence)
       (vector 'sequence
               (update-for-lambda (vector-ref m 1) bound free)
               (update-for-lambda (vector-ref m 2) bound free)))
      ((call/cc)
       (vector 'call/cc
               (update-for-lambda (vector-ref m 1) bound free)
               (vector-ref m 2)))
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
               (update-for-lambda (vector-ref m 3) bound free)))
      ((arg-list)
       (vector 'arg-list
               (update-for-lambda (vector-ref m 1) bound free)
               (update-for-lambda (vector-ref m 2) bound free)))
      (else
       m)))

  (case (vector-ref m 0)
    ((sequence)
     (vector 'sequence
             (update-lexical-addresses (vector-ref m 1))
             (update-lexical-addresses (vector-ref m 2))))
    ((call/cc)
     (vector 'call/cc
             (update-lexical-addresses (vector-ref m 1))
             (vector-ref m 2)))
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
             (update-lexical-addresses (vector-ref m 3))))
    ((arg-list)
     (vector 'arg-list
             (update-lexical-addresses (vector-ref m 1))
             (update-lexical-addresses (vector-ref m 2))))
    (else
     m)))

;; this pass generates the code for the virtual machine
(define (generate-code cs m)
  (case (vector-ref m 0)
    ((undef)
     (generate-undef cs))
    ((immed)
     (generate-immediate cs m))
    ((refer)
     (generate-reference cs m #t))
    ((sequence)
     (generate-sequence cs m))
    ((call/cc)
     (generate-call/cc cs m))
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

(define (generate-immediate cs m)
  (emit-immediate cs (vector-ref m 1)))

(define (generate-reference cs m unbox?)
  (let ((address (vector-ref m 2)))
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
             (error "unknown binding type" m))))
        (error "binding without address" m)))
  (and unbox?
       (memq 'box (vector-ref m 3))
       (instr cs 'OPEN-BOX)))

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
  (let ((bound (vector-ref m 1))
        (sets  (vector-ref m 2))
        (free  (vector-ref m 3)))
    (let loop ((f (reverse free)))
      (if (null? f)
        (let ((len (length free)))
          (instr1 cs 'MAKE-CLOSURE len)
          (let ((i (code-size cs)))
            ;; this will be back-patched later
            (instr1 cs 'JMP 0)
            (make-boxes cs bound sets)
            (generate-code cs (vector-ref m 4))
            (instr cs 'RETURN)
            (let ((j (code-size cs)))
              ;; back-patching jump over closure code
              (patch-instr! cs i (- j i 1)))))
        (let ((ref (car f)))
          (generate-reference cs ref #f)
          (instr cs 'PUSH)
          (loop (cdr f)))))))

(define (generate-assignment cs m)
  (let ((address (vector-ref m 2)))
    (if address
        (let ((kind (car address))
              (pos  (cdr address)))
          (generate-code cs (vector-ref m 3))
          (case kind
            ((bound)
             (instr1 cs 'ASSIGN pos))
            ((free)
             (instr1 cs 'ASSIGN-FREE pos))
            (else
             (error "unknown binding type" m))))
        (error "binding without address" m))))

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
          (cond
           ((= arity 1)
            (generate-code cs (vector-ref args 1))
            (instr cs code))
           ((= arity 2)
            (generate-code cs (vector-ref (vector-ref args 2) 1))
            (instr cs 'PUSH)
            (generate-code cs (vector-ref args 1))
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
  (let ((kind (vector-ref m 0)))
    (if (eq? kind 'arg-null)
        0
        (let ((len (generate-push-arguments cs (vector-ref m 2))))
          (generate-code cs (vector-ref m 1))
          (instr cs 'PUSH)
          (+ len 1)))))

;; Create instructions to box arguments to closure
;; that are assigned somewhere in the code
(define (make-boxes cs vars sets)
  (for-each (lambda (v)
	      (and (memq v sets)
		   (instr1 cs 'INSERT-BOX (index-of v vars))))
	    vars))

;;
;; low-level code generation
;;

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
          (if (eq? item ele)
              i
              (loop (+ i 1) (cdr list)))))))

(define (all? pred coll)
  (if (null? coll)
      #t
      (and (pred (car coll))
           (all? pred (cdr coll)))))

