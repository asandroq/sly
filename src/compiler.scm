;;;
;;; The Sly Scheme compiler
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

(define (sly-eval e)
  (compile-toplevel (list e)))

(define (sly-compile-file file)
  (let ((in  (open-input-file  (string-append file ".scm")))
        (out (open-output-file (string-append file ".fasl"))))
    (let loop ((exps '()))
      (let ((e (read in)))
        (if (eof-object? e)
            (let ((cs (compile-toplevel (reverse exps))))
              (write-code-vector cs out))
            (loop (cons e exps)))))))

(define (sly-load file)
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
    (call-with-output-file file
      (lambda (port)
        (write-code-vector cs port)))))

(define (compile-from-port in)
  (let loop ((exps '()))
    (let ((e (read in)))
      (if (eof-object? e)
          (compile-toplevel (reverse exps))
          (loop (cons e exps))))))

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
      (##generate-toplevel-code cs m)
      (instr cs 'RETURN)
      cs)))

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
          ((quasiquote)
           (##qq-expand (cadr exp)))
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
                (simplify
                 (list 'let (list (list var test))
                       (list 'if var var (simplify (cons 'or rest)))))))))))

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
	(simplify (cons 'begin body))
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
                                           (cons (reverse vars)
                                                 body))))
		 (cons name (reverse exps))))
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

;;;
;;; macro expansion
;;;

;; expands top-level expressions, doing macro
;; expansion, internal defines etc.
(define (##expand-code e)

  (define (expand-list e+ free user-env mac-env)
    (map (lambda (e) (expand e free user-env mac-env)) e+))

  (define (expand-body exps)

    (define (rest defs body)
      (cond
       ((any? define-exp? body)
        (error "internal defines must come first in body" exps))
       ((null? defs)
        (if (null? (cdr body))
            (car body)
            `(begin ,@body)))
       (else
        (let ((vars (map cadr defs))
              (exps (map caddr defs)))
          `(letrec ,(map list vars exps) ,@body)))))

    ;; gather internal defines
    (let loop ((defs '())
               (es exps))
      (if (null? es)
          (error "empty body" exps)
          (let ((e (car es)))
            (cond
             ((define-exp? e)
              (loop (cons e defs) (cdr es)))
             ((and (pair? e)
                   (eq? (car e) 'begin))
              (loop defs (append (cdr e) (cdr es))))
             (else
              (rest (reverse defs) es)))))))

  (define (expand e free user-env mac-env)
    (let ((env (if (memq (if (pair? e) (car e) e) free)
                   user-env
                   mac-env)))
      (cond
       ((syntactic-closure? e)
        (expand (syntactic-closure-exp e)
                (syntactic-closure-free e)
                user-env
                (syntactic-closure-env e)))
       ((symbol? e)
        (let ((pair (assv e env)))
          (if pair (cdr pair) e)))
       ((pair? e)
        (let ((op (car e)))
          (cond
           ((assv op env) =>
            (lambda (pair)
              (let ((expander (cdr pair)))
                (if (procedure? expander)
                    (expand (expander e user-env)
                            '()
                            user-env
                            scheme-syntactic-environment)
                    (expand-list e free user-env mac-env)))))
           ((eq? op 'quote) e)
           ((eq? op 'begin)
            (if (null? (cddr e))
                (expand (cadr e) free user-env mac-env)
                `(begin ,@(expand-list (cdr e) free user-env mac-env))))
           ((eq? op 'define)
            (let ((definee (cadr e))
                  (body (cddr e)))
              (cond
               ((symbol? definee)
                `(define ,definee
                   ,(expand (car body) free user-env mac-env)))
               ((pair? definee)
                (if (symbol-exp? definee)
                    (let ((name (car definee))
                          (args (cdr definee)))
                      `(define ,name
                         ,(expand `(lambda ,args ,@body) free user-env mac-env)))
                    (error "ill-formed 'define'" e)))
               (else
                (error "ill-formed 'define'" e)))))
           ((eq? op 'lambda)
            (let* ((vars (cadr e))
                   (vars-list (symbol-exp->list vars))
                   (new-env (map (lambda (n)
                                   (cons n (rename-var n)))
                                 vars-list))
                   (new-vars (symbol-exp->symbol-exp vars new-env))
                   (new-body (expand-list (cddr e) (append vars-list free)
                                          (append new-env user-env) mac-env)))
              `(lambda ,new-vars
                 ,(expand-body new-body))))
           ((memq op '(if set!))
            `(,(car e) ,@(expand-list (cdr e) free user-env mac-env)))
           (else
            (expand-list e free user-env mac-env)))))
       (else `',e))))

  (expand e '() scheme-syntactic-environment scheme-syntactic-environment))

(define (define-exp? e)
  (and (pair? e)
       (> (length e) 2)
       (eq? (car e) 'define)))

;;;
;;; core language compilation
;;;

;; compiles toplevel
(define (meaning-toplevel e+)
  (let* ((m+ (meaning-sequence e+ '() #t))
         (locals (##collect-locals m+))
         (sets (##collect-sets m+ locals))
         (env (##build-environment '() locals '() sets)))
    (##flag-boxes! m+ sets)
    (##make-toplevel-node env m+)))

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
             (meaning-quote (cadr e))
             (error "invalid quote expression")))
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
         (let ((op (car e)))
           (if (lambda-exp? op)
               (meaning-closed-application (cadr op) (cdr e) (caddr op) r tail?)
               (meaning-application op (cdr e) r tail?)))))
      (if (symbol? e)
          (meaning-reference e r)
          (meaning-quote e))))

(define (meaning-reference n r)
  (##make-reference-node n (lookup n r)))

(define (meaning-quote e)
  (##make-const-node e #f))

(define (meaning-sequence e+ r tail?)
  (if (null? (cdr e+))
      (meaning (car e+) r tail?)
      (##make-sequence-node (meaning (car e+) r #f)
                            (meaning-sequence (cdr e+) r tail?))))

(define (meaning-call/cc e r tail?)
  (if (lambda-exp? e)
      (if (one-symbol-list? (cadr e))
          (##make-call/cc-node (meaning e r tail?) tail?)
          (error "procedure must take one argument" e))
      (error "expression is not a procedure" e)))

(define (meaning-define n e)
  (meaning-assignment n e '()))

(define (meaning-alternative test then else r tail?)
  (##make-alternative-node (meaning test r #f)
                           (meaning then r tail?)
                           (if (eq? else *undef*)
                               (##make-undef-node)
                               (meaning else r tail?))))

(define (meaning-lambda n* e r)

  (define (parse n* regular)
    (cond
     ((null? n*)
      (meaning-internal (reverse regular) #f))
     ((symbol? n*)
      (meaning-internal (reverse regular) n*))
     ((pair? n*)
      (parse (cdr n*) (cons (car n*) regular)))
     (else
      (error "ill-formed 'lambda' arguments" n*))))

  (define (meaning-internal n* n)
    (let* ((an* (map (lambda (n)
                       (cons n (rename-var n)))
                     (if n (append n* (list n)) n*)))
           (r2 (cons an* r))
           (m (meaning e r2 #t))
           (bound (map cdr an*))
           (free (##collect-free m bound))
           (sets (##collect-sets m bound))
           (locals (##collect-locals m))
           (env (##build-environment bound locals free sets)))
      (##flag-boxes! m sets)
      (##make-lambda-node (length n*) (not n) env m)))

  (parse n* '()))

(define (meaning-assignment n e r)
  (let ((n2 (lookup n r))
        (prim? (assq n *primitives*)))
    (if (and prim? (not n2))
        (error "assignment to primitive not allowed" n)
        (##make-assign-node n n2 (meaning e r #f)))))

(define (meaning-application e e* r tail?)
  (let ((m  (meaning e r #f))
        (m* (meaning-args e* r)))
    (let ((prim? (and (##reference-node? m)
                      (assq (##reference-node-name m) *primitives*) #t)))
      (##make-apply-node m m* tail? prim?))))

(define (meaning-closed-application n* e* e r tail?)

  (define (parse n* regular)
    (cond
     ((null? n*)
      (meaning-internal (reverse regular) #f))
     ((symbol? n*)
      (meaning-internal (reverse regular) n*))
     ((pair? n*)
      (parse (cdr n*) (cons (car n*) regular)))
     (else
      (error "ill-formed 'lambda' arguments" n*))))

  (define (meaning-internal n* n)
    (let* ((an* (map (lambda (n)
                       (cons n (rename-var n)))
                     (if n (append n* (list n)) n*)))
           (r2 (cons an* r))
           (m (meaning e r2 tail?))
           (m* (meaning-args e* r))
           (locals (map cdr an*)))
      (##make-capply-node (length n*) (not n) locals m* m tail?)))

  (parse n* '()))

(define (meaning-args e* r)
  (if (null? e*)
      (##make-narg-node)
      (##make-args-node (meaning (car e*) r #f)
                        (meaning-args (cdr e*) r))))

(define *undef* (list 'undef))

(define (##immediate? x)
  (or (char? x)
      (boolean? x)
      (integer? x)
      (null? x)))

(define (lookup n r)
  (if (null? r)
      #f
      (let loop ((s (car r))
                 (r (cdr r)))
        (if (null? s)
            (if (null? r)
                #f
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
       (string-append "#:"
                      (symbol->string n)
                      "v"
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

(define (symbol-exp->list e)
  (let loop ((syms e)
             (ans '()))
    (cond
     ((null? syms)
      e)
     ((symbol? syms)
      (reverse (cons syms ans)))
     (else
      (loop (cdr syms)
            (cons (car syms) ans))))))

(define (symbol-exp->symbol-exp e env)
  (let loop ((syms e)
             (ans '()))
    (cond
     ((null? syms)
      (reverse ans))
     ((symbol? syms)
      (append (reverse ans)
              (let ((p (assv syms env)))
                (if p (cdr p) syms))))
     (else
      (let* ((sym (car syms))
             (new-sym (let ((p (assv sym env)))
                        (if p (cdr p) sym))))
        (loop (cdr syms)
              (cons new-sym ans)))))))

(define (one-symbol-list? e)
  (and (pair? e)
       (symbol? (car e))
       (null? (cdr e))))

(define (##collect-free m bound)
  (cond
   ((##reference-node? m)
    (let ((aname (##reference-node-alpha m)))
      (if (and aname (not (memq aname bound))) (list aname) '())))
   ((##sequence-node? m)
    (set-union (##collect-free (##sequence-node-first m) bound)
               (##collect-free (##sequence-node-rest  m) bound)))
   ((##call/cc-node? m)
    (##collect-free (##call/cc-node-lambda m) bound))
   ((##alternative-node? m)
    (set-union (##collect-free (##alternative-node-test m) bound)
               (set-union (##collect-free (##alternative-node-then m) bound)
                          (##collect-free (##alternative-node-else m) bound))))
   ((##lambda-node? m)
    (##collect-free (##lambda-node-body m)
                    (append (##collect-env-node-by-kind (##lambda-node-env m) 'bound) bound)))
   ((##assign-node? m)
    (set-union (let ((aname (##assign-node-alpha m)))
                 (if (and aname (not (memq aname bound))) (list aname) '()))
               (##collect-free (##assign-node-body m) bound)))
   ((##apply-node? m)
    (set-union (##collect-free (##apply-node-operator m) bound)
               (##collect-free (##apply-node-args m) bound)))
   ((##capply-node? m)
    (let ((new-bound (append (##capply-node-vars m) bound)))
      (set-union (##collect-free (##capply-node-args m) bound)
                 (##collect-free (##capply-node-body m) new-bound))))
   ((##args-node? m)
    (set-union (##collect-free (##args-node-first m) bound)
               (##collect-free (##args-node-rest  m) bound)))
   (else
    '())))

(define (##collect-sets m bound)
  (cond
   ((##sequence-node? m)
    (set-union (##collect-sets (##sequence-node-first m) bound)
               (##collect-sets (##sequence-node-rest  m) bound)))
   ((##call/cc-node? m)
    (##collect-sets (##call/cc-node-lambda m) bound))
   ((##alternative-node? m)
    (set-union (##collect-sets (##alternative-node-test m) bound)
               (set-union (##collect-sets (##alternative-node-then m) bound)
                          (##collect-sets (##alternative-node-else m) bound))))
   ((##lambda-node? m)
    (##collect-sets (##lambda-node-body m) bound))
   ((##assign-node? m)
    (set-union (let ((aname (##assign-node-alpha m)))
                 (if (and aname (memq aname bound)) (list aname) '()))
               (##collect-sets (##assign-node-body m) bound)))
   ((##apply-node? m)
    (set-union (##collect-sets (##apply-node-operator m) bound)
               (##collect-sets (##apply-node-args m) bound)))
   ((##capply-node? m)
    (set-union (##collect-sets (##capply-node-args m) bound)
               (##collect-sets (##capply-node-body m) bound)))
   ((##args-node? m)
    (set-union (##collect-sets (##args-node-first m) bound)
               (##collect-sets (##args-node-rest  m) bound)))
   (else
    '())))

(define (##collect-locals m)
  (cond
    ((##sequence-node? m)
     (append (##collect-locals (##sequence-node-first m))
             (##collect-locals (##sequence-node-rest  m))))
    ((##alternative-node? m)
     (append (##collect-locals (##alternative-node-test m))
             (##collect-locals (##alternative-node-then m))
             (##collect-locals (##alternative-node-else m))))
    ((##assign-node? m)
     (##collect-locals (##assign-node-body m)))
    ((##apply-node? m)
     (append (##collect-locals (##apply-node-operator m))
             (##collect-locals (##apply-node-args m))))
    ((##capply-node? m)
     (append (##capply-node-vars m)
             (##collect-locals (##capply-node-body m))
             (##collect-locals (##capply-node-args m))))
    ((##args-node? m)
     (append (##collect-locals (##args-node-first m))
             (##collect-locals (##args-node-rest  m))))
    (else
     '())))

;; flags assigned variables as boxes
(define (##flag-boxes! m sets)

  ;; flag free variables of lambda as boxes if assigned
  (define (lambda-handler visitor m sets)
    (let loop ((env (##lambda-node-env m)))
      (if (not (null? env))
          (let ((entry (car env)))
            (if (memq (car entry) sets)
                (##env-node-boxed?-set! (cdr entry) #t))
            (loop (cdr env))))))

  (##visit (list (cons '##lambda lambda-handler)) m sets))

(define (##build-environment bound locals free sets)
  (let ((bound-env (let lp ((i 0)
                            (bound bound)
                            (res '()))
                     (if (null? bound)
                         res
                         (let* ((b (car bound))
                                (boxed? (and (memq b sets) #t)))
                           (lp (+ i 1)
                               (cdr bound)
                               (cons (cons b
                                      (##make-env-node b 'bound i boxed?))
                                     res))))))
        (free-env (let lp ((i 0)
                           (free free)
                           (res '()))
                    (if (null? free)
                        res
                        (let ((f (car free)))
                          (lp (+ i 1)
                              (cdr free)
                              (cons (cons f
                                          (##make-env-node f 'free i #f))
                                    res))))))
        (local-env (map (lambda (l)
                          (cons l (##make-env-node l 'local 0 (and (memq l sets) #t))))
                        locals)))
    (append bound-env free-env local-env)))

(define (##collect-env-node-by-kind env kind)
  (let loop ((env env)
             (res '()))
    (if (null? env)
        (reverse res)
        (let ((entry (car env)))
          (if (eqv? (##env-node-kind (cdr entry)) kind)
              (loop (cdr env) (cons (car entry) res))
              (loop (cdr env) res))))))

;; this pass generates the code for the virtual machine
(define (##generate-toplevel-code cs m)
  (let ((env (##toplevel-node-env m))
        (body (##toplevel-node-body m)))

    ;; install constants into compiler state
    (##collect-constants! cs body)

    ;; emit code to initialise constants before
    ;; entering real code
    (let loop ((i 0)
               (consts (vector-ref cs 1)))
      (if (not (null? consts))
          (let ((const (car consts)))
            (##emit-constant cs const)
            (instr1 cs 'CONST-INIT i)
            (loop (+ i 1) (cdr consts)))))

    ;; generate rest of code
    (##generate-code cs body env 0)))

(define (##collect-constants! cs m)

  (define (const-handler visitor m)
    (let ((value (##const-node-value m)))
      (if (not (##immediate? value))
          (let ((index (##install-constant! cs value)))
            (##const-node-index-set! m index)))))

  (##visit (list (cons '##const const-handler)) m))

(define (##generate-code cs m env si)
  (cond
    ((##const-node? m)
     (##generate-constant cs m))
    ((##reference-node? m)
     (##generate-reference cs m env si #t))
    ((##sequence-node? m)
     (##generate-sequence cs m env si))
    ((##call/cc-node? m)
     (##generate-call/cc cs m env si))
    ((##alternative-node? m)
     (##generate-alternative cs m env si))
    ((##lambda-node? m)
     (##generate-lambda cs m env si))
    ((##assign-node? m)
     (##generate-assignment cs m env si))
    ((##apply-node? m)
     (generate-apply cs m env si))
    ((##capply-node? m)
     (##generate-capply cs m env si))
    (else
     (error "unknown AST node" m))))

(define (##generate-constant cs m)
  (let ((value (##const-node-value m)))
    (if (##immediate? value)
        (##emit-constant cs value)
        (instr1 cs 'CONST (##const-node-index m)))))

(define (##generate-reference cs m env si unbox?)
  (let ((alpha (##reference-node-alpha m)))
    (if alpha
        (let ((rec (assv alpha env)))
          (if rec
              (let* ((env (cdr rec))
                     (kind (##env-node-kind env))
                     (pos (##env-node-position env))
                     (boxed? (##env-node-boxed? env)))
                (case kind
                  ((bound local)
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
                (and unbox? boxed? (instr cs 'OPEN-BOX)))
              (error "reference not found")))
        (let* ((var (##reference-node-name m))
               (index (install-global! cs var)))
          (if (memq var *defined-globals*)
              (instr1 cs 'GLOBAL-REF index)
              (instr1 cs 'CHECKED-GLOBAL-REF index))))))

(define (generate-sequence cs m env si)
  (##generate-code cs (##sequence-node-first m) env si)
  (##generate-code cs (##sequence-node-rest  m) env si))

(define (##generate-call/cc cs m env si)
  
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
        (tail? (##call/cc-node-tail? m)))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (instr cs 'SAVE-CONT)
    (instr cs 'PUSH)
    (generate-continuation-closure cs)
    (instr cs 'PUSH)
    ;; calling closure given to call/cc with
    ;; continuation-restoring closure as sole argument
    (##generate-lambda cs (##call/cc-node-lambda m) env (+ si 1))
    (instr1 cs (if tail? 'TAIL-CALL 'CALL) 1)
    ;; back-patching return address
    (or tail? (patch-instr! cs i (code-size cs)))))

(define (##generate-alternative cs m env si)
  (let ((testc (##alternative-node-test m))
        (thenc (##alternative-node-then m))
        (elsec (##alternative-node-else m)))
    (##generate-code cs testc env si)
    (let ((i (code-size cs)))
      ;; this will be back-patched later
      (instr1 cs 'JMP-IF-NOT 0)
      (##generate-code cs thenc env si)
      (if (##undef-node? elsec)
          (let ((j (code-size cs)))
            (patch-instr! cs i (- j i 1)))
          (let ((j (code-size cs)))
            ;; this will be back-patched later
            (instr1 cs 'JMP 0)
            (let ((k (code-size cs)))
              (patch-instr! cs i (- k i 1))
              (##generate-code cs elsec env si)
              (let ((m (code-size cs)))
                (patch-instr! cs j (- m j 1)))))))))

(define (##generate-lambda cs m env si)
  (let ((arity (##lambda-node-arity m))
        (regular? (##lambda-node-regular? m))
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
              (generate-code cs (vector-ref m 5) (length bound))
              (instr cs 'RETURN)
              (let ((j (code-size cs)))
                ;; back-patching jump over closure code
                (patch-instr! cs i (- j i 1)))))
          (let ((ref (car f)))
            (##generate-reference cs ref si #f)
            (instr cs 'PUSH)
            (loop (cdr f)))))))

(define (##generate-assignment cs m env si)
  (##generate-code cs (##assign-node-body m) env si)
  (let ((alpha (##reference-node-alpha m)))
    (if alpha
        (let ((rec (assv alpha env)))
          (if rec
              (let* ((env (cdr rec))
                     (kind (##env-node-kind env))
                     (pos (##env-node-position env)))
                (case kind
                  ((bound local)
                   (instr1 cs 'ASSIGN pos))
                  ((free)
                   (instr1 cs 'ASSIGN-FREE pos))
                  (else
                   (error "unknown binding type" m))))
              (error "reference not found")))
        (let* ((var (##reference-node-name m))
               (index (install-global! cs var)))
          (if (memq var *defined-globals*)
              (instr1 cs 'GLOBAL-SET index)
              (instr1 cs 'CHECKED-GLOBAL-SET index))))))

(define (generate-apply cs m si)
  (if (memq 'prim (vector-ref m 1))
      (generate-primitive-apply cs m si)
      (generate-common-apply cs m si)))

(define (generate-primitive-apply cs m si)
  (let* ((ref (vector-ref m 2))
         (args (vector-ref m 3))
         (nargs (number-of-arguments args))
         (prim (vector-ref ref 1))
         (prim-rec (assv prim *primitives*)))
    (if prim-rec
        (let ((code (cadr prim-rec))
              (arity (caddr prim-rec)))
          (if (= nargs arity)
              (case arity
                ((0)
                 (instr cs code))
                ((1)
                 (generate-code cs (vector-ref args 1) si)
                 (instr cs code))
                ((2)
                 (generate-code cs (vector-ref args 1) si)
                 (instr cs 'PUSH)
                 (generate-code cs (vector-ref (vector-ref args 2) 1) si)
                 (instr cs code))
                (else
                 (error "Primitive with unknown arity")))
              (error "Primitive called with wrong arity!" prim)))
        (error "Unknown primitive"))))

(define (generate-common-apply cs m si)
  (let ((i (code-size cs))
        (tail? (memq 'tail (vector-ref m 1))))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (let ((len (generate-push-arguments cs (vector-ref m 3) si)))
      (generate-code cs (vector-ref m 2) si)
      (instr1 cs (if tail? 'TAIL-CALL 'CALL) len)
      ;; back-patching return address
      ;; this not a position-independent value
      (or tail? (patch-instr! cs i (code-size cs))))))

(define (generate-closed-apply cs m si)
  (let ((tail? (memq 'tail (vector-ref m 1)))
        (sets (vector-ref m 3))
        (len (generate-push-arguments cs (vector-ref m 5) si)))
    (make-local-boxes cs (vector-ref m 4) sets si)
    (generate-code cs (vector-ref m 6) si)
    (or tail? (instr1 cs 'POP len))))

(define (generate-push-arguments cs m si)
  (let loop ((len 0)
             (m m))
    (let ((kind (vector-ref m 0)))
      (if (eq? kind 'arg-null)
          len
          (let ((arg (vector-ref m 1)))
            (generate-code cs arg si)
            (instr cs 'PUSH)
            (loop (+ len 1) (vector-ref m 2)))))))

;; Create instructions to box arguments to closure
;; that are assigned somewhere in the code
(define (make-boxes cs vars sets)
  (for-each (lambda (v)
	      (and (memq v sets)
		   (instr1 cs 'INSERT-BOX (index-of v vars))))
	    vars))

(define (make-local-boxes cs locals sets si)
  (for-each (lambda (l)
              (and (memq (vector-ref l 2) sets)
                   (instr1 cs
                           'INSERT-BOX
                           (+ si (cdr (vector-ref l 3))))))
            locals))

(define (number-of-arguments m)
  (let loop ((len 0)
             (m m))
    (let ((kind (vector-ref m 0)))
      (if (eq? kind 'arg-null)
          len
          (loop (+ len 1) (vector-ref m 2))))))

;; procs is an alist of (tag . proc)
;; the procedure gets the node and the visitor
;; to apply it again
(define (##visit procs m . args)

  (define (visitor m)
    (let* ((tag (vector-ref m 0))
           (tag-proc (assq tag procs)))
      (if tag-proc
          (apply (cdr tag-proc) visitor m args)
          (case tag
            ((##sequence)
             (visitor (##sequence-node-first m))
             (visitor (##sequence-node-rest  m)))
            ((##call/cc)
             (visitor (##call/cc-node-lambda m)))
            ((##alternative)
             (visitor (##alternative-node-test m))
             (visitor (##alternative-node-then m))
             (visitor (##alternative-node-else m)))
            ((##lambda)
             (visitor (##lambda-node-body m)))
            ((##assign)
             (visitor (##assign-node-body m)))
            ((##apply)
             (visitor (##apply-node-operator m))
             (visitor (##apply-node-args m)))
            ((##capply)
             (visitor (##capply-node-args m))
             (visitor (##capply-node-body m)))
            ((##args)
             (visitor (##args-node-first m))
             (visitor (##args-node-rest  m)))))))

  (visitor m))

;;
;; AST nodes
;;

(define (##make-undef-node)
  (vector '##undef))

(define (##undef-node? n)
  (##node-type? n '##undef))

(define (##make-const-node v i)
  (vector '##const v i))

(define (##const-node? n)
  (##node-type? n '##const))

(define (##const-node-value n)
  (if (##const-node? n)
      (vector-ref n 1)
      (error "not a const node")))

(define (##const-node-value-set! n a)
  (if (##const-node? n)
      (vector-set! n 1 a)
      (error "not a const node")))

(define (##const-node-index n)
  (if (##const-node? n)
      (vector-ref n 2)
      (error "not a const node")))

(define (##const-node-index-set! n a)
  (if (##const-node? n)
      (vector-set! n 2 a)
      (error "not a const node")))

(define (##make-reference-node n an)
  (vector '##reference n an))

(define (##reference-node? n)
  (##node-type? n '##reference))

(define (##reference-node-name n)
  (if (##reference-node? n)
      (vector-ref n 1)
      (error "not a reference node")))

(define (##reference-node-name-set! n a)
  (if (##reference-node? n)
      (vector-set! n 1 a)
      (error "not a reference node")))

(define (##reference-node-alpha n)
  (if (##reference-node? n)
      (vector-ref n 2)
      (error "not a reference node")))

(define (##reference-node-alpha-set! n a)
  (if (##reference-node? n)
      (vector-set! n 2 a)
      (error "not a reference node")))

(define (##make-sequence-node f r)
  (vector '##sequence f r))

(define (##sequence-node? n)
  (##node-type? n '##sequence))

(define (##sequence-node-first n)
  (if (##sequence-node? n)
      (vector-ref n 1)
      (error "not a sequence node")))

(define (##sequence-node-first-set! n a)
  (if (##sequence-node? n)
      (vector-set! n 1 a)
      (error "not a sequence node")))

(define (##sequence-node-rest n)
  (if (##sequence-node? n)
      (vector-ref n 2)
      (error "not a sequence node")))

(define (##sequence-node-rest-set! n a)
  (if (##sequence-node? n)
      (vector-set! n 2 a)
      (error "not a sequence node")))

(define (##make-call/cc-node l t)
  (vector '##call/cc l t))

(define (##call/cc-node? n)
  (##node-type? n '##call/cc))

(define (##call/cc-node-lambda n)
  (if (##call/cc-node? n)
      (vector-ref n 1)
      (error "not a call/cc node")))

(define (##call/cc-node-lambda-set! n a)
  (if (##call/cc-node? n)
      (vector-set! n 1 a)
      (error "not a call/cc node")))

(define (##call/cc-node-tail? n)
  (if (##call/cc-node? n)
      (vector-ref n 2)
      (error "not a call/cc node")))

(define (##call/cc-node-tail?-set! n a)
  (if (##call/cc-node? n)
      (vector-set! n 2 a)
      (error "not a call/cc node")))

(define (##make-alternative-node t y n)
  (vector '##alternative t y n))

(define (##alternative-node? n)
  (##node-type? n '##alternative))

(define (##alternative-node-test n)
  (if (##alternative-node? n)
      (vector-ref n 1)
      (error "not an alternative node")))

(define (##alternative-node-test-set! n a)
  (if (##alternative-node? n)
      (vector-set! n 1 a)
      (error "not an alternative node")))

(define (##alternative-node-then n)
  (if (##alternative-node? n)
      (vector-ref n 2)
      (error "not an alternative node")))

(define (##alternative-node-then-set! n a)
  (if (##alternative-node? n)
      (vector-set! n 2 a)
      (error "not an alternative node")))

(define (##alternative-node-else n)
  (if (##alternative-node? n)
      (vector-ref n 3)
      (error "not an alternative node")))

(define (##alternative-node-else-set! n a)
  (if (##alternative-node? n)
      (vector-set! n 3 a)
      (error "not an alternative node")))

(define (##make-env-node n k p b)
  (vector '##env n k p b))

(define (##env-node? n)
  (##node-type? n '##env))

(define (##env-node-name n)
  (if (##env-node? n)
      (vector-ref n 1)
      (error "not an environment node")))

(define (##env-node-name-set! n a)
  (if (##env-node? n)
      (vector-set! n 1 a)
      (error "not an environment node")))

(define (##env-node-kind n)
  (if (##env-node? n)
      (vector-ref n 2)
      (error "not an environment node")))

(define (##env-node-kind-set! n a)
  (if (##env-node? n)
      (vector-set! n 2 a)
      (error "not an environment node")))

(define (##env-node-position n)
  (if (##env-node? n)
      (vector-ref n 3)
      (error "not an environment node")))

(define (##env-node-position-set! n a)
  (if (##env-node? n)
      (vector-set! n 3 a)
      (error "not an environment node")))

(define (##env-node-boxed? n)
  (if (##env-node? n)
      (vector-ref n 4)
      (error "not an environment node")))

(define (##env-node-boxed?-set! n a)
  (if (##env-node? n)
      (vector-set! n 4 a)
      (error "not an environment node")))

(define (##make-lambda-node a r e bd)
  (vector '##lambda a r e bd))

(define (##lambda-node? n)
  (##node-type? n '##lambda))

(define (##lambda-node-arity n)
  (if (##lambda-node? n)
      (vector-ref n 1)
      (error "not a lambda node")))

(define (##lambda-node-arity-set! n a)
  (if (##lambda-node? n)
      (vector-set! n 1 a)
      (error "not a lambda node")))

(define (##lambda-node-regular? n)
  (if (##lambda-node? n)
      (vector-ref n 2)
      (error "not a lambda node")))

(define (##lambda-node-regular?-set! n a)
  (if (##lambda-node? n)
      (vector-set! n 2 a)
      (error "not a lambda node")))

(define (##lambda-node-env n)
  (if (##lambda-node? n)
      (vector-ref n 3)
      (error "not a lambda node")))

(define (##lambda-node-env-set! n a)
  (if (##lambda-node? n)
      (vector-set! n 3 a)
      (error "not a lambda node")))

(define (##lambda-node-body n)
  (if (##lambda-node? n)
      (vector-ref n 4)
      (error "not a lambda node")))

(define (##lambda-node-body-set! n a)
  (if (##lambda-node? n)
      (vector-set! n 4 a)
      (error "not a lambda node")))

(define (##make-toplevel-node e b)
  (vector '##toplevel e b))

(define (##toplevel-node? n)
  (##node-type? n '##toplevel))

(define (##toplevel-node-env n)
  (if (##toplevel-node? n)
      (vector-ref n 1)
      (error "not a toplevel node")))

(define (##toplevel-node-env-set! n a)
  (if (##toplevel-node? n)
      (vector-set! n 1 a)
      (error "not a toplevel node")))

(define (##toplevel-node-body n)
  (if (##toplevel-node? n)
      (vector-ref n 2)
      (error "not a toplevel node")))

(define (##toplevel-node-body-set! n a)
  (if (##toplevel-node? n)
      (vector-set! n 2 a)
      (error "not a toplevel node")))

(define (##make-assign-node n a b)
  (vector '##assign n a b))

(define (##assign-node? n)
  (##node-type? n '##assign))

(define (##assign-node-name n)
  (if (##assign-node? n)
      (vector-ref n 1)
      (error "not an assign node")))

(define (##assign-node-name-set! n a)
  (if (##assign-node? n)
      (vector-set! n 1 a)
      (error "not an assign node")))

(define (##assign-node-alpha n)
  (if (##assign-node? n)
      (vector-ref n 2)
      (error "not an assign node")))

(define (##assign-node-alpha-set! n a)
  (if (##assign-node? n)
      (vector-set! n 2 a)
      (error "not an assign node")))

(define (##assign-node-body n)
  (if (##assign-node? n)
      (vector-ref n 3)
      (error "not an assign node")))

(define (##assign-node-body-set! n a)
  (if (##assign-node? n)
      (vector-set! n 3 a)
      (error "not an assign node")))

(define (##make-apply-node o a t p)
  (vector '##apply o a t p))

(define (##apply-node? n)
  (##node-type? n '##apply))

(define (##apply-node-operator n)
  (if (##apply-node? n)
      (vector-ref n 1)
      (error "not an apply node")))

(define (##apply-node-operator-set! n a)
  (if (##apply-node? n)
      (vector-set! n 1 a)
      (error "not an apply node")))

(define (##apply-node-args n)
  (if (##apply-node? n)
      (vector-ref n 2)
      (error "not an apply node")))

(define (##apply-node-args-set! n a)
  (if (##apply-node? n)
      (vector-set! n 2 a)
      (error "not an apply node")))

(define (##apply-node-tail? n)
  (if (##apply-node? n)
      (vector-ref n 3)
      (error "not an apply node")))

(define (##apply-node-tail?-set! n a)
  (if (##apply-node? n)
      (vector-set! n 3 a)
      (error "not an apply node")))

(define (##apply-node-prim? n)
  (if (##apply-node? n)
      (vector-ref n 4)
      (error "not an apply node")))

(define (##apply-node-prim?-set! n a)
  (if (##apply-node? n)
      (vector-set! n 4 a)
      (error "not an apply node")))

(define (##make-capply-node a r v ag b t)
  (vector '##capply a r v ag b t))

(define (##capply-node? n)
  (##node-type? n '##capply))

(define (##capply-node-arity n)
  (if (##capply-node? n)
      (vector-ref n 1)
      (error "not a capply node")))

(define (##capply-node-arity-set! n a)
  (if (##capply-node? n)
      (vector-set! n 1 a)
      (error "not a capply node")))

(define (##capply-node-regular? n)
  (if (##capply-node? n)
      (vector-ref n 2)
      (error "not a capply node")))

(define (##capply-node-regular?-set! n a)
  (if (##capply-node? n)
      (vector-set! n 2 a)
      (error "not a capply node")))

(define (##capply-node-vars n)
  (if (##capply-node? n)
      (vector-ref n 3)
      (error "not a capply node")))

(define (##capply-node-vars-set! n a)
  (if (##capply-node? n)
      (vector-set! n 3 a)
      (error "not a capply node")))

(define (##capply-node-args n)
  (if (##capply-node? n)
      (vector-ref n 4)
      (error "not a capply node")))

(define (##capply-node-args-set! n a)
  (if (##capply-node? n)
      (vector-set! n 4 a)
      (error "not a capply node")))

(define (##capply-node-body n)
  (if (##capply-node? n)
      (vector-ref n 5)
      (error "not a capply node")))

(define (##capply-node-body-set! n a)
  (if (##capply-node? n)
      (vector-set! n 5 a)
      (error "not a capply node")))

(define (##capply-node-tail? n)
  (if (##capply-node? n)
      (vector-ref n 6)
      (error "not a capply node")))

(define (##capply-node-tail?-set! n a)
  (if (##capply-node? n)
      (vector-set! n 6 a)
      (error "not a capply node")))

(define (##make-args-node f r)
  (vector '##args f r))

(define (##args-node? n)
  (##node-type? n '##args))

(define (##args-node-first n)
  (if (##args-node? n)
      (vector-ref n 1)
      (error "not an args node")))

(define (##args-node-first-set! n a)
  (if (##args-node? n)
      (vector-set! n 1 a)
      (error "not an args node")))

(define (##args-node-rest n)
  (if (##args-node? n)
      (vector-ref n 2)
      (error "not an args node")))

(define (##args-node-rest-set! n a)
  (if (##args-node? n)
      (vector-set! n 2 a)
      (error "not an args node")))

(define (##make-narg-node)
  (vector '##null-arg))

(define (##narg-node? n)
  (##node-type? n '##null-arg))

(define (##node-type? n t)
  (and (vector? n)
       (eqv? t (vector-ref n 0))))

;;
;; low-level code generation
;;

;; bytecode instructions
(define *opcodes*
  ;; basic VM instructions
  '((LOAD-NIL           . 1)
    (LOAD-FALSE         . 2)
    (LOAD-TRUE          . 3)
    (LOAD-UNDEF         . 4)
    (LOAD-ZERO          . 5)
    (LOAD-ONE           . 6)
    (PUSH               . 7)
    (LOAD0              . 8)
    (LOAD1              . 9)
    (LOAD2              . 10)
    (LOAD3              . 11)
    (RETURN             . 12)
    (SAVE-CONT          . 13)
    (REST-CONT          . 14)
    (BOX                . 15)
    (OPEN-BOX           . 16)
    (HALT               . 17)
    (ABORT              . 18)

    ;; type predicates
    (NULL?              . 40)
    (BOOL?              . 41)
    (CHAR?              . 42)
    (FIXNUM?            . 43)
    (PAIR?              . 44)
    (SYMBOL?            . 45)

    ;; primitives optimised as instructions
    (INC                . 60)
    (DEC                . 61)
    (FIXNUM->CHAR       . 62)
    (CHAR->FIXNUM       . 63)
    (ZERO?              . 64)
    (NOT                . 65)
    (PLUS               . 66)
    (MINUS              . 67)
    (MULT               . 68)
    (CONS               . 69)
    (CAR                . 70)
    (CDR                . 71)
    (NUM-EQ             . 72)
    (EQ?                . 73)
    (EQV?               . 74)
    (MAKE-STRING        . 75)
    (STRING-SET         . 76)
    (STRING->SYMBOL     . 77)
    (MAKE-VECTOR        . 78)
    (VECTOR-SET         . 79)
    (DEBUG              . 80)

    ;; instructions that take an operand
    (LOAD-FIXNUM        . 120)
    (LOAD-CHAR          . 121)
    (LOAD               . 122)
    (MAKE-CLOSURE       . 123)
    (JMP-IF-NOT         . 124)
    (JMP                . 125)
    (LOAD-FREE          . 126)
    (ASSIGN             . 127)
    (ASSIGN-FREE        . 128)
    (FRAME              . 129)
    (LOAD-LOCAL         . 130)
    (INSERT-BOX         . 131)
    (ASSIGN-LOCAL       . 132)
    (POP                . 133)
    (GLOBAL-REF         . 134)
    (CHECKED-GLOBAL-REF . 135)
    (GLOBAL-SET         . 136)
    (CHECKED-GLOBAL-SET . 137)
    (CONST              . 138)
    (CONST-INIT         . 139)
    (ARITY=             . 140)
    (ARITY>=            . 141)
    (LISTIFY            . 142)
    (CALL               . 143)
    (TAIL-CALL          . 144)))

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

(define (##install-constant! cs val)
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

(define (##emit-constant cs c)
  (cond
   ((##immediate? c)
    (emit-immediate cs c))
   ((pair? c)
    (emit-pair cs c))
   ((string? c)
    (emit-string cs c))
   ((symbol? c)
    (emit-string cs (symbol->string c))
    (instr cs 'STRING->SYMBOL))
   ((vector? c)
    (emit-vector cs c))
   (else
    (error "unimplemented complex constant" c))))

(define (emit-pair cs p)
  (##emit-constant cs (car p))
  (instr cs 'PUSH)
  (##emit-constant cs (cdr p))
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
            (##emit-constant cs (vector-ref vec i))
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

(define (write-code-vector cs port)
  (display "#( " port)
  (let ((globals (vector-ref cs 0))
        (consts (vector-ref cs 1))
        (code (vector-ref cs 3))
	(code-size (code-size cs)))
    
    ;; globals
    (write-fixnum (length globals) port)
    (for-each (lambda (g)
                (write-global g port))
              globals)

    ;; constants
    (write-fixnum (length consts) port)

    ;; code
    (write-fixnum code-size port)
    (let loop ((i 0))
      (if (= i code-size)
	  (display ")" port)
	  (let ((instr (vector-ref code i)))
	    (let ((op   (vector-ref instr 0))
		  (arg1 (vector-ref instr 1)))
	      (display (cdr (assv op *opcodes*)) port)
	      (display " " port)
	      (and arg1 (write-fixnum arg1 port)))
	    (loop (+ i 1)))))))

(define (write-global sym port)
  (let* ((str (symbol->string sym))
         (len (string-length str)))
    (write-fixnum len port)
    (let loop ((i 0))
      (if (< i len)
          (begin
            (write-fixnum (char->integer (string-ref str i)) port)
            (loop (+ i 1)))))))

(define (write-fixnum x port)
  (let* (;(b4 (quotient  x  16777216))
         ;(x4 (remainder x  16777216))
         (b3 (quotient  x  65536))
         (x3 (remainder x  65536))
         (b2 (quotient  x3 256))
         (b1 (remainder x3 256)))
    (display b1 port)
    (display " " port)
    (display b2 port)
    (display " " port)
    (display b3 port)
    (display " " port)
    (display 0 port)
    (display " " port)))

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
    (char->integer CHAR->FIXNUM 1)
    (integer->char FIXNUM->CHAR 1)
    (null? NULL? 1)
    (zero? ZERO? 1)
    (not NOT 1)
    (boolean? BOOL? 1)
    (char? CHAR? 1)
    (integer? FIXNUM? 1)
    (* MULT 2)
    (cons CONS 2)
    (car CAR 1)
    (cdr CDR 1)
    (= NUM-EQ 2)
    (eq? EQ? 2)
    (eqv? EQV? 2)
    (string->symbol STRING->SYMBOL 1)
    (pair? PAIR? 1)
    (symbol? SYMBOL? 1)
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

