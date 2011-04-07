;;;
;;; The Sly Scheme compiler
;;; Copyright 2009-2011 Alex Queiroz <asandroq@gmail.com>
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
  (let* ((t+ (map (lambda (e)
                    (##purify-letrecs (##expand-code e)))
                  e+))
         (def-exps (filter define-exp? t+))
         (defs (map cadr def-exps))
         (cs (make-compiler-state)))
    (set! *defined-globals*
          (append defs *defined-globals*))
    (let  ((m (meaning-toplevel t+)))
      (generate-toplevel-code cs m)
      (instr cs 'RETURN)
      cs)))

;; already seen globals
(define *defined-globals* '())

;;;
;;; macro expansion
;;;

;; an identifier is the meaning of a variable
(define (##make-identifier name)
  (vector 'ident name #f #f))

(define (##identifier? id)
  (and (vector? id)
       (eqv? (vector-ref id 0) 'ident)))

(define (##identifier-name id)
  (vector-ref id 1))

(define (##identifier-referenced? id)
  (vector-ref id 2))

(define (##identifier-referenced-set! id bool)
  (vector-set! id 2 bool))

(define (##identifier-assigned? id)
  (vector-ref id 3))

(define (##identifier-assigned-set! id bool)
  (vector-set! id 3 bool))

;; expands top-level expressions, doing macro
;; expansion, internal defines etc. as per:
;;
;; Alan Bawden and Jonathan Rees. Syntactic closures. 1988 ACM Conference
;; On Lisp and Functional Programming, pages 86-95
;;
(define (##expand-code e)

  (define (expand-list e+ free user-env mac-env)
    (map (lambda (e) (expand e free user-env mac-env)) e+))

  (define (expand-body exps)

    ;; ugly letrec post renaming
    (define (rename-letrec e env)

      (define (rename e)
        (if (pair? e)
            (case (car e)
              ((quote)
               e)
              ((lambda)
               `(lambda ,(cadr e)
                  ,(rename (caddr e))))
              ((letrec letrec*)
               (let ((vars (map car (cadr e)))
                     (exps (map rename (map cadr (cadr e)))))
                 `(,(car e) ,(map list vars exps)
                    ,(rename (caddr e)))))
              ((set!)
               (let ((assignee (cadr e))
                     (assigned (rename (caddr e))))
                 (if (symbol? assignee)
                     (let ((p (assv assignee env)))
                       (if p
                           (let ((id (cdr p)))
                             (##identifier-assigned-set! id #t)
                             `(set! ,id ,assigned))
                           `(set! ,assignee ,assigned)))
                     `(set! ,assignee ,assigned))))
              (else
               (map rename e)))
            (if (symbol? e)
                (let ((p (assv e env)))
                  (if p
                      (let ((id (cdr p)))
                        (##identifier-referenced-set! id #t)
                        id)
                      e))
                e)))

      (rename e))

    (define (rest defs body)
      (cond
       ((any? define-exp? body)
        (error "internal defines must come first in body" exps))
       ((null? defs)
        (if (null? (cdr body))
            (car body)
            `(begin ,@body)))
       (else
        (let* ((vars (map cadr defs))
               (exps (map caddr defs))
               (env (##make-env vars))
               (new-vars (map cdr env)))
          (rename-letrec `(letrec* ,(map list new-vars exps)
                            ,(if (null? (cdr body))
                                 (car body)
                                 `(begin ,@body)))
                         env)))))

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
       ((##identifier? e) e)
       ((syntactic-closure? e)
        (expand (syntactic-closure-exp e)
                (syntactic-closure-free e)
                user-env
                (syntactic-closure-env e)))
       ((symbol? e)
        (let ((pair (assv e env)))
          (if pair
              (let ((id (cdr pair)))
                (##identifier-referenced-set! id #t)
                id)
              e)))
       ((pair? e)
        (let ((op (car e)))
          (cond
           ((assv op env) =>
            (lambda (pair)
              (let ((expander (cdr pair)))
                (if (procedure? expander)
                    (expand (expander e user-env mac-env)
                            free
                            user-env
                            mac-env)
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
           ((eq? op 'if)
            `(if ,@(expand-list (cdr e) free user-env mac-env)))
           ((eq? op 'lambda)
            (let* ((vars (cadr e))
                   (vars-list (symbol-exp->list vars))
                   (new-env (##make-env vars-list))
                   (new-vars (symbol-exp->symbol-exp vars new-env))
                   (new-body (expand-list (cddr e) (append vars-list free)
                                          (append new-env user-env) mac-env)))
              `(lambda ,new-vars
                 ,(expand-body new-body))))
           ((or (eq? op 'letrec)
                (eq? op 'letrec*))
            (let* ((vars (map car (cadr e)))
                   (new-env (##make-env vars))
                   (new-vars (map cdr new-env))
                   (new-free (append vars free))
                   (new-user-env (append new-env user-env))
                   (new-args (expand-list (map cadr (cadr e)) new-free
                                          new-user-env mac-env))
                   (new-body (expand-list (cddr e) new-free
                                          new-user-env mac-env)))
              `(,op ,(map list new-vars new-args)
                 ,(expand-body new-body))))
           ((eq? op 'set!)
            (if (and (not (null? (cdr e)))
                     (not (null? (cddr e)))
                     (null? (cdddr e))
                     (symbol? (cadr e)))
                (let* ((assignee (cadr e))
                       (assigned (expand (caddr e) free user-env mac-env))
                       (env (if (memv assignee free) user-env mac-env))
                       (pair (assv assignee env)))
                  (if pair
                      (let ((id (cdr pair)))
                        (##identifier-assigned-set! id #t)
                        `(set! ,id ,assigned))
                      `(set! ,assignee ,assigned)))
                (error "Ill-formed set!" e)))
           (else
            (expand-list e free user-env mac-env)))))
       (else `',e))))

  (if (and (pair? e)
           (eqv? (car e) 'define-syntax))
      ;; toplevel syntax definition
      (if (and (pair? (cdr e))
               (pair? (cddr e)))
          (let ((name (cadr e))
                (expander (expand (caddr e)
                                  '()
                                  ##user-syntactic-environment
                                  ##user-syntactic-environment)))
            (if (symbol? name)
                `(begin
                   (set! ##user-syntactic-environment
                         (cons (cons ',name ,expander)
                               ##user-syntactic-environment))
                   ',name)
                (error "Ill-formed define-syntax")))
          (error "Ill-formed define-syntax" e))
      (expand e '() ##user-syntactic-environment ##user-syntactic-environment)))

(define (##make-env vars)
  (map (lambda (n)
         (cons n (##make-identifier n)))
       vars))

(define (define-exp? e)
  (and (pair? e)
       (> (length e) 2)
       (eq? (car e) 'define)))

;;;
;;; letrec transformation, as per:
;;;
;;; Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig.
;;; Fixing letrec: A faithful yet efficient implementation of Scheme’s
;;; recursive binding construct. Higher Order Symbol. Comput.,
;;; 18(3-4):299–326, 2005.
;;;

(define (##fix-letrecs e)

  (define (partition-bindings bindings body old-vars cont)

    (define (assimilate-let b v unref lambdas simple complex)
      (if (and (null? unref)
               (null? complex))
          (cons simple lambdas)
          #f))

    (let* ((new-body (walk-exp body))
           (lambda-body? (##lambda-exp? new-body))
           (new-vars (map car bindings))
           (all-vars (append old-vars new-vars)))
      (let classify ((unref '())
                     (lambdas '())
                     (simple '())
                     (complex '())
                     (bindings bindings))
        (if (null? bindings)
            (cont new-body
                  new-vars
                  (reverse unref)
                  (reverse lambdas)
                  (reverse simple)
                  (reverse complex))
            (let* ((binding (car bindings))
                   (var (car binding))
                   (exp (walk-exp (cadr binding))))
              (cond
               ((not (##identifier-referenced? var))
                (classify (cons (cons var exp) unref)
                          lambdas
                          simple
                          complex
                          (cdr bindings)))
               ((and (not (##identifier-assigned? var))
                     (##lambda-exp? exp))
                (classify unref
                          (cons (list var exp) lambdas)
                          simple
                          complex
                          (cdr bindings)))
               ((and (or lambda-body?
                         (not (##identifier-assigned? var)))
                     (##simple-exp? exp all-vars))
                (classify unref
                          lambdas
                          (cons (cons var exp) simple)
                          complex
                          (cdr bindings)))
               ((and (not (##identifier-assigned? var))
                     (pair? exp)
                     (eqv? (car exp) '##fix))
                ;; assimilating nested ##fixes
                (classify unref
                          (append (cadr exp) lambdas)
                          simple
                          complex
                          (cons (list var (caddr exp))
                                (cdr bindings))))
               ((and (not (##identifier-assigned? var))
                     (pair? exp)
                     (##lambda-exp? (car exp))
                     (partition-bindings (map list (cadar exp) (cdr exp))
                                         (caddar exp)
                                         all-vars
                                         assimilate-let)) =>
                ;; assimilating nested lets (closed applications)
                (lambda (simple-lambdas)
                  (classify unref
                            (append (cdr simple-lambdas) lambdas)
                            (append (car simple-lambdas) simple)
                            complex
                            (cons (list var (caddar exp))
                                  (cdr bindings)))))
               (else
                (let ((tvar (##make-identifier 'tvar)))
                  (##identifier-assigned-set! var #t)
                  (##identifier-referenced-set! tvar #t)
                  (classify unref
                            lambdas
                            simple
                            (cons (list var tvar #f exp)
                                  complex)
                            (cdr bindings))))))))))

  (define (fix-letrec body vars unref lambdas simple complex)
    (let* ((c-code (if (null? complex)
                       '()
                       (let ((sets (map (lambda (c t)
                                          `(set! ,c ,t))
                                        (map car complex)
                                        (map cadr complex))))
                         (list `((lambda ,(map cadr complex)
                                   ,(if (null? (cdr sets))
                                        (car sets)
                                        `(begin ,@sets)))
                                 ,@(map cadddr complex))))))
           (b-code (let ((unref-exps (if (null? unref)
                                         '()
                                         (map cdr unref)))
                         (old-body (if (and (pair? body)
                                            (eqv? (car body) 'begin))
                                       (cdr body)
                                       (list body))))
                     (append unref-exps c-code old-body)))
           (l-code (let ((b*-code (if (null? (cdr b-code))
                                      (car b-code)
                                      (cons 'begin b-code))))
                     (if (null? lambdas)
                         b*-code
                         `(##fix ,lambdas
                                 ,b*-code)))))
      (if (and (null? simple)
               (null? complex))
          l-code
          `((lambda ,(append (map car simple)
                             (map car complex))
              ,l-code)
            ,@(append (map cdr simple)
                      (map caddr complex))))))

  (define (fix-letrec* body vars unref lambdas simple complex)
    (let* ((sets (let loop ((unref unref)
                            (complex complex)
                            (sets '()))
                   (if (null? unref)
                       (if (null? complex)
                           (reverse sets)
                           (let* ((binding (car complex))
                                  (var (car binding))
                                  (exp (cadddr binding))
                                  (set `(set! ,var ,exp)))
                             (loop '() (cdr complex) (cons set sets))))
                       (if (null? complex)
                           (let* ((binding (car unref))
                                  (exp (cdr unref)))
                             (loop (cdr unref) '() (cons exp sets)))
                           (let ((fu (caar unref))
                                 (fc (caar complex)))
                             (if (##before? fu fc vars)
                                 (let ((exp (cdar unref)))
                                   (loop (cdr unref)
                                         complex
                                         (cons exp sets)))
                                 (let* ((binding (car complex))
                                        (exp (cadddr binding))
                                        (set `(set! ,fc ,exp)))
                                   (loop unref
                                         (cdr complex)
                                         (cons exp sets)))))))))
           (b-code (let ((old-body (if (and (pair? body)
                                            (eqv? (car body) 'begin))
                                       (cdr body)
                                       (list body))))
                     (append sets old-body)))
           (l-code (let ((b*-code (if (null? (cdr b-code))
                                      (car b-code)
                                      (cons 'begin b-code))))
                     `(##fix ,lambdas
                             ,b*-code))))
      (if (and (null? simple)
               (null? complex))
          l-code
          `((lambda ,(append (map car simple)
                             (map car complex))
              ,l-code)
            ,@(append (map cdr simple)
                      (map caddr complex))))))

  ;; walks code calling fix-letrec on letrecs
  ;; and removing assignments to unreferenced variables
  (define (walk-exp e)
    (if (pair? e)
        (case (car e)
          ((quote) e)
          ((begin if)
           `(,(car e) ,@(map walk-exp (cdr e))))
          ((define)
           `(define ,(cadr e) ,(walk-exp (caddr e))))
          ((lambda)
           `(lambda ,(cadr e)
              ,(walk-exp (caddr e))))
          ((letrec)
           (let ((body (caddr e))
                 (bindings (cadr e)))
             (partition-bindings bindings body '() fix-letrec)))
          ((letrec*)
           (let ((body (caddr e))
                 (bindings (cadr e)))
             (partition-bindings bindings body '() fix-letrec*)))
          ((set!)
           (let ((assignee (cadr e))
                 (assigned (walk-exp (caddr e))))
             (if (or (symbol? assignee)
                     (##identifier-referenced? assignee))
                 `(set! ,assignee ,assigned)
                 assigned)))
          (else
           (map walk-exp e)))
        e))

  (walk-exp e))

;; a simple expression is free from effects and cannot capture its
;; continuation
(define (##simple-exp? e bound-vars)
  (cond
   ((pair? e)
    (case (car e)
      ((begin if) (all? (lambda (e)
                          (##simple-exp? e bound-vars))
                        (cdr e)))
      ((lambda ##fix) #f)
      ((quote) #t)
      ((set!) #f)
      (else (and (all? (lambda (e)
                         (##simple-exp? e bound-vars))
                       (cdr e))
                 (memq (car e) ##effectless-primitives)))))
   ((##identifier? e) (not (memq e bound-vars)))
   ((symbol? e) #f)
   (else #t)))

(define ##effectless-primitives
  '(+ - * / < > append assoc assv assq car cdr cons integer? list
      member memv memq not number? pair? quotient remainder symbol?))

;;
;; Assignment and global conversion
;;

(define (##make-primitive sym)
  (vector 'primitive sym))

(define (##primitive? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) 'primitive)))

(define (##convert-assignments e)

  (define (convert-lambda l)
    (let ((new-body (walk-exp (caddr l))))
      (let loop ((args (cadr l))
                 (new-args '())
                 (mapping '()))
        (if (null? args)
            (if (null? mapping)
                `(lambda ,(cadr l)
                   ,new-body)
                `(lambda ,(reverse new-args)
                   ((lambda ,(map car mapping)
                      ,new-body)
                    ,@(map (lambda (m)
                             `(##make-box ,(cdr m)))
                           mapping))))
            (let ((arg (car args)))
              (if (##identifier-assigned? arg)
                  (let ((new-arg (##make-identifier 'svar)))
                    (##identifier-assigned-set! arg #f)
                    (##identifier-referenced-set! new-arg #t)
                    (loop (cdr args)
                          (cons new-arg new-args)
                          (cons (cons arg new-arg) mapping)))
                  (loop (cdr args)
                        (cons arg new-args)
                        mapping)))))))

  (define (walk-exp e)
    (if (pair? e)
        (let ((op (car e)))
          (cond
           ((eqv? op 'quote)
            e)
           ((member op '(begin if))
            `(,op ,@(map walk-exp (cdr e))))
           ((eqv? op 'define)
            `(##make-global ',(cadr e) ,(walk-exp (caddr e))))
           ((eqv? op 'lambda)
            (convert-lambda e))
           ((eqv? op '##fix)
            (let loop ((lambdas (cadr e))
                       (converted '()))
              (if (null? lambdas)
                  `(##fix ,(reverse converted)
                          ,(walk-exp (caddr e)))
                  (let ((binding (car lambdas)))
                    (loop (cdr lambdas)
                          (cons (list (car binding)
                                      (convert-lambda (cadr binding)))
                                converted))))))
           ((eqv? op 'set!)
            (let ((assignee (cadr e))
                  (assigned (walk-exp (caddr e))))
              (if (symbol? assignee)
                  `(##global-set! ',assignee ,assigned)
                  `(##box-set! ,assignee ,assigned))))
           ((member op vm-prims)
            `(,(##make-primitive op) ,@(map walk-exp (cdr e))))
           (else
            (map walk-exp e))))
        (cond
         ((symbol? e)
          `(##global-ref ',e))
         ((and (##identifier? e)
               (##identifier-assigned? e))
          `(##box-ref ,e))
         (else e))))

  ;; primitives implemented by VM instructions
  (define vm-prims '(+ - * / = car cdr cons null? bool? char? fixnum? pair?
                       symbol? zero? not))

  (walk-exp e))

;;
;; this pass transforms expressions into continuation-passing
;; style
;;

(define (##to-cps e)

  (define (cps-lambda e)
    (let ((vars (cadr e))
          (body (caddr e))
          (k (##make-identifier 'k)))
      `(lambda (,k ,@vars)
         ,(cps-c body k))))

  (define (cps*-k e* k)
    (if (null? e*)
        (k '())
        (cps-k (car e*)
               (lambda (hd)
                 (cps*-k (cdr e*)
                         (lambda (tl)
                           (k (cons hd tl))))))))

  (define (cps-k e k)
    (if (pair? e)
        (let ((op (car e)))
          (cond
           ((eqv? op 'quote)
            (k e))
           ((eqv? op 'begin)
            (let ((rest (cddr e)))
              (cps-k (cadr e)
                     (if (null? rest)
                         k
                         (lambda (hd)
                           (cps-k `(begin ,@rest) k))))))
           ((eqv? op 'if)
            (let ((r (##make-identifier 'r))
                  (c (##make-identifier 'k))
                  (conseq (caddr e))
                  (altern (if (null? (cdddr e))
                              (##void)
                              (cadddr e))))
              `((lambda (,c)
                  ,(cps-k (cadr e)
                          (lambda (test)
                            `(if ,test
                                 ,(cps-c conseq c)
                                 ,(cps-c altern c)))))
                (lambda (,r) ,(k r)))))
           ((eqv? op 'lambda)
            (k (cps-lambda e)))
           ((eqv? op '##fix)
            (let loop ((bindings (cadr e))
                       (new-bindings '()))
              (if (null? bindings)
                  `(##fix ,(reverse new-bindings)
                          ,(cps-k (caddr e) k))
                  (let* ((binding (car bindings))
                         (i (car binding))
                         (l (cadr binding)))
                    (loop (cdr bindings)
                          (cons (list i (cps-lambda l))
                                new-bindings))))))
           ((##primitive? op)
            (let* ((r (##make-identifier 'r))
                   (c `(lambda (,r) ,(k r))))
              (cps*-k (cdr e)
                      (lambda (args)
                        `(,op ,c ,@args)))))
           (else
            (let* ((r (##make-identifier 'r))
                   (c `(lambda (,r) ,(k r))))
              (cps-k (car e)
                     (lambda (op)
                       (cps*-k (cdr e)
                               (lambda (args)
                                 `(,op ,c ,@args)))))))))
        (k e)))

  (define (cps-c e c)
    (if (pair? e)
        (let ((op (car e)))
          (cond
           ((eqv? op 'quote)
            `(,c ,e))
           ((eqv? op 'begin)
            (let ((rest (cddr e)))
              (if (null? rest)
                  (cps-c (cadr e) c)
                  (cps-k (cadr e)
                         (lambda (hd)
                           (cps-c `(begin ,@rest) c))))))
           ((eqv? op 'if)
            (let ((conseq (caddr e))
                  (altern (if (null? (cdddr e))
                              (##void)
                              (cadddr e))))
              (cps-k (cadr e)
                     (lambda (test)
                       `(if ,test
                            ,(cps-c conseq c)
                            ,(cps-c altern c))))))
           ((eqv? op 'lambda)
            `(,c ,(cps-lambda e)))
           ((eqv? op '##fix)
            (let loop ((bindings (cadr e))
                       (new-bindings '()))
              (if (null? bindings)
                  `(##fix ,(reverse new-bindings)
                          ,(cps-c (caddr e) c))
                  (let* ((binding (car bindings))
                         (i (car binding))
                         (l (cadr binding)))
                    (loop (cdr bindings)
                          (cons (list i (cps-lambda l))
                                new-bindings))))))
           ((##primitive? op)
            (cps*-k (cdr e)
                    (lambda (args)
                      `(,op ,c ,@args))))
           (else
            (cps-k (car e)
                   (lambda (op)
                     (cps*-k (cdr e)
                             (lambda (args)
                               `(,op ,c ,@args))))))))
        `(,c ,e)))

  (cps-c e 'halt))

;;;
;;; core language compilation
;;;

;; compiles toplevel
(define (meaning-toplevel e+)
  (if (null? (cdr e+))
      (meaning (car e+) '() #t)
      (let ((m (meaning (car e+) '() #f))
            (m+ (meaning-toplevel (cdr e+))))
        (vector 'sequence m m+))))

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
         (if (##lambda-exp? e)
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
  (if (##lambda-exp? e)
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

  (define (meaning-internal n* n e r)
    (let* ((an* (map (lambda (n)
                       (cons n (rename-var n)))
                     (if n
                         (append n* (list n))
                         n*)))
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
       (string-append "#:"
                      (symbol->string n)
                      "v"
                      (number->string c))))))

;; tests is an expression is a valid lambda expression
(define (##lambda-exp? e)
  (and (pair? e)
       (eq? (car e) 'lambda)
       (> (length e) 2)
       (symbol-exp? (cadr e))))

(define (symbol-exp? e)
  (or (symbol? e)
      (##identifier? e)
      (null? e)
      (and (pair? e)
           (or (symbol? (car e))
               (##identifier? (car e)))
           (symbol-exp? (cdr e)))))

(define (symbol-exp->list e)
  (let loop ((syms e)
             (ans '()))
    (cond
     ((null? syms)
      e)
     ((or (symbol? syms)
          (##identifier? syms))
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
     ((or (symbol? syms)
          (##identifier? syms))
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
    (instr cs 'SAVE-CONT)
    (instr cs 'PUSH)
    (generate-continuation-closure cs)
    (instr cs 'PUSH)
    ;; calling closure given to call/cc with
    ;; continuation-restoring closure as sole argument
    (generate-lambda cs (vector-ref m 1))
    (instr1 cs (if tail? 'TAIL-CALL 'CALL) 1)
    ;; back-patching return address
    (or tail? (patch-instr! cs i (code-size cs)))))

(define (generate-alternative cs m)
  (let ((testc (vector-ref m 1))
        (thenc (vector-ref m 2))
        (elsec (vector-ref m 3)))
    (generate-code cs testc)
    (let ((i (code-size cs)))
      ;; this will be back-patched later
      (instr1 cs 'JMP-IF-NOT 0)
      (generate-code cs thenc)
      (if (eqv? (vector-ref elsec 0) 'undef)
          (let ((j (code-size cs)))
            (patch-instr! cs i (- j i 1)))
          (let ((j (code-size cs)))
            ;; this will be back-patched later
            (instr1 cs 'JMP 0)
            (let ((k (code-size cs)))
              (patch-instr! cs i (- k i 1))
              (generate-code cs elsec)
              (let ((m (code-size cs)))
                (patch-instr! cs j (- m j 1)))))))))

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
                 (generate-code cs (vector-ref args 1))
                 (instr cs code))
                ((2)
                 (generate-code cs (vector-ref args 1))
                 (instr cs 'PUSH)
                 (generate-code cs (vector-ref (vector-ref args 2) 1))
                 (instr cs code))
                (else
                 (error "Primitive with unknown arity")))
              (error "Primitive called with wrong arity!" prim)))
        (error "Unknown primitive"))))

(define (generate-common-apply cs m)
  (let ((i (code-size cs))
        (tail? (memq 'tail (vector-ref m 1))))
    ;; this is the return address, will be back-patched later
    (or tail? (instr1 cs 'FRAME 0))
    (let ((len (generate-push-arguments cs (vector-ref m 3))))
      (generate-code cs (vector-ref m 2))
      (instr1 cs (if tail? 'TAIL-CALL 'CALL) len)
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
    (instr cs 'STRING->SYMBOL))
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

(define (##before? a b list)
  (let loop ((list list))
    (if (null? list)
        #f
        (let ((item (car list)))
          (if (eqv? item a)
              #t
              (if (eqv? item b)
                  #f
                  (loop (cdr list))))))))
