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
                                  (exp (cdr binding)))
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

(define (##convert-assignments e)

  (define global-ref-prim (##make-primitive '##global-ref))

  (define global-set-prim (##make-primitive '##global-set!))

  (define make-global-prim (##make-primitive '##make-global))

  (define box-ref-prim (##make-primitive '##box-ref))

  (define box-set-prim (##make-primitive '##box-set!))

  (define make-box-prim (##make-primitive '##make-box))

  (define (convert-lambda l)
    (let ((new-body (walk-exp (caddr l))))
      (let loop ((args (cadr l))
                 (new-args '())
                 (mapping '())
                 (extra #f))
        (cond
         ((null? args)
          (if (null? mapping)
              `(lambda ,(cadr l)
                 ,new-body)
              `(lambda ,(if extra
                            (if (null? new-args)
                                extra
                                `(,@(reverse new-args) . ,extra))
                            (reverse new-args))
                 ((lambda ,(map car mapping)
                    ,new-body)
                  ,@(map (lambda (m)
                           `(,make-box-prim ,(cdr m)))
                         mapping)))))
         ((pair? args)
          (let ((arg (car args)))
            (if (##identifier-assigned? arg)
                (let ((new-arg (##make-identifier 'svar)))
                  (##identifier-assigned-set! arg #f)
                  (##identifier-referenced-set! new-arg #t)
                  (loop (cdr args)
                        (cons new-arg new-args)
                        (cons (cons arg new-arg) mapping)
                        #f))
                (loop (cdr args) (cons arg new-args) mapping #f))))
         (else
          (if (##identifier-assigned? args)
              (let ((new-arg (##make-identifier 'svar)))
                (##identifier-assigned-set! args #f)
                (##identifier-referenced-set! new-arg #t)
                (loop '()
                      new-args
                      (cons (cons args new-arg) mapping)
                      new-arg))
              (loop '() new-args mapping args)))))))

  (define (walk-exp e)
    (if (pair? e)
        (let ((op (car e)))
          (cond
           ((eqv? op 'quote)
            e)
           ((member op '(begin if))
            `(,op ,@(map walk-exp (cdr e))))
           ((eqv? op 'define)
            `(,make-global-prim ',(cadr e) ,(walk-exp (caddr e))))
           ((eqv? op 'lambda)
            (convert-lambda e))
           ((eqv? op '##fix)
            `(##fix ,(map (lambda (b)
                            (list (car b) (convert-lambda (cadr b))))
                          (cadr e))
                    ,(walk-exp (caddr e))))
           ((eqv? op 'set!)
            (let ((assignee (cadr e))
                  (assigned (walk-exp (caddr e))))
              (if (symbol? assignee)
                  `(,global-set-prim ',assignee ,assigned)
                  `(,box-set-prim ,assignee ,assigned))))
           ((member op vm-prims)
            `(,(##make-primitive op) ,@(map walk-exp (cdr e))))
           (else
            (map walk-exp e))))
        (cond
         ((symbol? e)
          `(,global-ref-prim ',e))
         ((and (##identifier? e)
               (##identifier-assigned? e))
          `(,box-ref-prim ,e))
         (else e))))

  ;; primitives implemented by VM instructions
  (define vm-prims '(+ - * / = car cdr cons null? bool? char? fixnum? pair?
                       symbol? zero? not))

  (walk-exp e))

;;;
;;; CPS transformation
;;;
;;; from:
;;; http://matt.might.net/articles/cps-conversion/
;;;

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
                              '(##void)
                              (cadddr e))))
              `(##fix ((,c (lambda (,r) ,(k r))))
                 ,(cps-k (cadr e)
                         (lambda (test)
                           `(if ,test
                                ,(cps-c conseq c)
                                ,(cps-c altern c)))))))
           ((eqv? op 'lambda)
            (let ((l (##make-identifier 'l)))
              `(##fix ((,l ,(cps-lambda e)))
                 ,(k l))))
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
            (let ((c (##make-identifier 'k))
                  (r (##make-identifier 'r)))
              (cps*-k (cdr e)
                      (lambda (args)
                        `(##fix ((,c (lambda (,r) ,(k r))))
                           (,op ,c ,@args))))))
           (else
            (let ((c (##make-identifier 'k))
                  (r (##make-identifier 'r)))
              (cps-k (car e)
                     (lambda (op)
                       (cps*-k (cdr e)
                               (lambda (args)
                                 `(##fix ((,c (lambda (,r) ,(k r))))
                                    (,op ,c ,@args))))))))))
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
            (let ((l (##make-identifier 'l)))
              `(##fix ((,l ,(cps-lambda e)))
                 (,c ,l))))
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

  (cps-c e '##halt))

;;;
;;; AST creation
;;; from this point onwards the code must be analysed and the tree annotated.
;;; to make this easier, we transform the source tree into an object tree.
;;;

(define (##objectify e)

  (define (objectify-lambda e)

    (define (parse n* regular)
      (cond
       ((null? n*)
        (objectify-internal (reverse regular) #f))
       ((pair? n*)
        (parse (cdr n*) (cons (car n*) regular)))
       (else
        (objectify-internal (reverse regular) n*))))

    (define (objectify-internal n* n)
      (let ((body (objectify (caddr e))))
        (##make-lambda (cons (if n '>= '=)
                             (length n*))
                       (if n (append n* (list n)) n*)
                       body)))

    (parse (cadr e) '()))

  (define (objectify e)
    (if (pair? e)
        (let ((op (car e)))
          (cond
           ((eqv? op '##fix)
            (let loop ((bindings (cadr e))
                       (idents '()))
              (if (null? bindings)
                  (##make-fix idents (objectify (caddr e)))
                  (let* ((binding (car bindings))
                         (ident (car binding))
                         (proc (cadr binding)))
                    (##identifier-lambda-set! ident (objectify proc))
                    (loop (cdr bindings) (cons ident idents))))))
           ((eqv? op 'if)
            (##make-conditional (objectify (cadr e))
                                (objectify (caddr e))
                                (objectify (cadddr e))))
           ((eqv? op 'lambda)
            (objectify-lambda e))
           ((eqv? op 'quote)
            (##make-constant (cadr e)))
           (else
            (##make-application (objectify op)
                                (map objectify (cdr e))))))
        (if (or (##identifier? e)
                (##primitive? e))
            e
            (##make-constant e))))

  (objectify e))

;; walks and annotate the tree regarding identifier use
(define (##annotate-identifiers! e)

  (define (annotate! e)
    (cond
     ((##application? e)
      (let ((op (##application-op e)))
        (if (##identifier? op)
            (let ((calls (##identifier-calls op)))
              (##identifier-calls-set! op (+ calls 1)))))
      (for-each annotate! (##application-args e)))
     ((##conditional? e)
      (annotate! (##conditional-test e))
      (annotate! (##conditional-conseq e))
      (annotate! (##conditional-altern e)))
     ((##fix? e)
      (for-each (lambda (i)
                  (##identifier-uses-set! i 0)
                  (##identifier-calls-set! i 0))
                (##fix-lambdas e))
      (for-each (lambda (i)
                  (annotate! (##identifier-lambda i)))
                (##fix-lambdas e))
      (annotate! (##fix-body e)))
     ((##identifier? e)
      (let ((uses (##identifier-uses e)))
        (##identifier-uses-set! e (+ uses 1))))
     ((##lambda? e)
      (for-each (lambda (v)
                  (##identifier-uses-set! v 0)
                  (##identifier-calls-set! v 0)
                  (##identifier-lambda-set! v #f))
                (##lambda-vars e))
      (annotate! (##lambda-body e)))))

  (annotate! e))

;; this pass makes only optimisations that guarantee a smaller
;; code size
(define (##beta-contraction e)

  (define (dead? i)
    (and (##identifier? i)
         (zero? (##identifier-uses i))
         (zero? (##identifier-calls i))))

  ;; a known function is bound in a FIX, do not escape
  ;; and has fixed arity (for now)
  (define (known? i)
    (and (##identifier? i)
         (zero? (##identifier-uses i))
         (let ((proc (##identifier-lambda i)))
           (and proc
                (let ((arity (##lambda-arity proc)))
                  (eqv? (car arity) '=))))))

  ;; reducing lambdas that are known and are called
  ;; just once
  (define (reducible? i)
    (and (known? i)
         (= (##identifier-calls i) 1)))

  (define (subst e m)
    (cond
     ((##application? e)
      (##make-application (subst (##application-op e) m)
                          (map (lambda (a)
                                 (subst a m))
                               (##application-args e))))
     ((##conditional? e)
      (##make-conditional (subst (##conditional-test e) m)
                          (subst (##conditional-conseq e) m)
                          (subst (##conditional-altern e) m)))
     ((##fix? e)
      (##make-fix (map (lambda (l)
                         (##identifier-lambda-set!
                          l
                          (subst (##identifier-lambda l) m))
                         l)
                       (##fix-lambdas e))
                  (subst (##fix-body e) m)))
     ((##identifier? e)
      (let ((ret (assq e m)))
        (if ret (cdr ret) e)))
     ((##lambda? e)
      (##make-lambda (##lambda-arity e)
                     (##lambda-vars e)
                     (subst (##lambda-body e) m)))
     (else
      e)))

  (define (reduce e)
    (cond
     ((##application? e)
      (let ((op (##application-op e))
            (args (map reduce (##application-args e))))
        (if (known? op)
            (let* ((proc (##identifier-lambda op))
                   (vars (##lambda-original-vars proc))
                   (arity (length vars)))
              (if (= (length args) arity)
                  (if (reducible? op)
                      ;; beta-reducing the lambda
                      (let ((mapping (map cons
                                          (##lambda-vars proc)
                                          args)))
                        (subst (##lambda-body proc) mapping))
                      ;; eliminating unused arguments
                      (let loop ((args args)
                                 (vars vars)
                                 (new-args '()))
                        (if (null? vars)
                            (##make-application op (reverse new-args))
                            (let ((var (car vars))
                                  (arg (car args)))
                              (loop (cdr args)
                                    (cdr vars)
                                    (if (dead? var)
                                        new-args
                                        (cons arg new-args)))))))
                  (##make-application op args)))
            (##make-application op args))))
     ((##conditional? e)
      (let ((test (##conditional-test e))
            (conseq (##conditional-conseq e))
            (altern (##conditional-altern e)))
        ;; removing unreachable branches
        (if (##constant? test)
            (if (##constant-value test)
                (reduce conseq)
                (reduce altern))
            (##make-conditional (reduce test)
                                (reduce conseq)
                                (reduce altern)))))
     ((##fix? e)
      (let loop ((lambdas (##fix-lambdas e))
                 (out '()))
        (if (null? lambdas)
            (if (null? out)
                ;; removing empty FIXes
                (reduce (##fix-body e))
                (##make-fix out (reduce (##fix-body e))))
            (let ((ident (car lambdas)))
              (if (or (dead? ident)
                      (reducible? ident))
                  (loop (cdr lambdas) out)
                  (let ((proc (reduce (##identifier-lambda ident))))
                    (if (known? ident)
                        (let ((vars (filter (lambda (v)
                                              (not (dead? v)))
                                            (##lambda-vars proc))))
                          (##lambda-arity-set! proc (cons '= (length vars)))
                          (##lambda-vars-set! proc vars)))
                    (begin
                      (##identifier-lambda-set! ident proc)
                      (loop (cdr lambdas) (cons ident out)))))))))
     ((##lambda? e)
      (##make-lambda (##lambda-arity e)
                     (##lambda-vars e)
                     (reduce (##lambda-body e))))
     (else
      e)))

  (reduce e))

;; an identifier is the meaning of a variable
(define (##make-identifier name)
  (vector '##ident (rename-var name) #f #f #f))

(define (##identifier? id)
  (and (vector? id)
       (eqv? (vector-ref id 0) '##ident)))

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

;; reused slot
(define (##identifier-uses id)
  (vector-ref id 2))

(define (##identifier-uses-set! id n)
  (vector-set! id 2 n))

;; reused slot
(define (##identifier-calls id)
  (vector-ref id 3))

(define (##identifier-calls-set! id n)
  (vector-set! id 3 n))

(define (##identifier-lambda id)
  (vector-ref id 4))

(define (##identifier-lambda-set! id l)
  (vector-set! id 4 l))

;; a primitive gets special treatment from the VM
(define (##make-primitive name)
  (vector '##prim name))

(define (##primitive? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##prim)))

(define (##primitive-name a)
  (vector-ref a 1))

;; application node
(define (##make-application op args)
  (vector '##app op args))

(define (##application? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##app)))

(define (##application-op a)
  (vector-ref a 1))

(define (##application-args a)
  (vector-ref a 2))

;; conditional node
(define (##make-conditional test conseq altern)
  (vector '##cond test conseq altern))

(define (##conditional? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##cond)))

(define (##conditional-test a)
  (vector-ref a 1))

(define (##conditional-conseq a)
  (vector-ref a 2))

(define (##conditional-altern a)
  (vector-ref a 3))

;; constant node
(define (##make-constant value)
  (vector '##const value))

(define (##constant? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##const)))

(define (##constant-value a)
  (vector-ref a 1))

;; fix node
(define (##make-fix lambdas body)
  (vector '##fix lambdas body))

(define (##fix? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##fix)))

(define (##fix-lambdas a)
  (vector-ref a 1))

(define (##fix-body a)
  (vector-ref a 2))

;; lambda node
(define (##make-lambda arity vars body)
  (vector '##lambda arity vars vars body))

(define (##lambda? a)
  (and (vector? a)
       (eqv? (vector-ref a 0) '##lambda)))

(define (##lambda-arity a)
  (vector-ref a 1))

(define (##lambda-arity-set! a n)
  (vector-set! a 1 n))

(define (##lambda-vars a)
  (vector-ref a 2))

(define (##lambda-vars-set! a v)
  (vector-set! a 2 v))

(define (##lambda-original-vars a)
  (vector-ref a 3))

(define (##lambda-body a)
  (vector-ref a 4))

(define (immediate? x)
  (or (char? x)
      (boolean? x)
      (integer? x)
      (null? x)))

(define rename-var
  (let ((c 0))
    (lambda (n)
      (set! c (+ c 1))
      (string->symbol
       (string-append (symbol->string n)
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
