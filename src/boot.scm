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

;; Temporary bizarre solution
(define gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (add1 counter))
      (string->symbol
       (string-append "sly-gensym-"
                      (number->string counter))))))

;; creates a new syntactic closure closing 'exp'
;; with 'env' letting 'free' free
(define (make-syntactic-closure env free exp)
  (vector 'syntactic-closure free env exp))

(define (syntactic-closure? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eqv? (vector-ref obj 0) 'syntactic-closure)))

(define (syntactic-closure-free sc)
  (vector-ref sc 1))

(define (syntactic-closure-env sc)
  (vector-ref sc 2))

(define (syntactic-closure-exp sc)
  (vector-ref sc 3))

;; helper procedure to close a list of expressions
(define (make-syntactic-closure-list syn-env free exps)
  (map (lambda (exp)
         (make-syntactic-closure syn-env free exp))
       exps))

(define scheme-syntactic-environment
  (let ((and-expander (lambda (exp env)
                        (let ((ops (make-syntactic-closure-list env
                                                                '()
                                                                (cdr exp))))
                          (cond
                           ((null? ops) #t)
                           ((null? (cdr ops)) (car ops))
                           (else
                            `(let ((temp ,(car ops)))
                               (if temp
                                   (and ,@(cdr ops))
                                   temp)))))))
        (case-expander (lambda (exp env)
                         (if (or (null? (cdr exp))
                                 (null? (cddr exp)))
                             (error "invalid 'case' expression" exp)
                             (let ((key (make-syntactic-closure env
                                                                '()
                                                                (cadr exp)))
                                   (clauses (reverse (cddr exp))))
                               (let collect ((code '())
                                             (clauses clauses)
                                             (last? #t))
                                 (if (null? clauses)
                                     (if (null? code)
                                         (error "empty 'case'" exp)
                                         `(let ((temp ,key))
                                            ,code))
                                     (let ((clause (car clauses)))
                                       (if (pair? clause)
                                           (let ((data (car clause))
                                                 (body (make-syntactic-closure-list env
                                                                                    '()
                                                                                    (cdr clause))))
                                             (cond
                                              ((eq? data 'else)
                                               (if last?
                                                   (collect `(begin ,@body) (cdr clauses) #f)
                                                   (error "'else' must be last clause in 'case'" exp)))
                                              ((pair? data)
                                               (collect (if (null? code)
                                                            `(if (memv temp ',data)
                                                                 (begin ,@body))
                                                            `(if (memv temp ',data)
                                                                 (begin ,@body)
                                                                 ,code))
                                                        (cdr clauses)
                                                        #f))
                                              (else
                                               (error "ill-formed 'case' clause" clause))))
                                           (error "ill-formed 'case' clause" clause)))))))))
        (cond-expander (lambda (exp env)
                         (let collect ((code '())
                                       (clauses (reverse (cdr exp)))
                                       (last? #t))
                           (if (null? clauses)
                               (if (null? code)
                                   (error "Empty 'cond'" exp)
                                   code)
                               (let ((clause (car clauses)))
                                 (if (pair? clause)
                                     (let ((test (car clause))
                                           (body (cdr clause)))
                                       (if (eqv? test 'else)
                                           (if last?
                                               (let ((body (make-syntactic-closure-list env
                                                                                        '()
                                                                                        body)))
                                                 (collect `(begin ,@body) (cdr clauses) #f))
                                               (error "'else' must be last clause in 'cond'" exp))
                                           (let ((test (make-syntactic-closure env '() test)))
                                             (cond
                                              ((null? body)
                                               (collect (if (null? code)
                                                            ,test
                                                            `(or ,test ,code))
                                                        (cdr clauses)
                                                        #f))
                                              ((eq? (car body) '=>)
                                               (let ((proc (make-syntactic-closure env
                                                                                   '()
                                                                                   (cadr body))))
                                                 (collect (if (null? code)
                                                              `(let ((temp ,test))
                                                                 (if temp
                                                                     (,proc temp)))
                                                              `(let ((temp ,test))
                                                                 (if temp
                                                                     (,proc temp)
                                                                     ,code)))
                                                          (cdr clauses)
                                                          #f)))
                                              (else
                                               (let ((body (make-syntactic-closure-list env
                                                                                        '()
                                                                                        body)))
                                                 (collect (if (null? code)
                                                              `(if ,test
                                                                   (begin ,@body))
                                                              `(if ,test
                                                                   (begin ,@body)
                                                                   ,code))
                                                          (cdr clauses)
                                                          #f)))))))
                                     (error "Ill-formed 'cond' clause" clause)))))))
        (do-expander (lambda (exp env)
                       (let loop ((bindings (cadr exp))
                                  (vars '())
                                  (inits '())
                                  (steps '()))
                         (if (null? bindings)
                             (let* ((vars (reverse vars))
                                    (inits (reverse inits))
                                    (steps (reverse steps))
                                    (test (make-syntactic-closure env
                                                                  vars
                                                                  (caaddr exp)))
                                    (cmds (make-syntactic-closure-list env
                                                                       vars
                                                                       (cdaddr exp))))
                               `(let loop ,(map list vars inits)
                                  (if ,test
                                      (begin ,@cmds)
                                      (begin
                                        ,@(make-syntactic-closure-list env
                                                                       vars
                                                                       (cdddr exp))
                                        (loop ,@(map list vars steps))))))
                             (let* ((binding (car bindings))
                                    (var (car binding))
                                    (init (make-syntactic-closure env
                                                                  '()
                                                                  (cadr binding)))
                                    (step (if (null? (cddr binding))
                                              var
                                              (make-syntactic-closure env
                                                                      `(,var)
                                                                      (caddr binding)))))
                               (loop (cdr bindings)
                                     (cons var vars)
                                     (cons init inits)
                                     (cons step steps)))))))
        (or-expander (lambda (exp env)
                       (let ((ops (make-syntactic-closure-list env
                                                               '()
                                                               (cdr exp))))
                         (cond
                          ((null? ops) #f)
                          ((null? (cdr ops)) (car ops))
                          (else
                           `(let ((temp ,(car ops)))
                              (if temp
                                  temp
                                  (or ,@(cdr ops)))))))))
        (let-expander (lambda (exp env)
                        (if (symbol? (cadr exp))
                            ;; named let
                            (let* ((name (cadr exp))
                                   (bindings (caddr exp))
                                   (identifiers (map car bindings))
                                   (expressions (make-syntactic-closure-list env
                                                                             '()
                                                                             (map cadr bindings)))
                                   (body (make-syntactic-closure-list env
                                                                      (cons name identifiers)
                                                                      (cdddr exp))))
                              `(letrec ((,name (lambda ,identifiers
                                                 ,@body)))
                                 (,name ,@expressions)))
                            ;; ordinary let
                            (let ((identifiers (map car (cadr exp))))
                              `((lambda ,identifiers
                                  ,@(make-syntactic-closure-list env
                                                                 identifiers
                                                                 (cddr exp)))
                                ,@(make-syntactic-closure-list
                                   env
                                   '()
                                   (map cadr (cadr exp))))))))
        (let*-expander (lambda (exp env)
                         (let ((bindings (cadr exp)))
                           (cond
                            ((null? bindings)
                             `((lambda ()
                                 ,@(make-syntactic-closure-list env
                                                                '()
                                                                (cddr exp)))))
                            ((null? (cdr bindings))
                             (let ((var (caar bindings)))
                               `((lambda (,var)
                                   ,@(make-syntactic-closure-list env
                                                                  `(,var)
                                                                  (cddr exp)))
                                 ,(make-syntactic-closure env
                                                          '()
                                                          (cadar bindings)))))
                            (else
                             (let ((identifiers (map car bindings)))
                               `((lambda (,(caar bindings))
                                   (let* ,(cdr bindings)
                                     ,@(make-syntactic-closure-list env
                                                                    identifiers
                                                                    (cddr exp))))
                                 ,(make-syntactic-closure env
                                                          '()
                                                          (cadar bindings)))))))))
        (quasi-expander (lambda (exp env)
                          (define (quoted-symbol? e)
                            (and (eq? (car e) 'quote)
                                 (pair? (cdr e))
                                 (null? (cddr e))
                                 (symbol? (cadr e))))
                          (define (qq-expand e level)
                            (if (and (pair? e)
                                     (not (quoted-symbol? e)))
                                (case (car e)
                                  ((quasiquote)
                                   `(cons 'quasiquote ,(qq-expand (cadr e) (+ level 1))))
                                  ((unquote unquote-splicing)
                                   (cond
                                    ((> level 0)
                                     `(cons ',(car e) ,(qq-expand (cadr e) (- level 1))))
                                    ((eqv? (car e) 'unquote)
                                     (make-syntactic-closure env '() (cadr e)))
                                    (else
                                     (error "Illegal use if unquote-splicing"))))
                                  (else
                                   `(append ,(qq-expand-list (car e) level)
                                            ,(qq-expand (cdr e) level))))
                                `',e))
                          (define (qq-expand-list e level)
                            (if (and (pair? e)
                                     (not (quoted-symbol? e)))
                                (case (car e)
                                  ((quasiquote)
                                   `(list (cons 'quasiquote ,(qq-expand (cadr e) (+ level 1)))))
                                  ((unquote unquote-splicing)
                                   (cond
                                    ((> level 0)
                                     `(list (cons ',(car e) ,(qq-expand (cadr e) (- level 1)))))
                                    ((eqv? (car e) 'unquote)
                                     `(list ,(make-syntactic-closure env '() (cadr e))))
                                    (else
                                     (make-syntactic-closure env '() (cadr e)))))
                                  (else
                                   `(list (append ,(qq-expand-list (car e) level)
                                                  ,(qq-expand (cdr e) level)))))
                                `'(,e)))
                          (qq-expand (cadr exp) 0)))
        (when-expander (lambda (exp env)
                         (let ((test (make-syntactic-closure env
                                                             '()
                                                             (cadr exp)))
                               (body (make-syntactic-closure-list env
                                                                  '()
                                                                  (cddr exp))))
                           `(if ,test (begin ,@body)))))
        (unless-expander (lambda (exp env)
                           (let ((test (make-syntactic-closure env
                                                               '()
                                                               (cadr exp)))
                                 (body (make-syntactic-closure-list env
                                                                    '()
                                                                    (cddr exp))))
                             `(if (not ,test) (begin ,@body))))))
    `((and        . ,and-expander)
      (case       . ,case-expander)
      (cond       . ,cond-expander)
      (do         . ,do-expander)
      (let        . ,let-expander)
      (let*       . ,let*-expander)
      (or         . ,or-expander)
      (quasiquote . ,quasi-expander)
      (when       . ,when-expander)
      (unless     . ,unless-expander))))
