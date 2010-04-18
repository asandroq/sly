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
  (let ((and-expander (lambda (syn-env exp)
                        (let ((ops (make-syntactic-closure-list syn-env
                                                                '()
                                                                (cdr exp))))
                          (cond
                           ((null? ops)
                            (make-syntactic-closure
                             scheme-syntactic-environment '() #t))
                           ((null? (cdr ops))
                            (car ops))
                           (else
                            (make-syntactic-closure
                             scheme-syntactic-environment
                             '()
                             `(let ((temp ,(car ops)))
                                (if temp
                                    (and ,@(cdr ops))
                                    temp))))))))
        (or-expander (lambda (syn-env exp)
                       (let ((ops (make-syntactic-closure-list syn-env
                                                               '()
                                                               (cdr exp))))
                         (cond
                          ((null? ops)
                           (make-syntactic-closure
                            scheme-syntactic-environment '() #f))
                          ((null? (cdr ops))
                           (car ops))
                          (else
                           (make-syntactic-closure
                            scheme-syntactic-environment
                            '()
                            `(let ((temp ,(car ops)))
                               (if temp
                                   temp
                                   (or ,@(cdr ops))))))))))
        (let-expander (lambda (syn-env exp)
                        (let ((identifiers (map car (cadr exp))))
                          (make-syntactic-closure
                           scheme-syntactic-environment
                           '()
                           `((lambda ,identifiers
                               ,@(make-syntactic-closure-list syn-env
                                                              identifiers
                                                              (cddr exp)))
                             ,@(make-syntactic-closure-list
                                syn-env
                                '()
                                (map cadr (cadr exp))))))))
        (let*-expander (lambda (syn-env exp)
                         (let ((bindings (cadr exp)))
                           (cond
                            ((null? bindings)
                             (make-syntactic-closure
                              scheme-syntactic-environment
                              '()
                              `((lambda ()
                                  ,@(make-syntactic-closure-list syn-env
                                                                 '()
                                                                 (cddr exp))))))
                            ((null? (cdr bindings))
                             (let ((var (caar bindings)))
                               (make-syntactic-closure
                                scheme-syntactic-environment
                                '()
                                `((lambda (,var)
                                    ,@(make-syntactic-closure-list syn-env
                                                                   `(,var)
                                                                   (cddr exp)))
                                  ,(make-syntactic-closure syn-env
                                                           '()
                                                           (cadar bindings))))))
                            (else
                             (let ((identifiers (map car bindings)))
                               (make-syntactic-closure
                                scheme-syntactic-environment
                                '()
                                `((lambda (,(caar bindings))
                                    (let* ,(cdr bindings)
                                      ,@(make-syntactic-closure-list syn-env
                                                                     identifiers
                                                                     (cddr exp))))
                                  ,(make-syntactic-closure syn-env
                                                           '()
                                                           (cadar bindings))))))))))
        (when-expander (lambda (syn-env exp)
                         (let ((test (make-syntactic-closure syn-env
                                                             '()
                                                             (cadr exp)))
                               (body (make-syntactic-closure-list syn-env
                                                                  '()
                                                                  (cddr exp))))
                           (make-syntactic-closure
                            scheme-syntactic-environment '()
                            `(if ,test (begin ,@body))))))
        (unless-expander (lambda (syn-env exp)
                           (let ((test (make-syntactic-closure syn-env
                                                               '()
                                                               (cadr exp)))
                                 (body (make-syntactic-closure-list syn-env
                                                                    '()
                                                                    (cddr exp))))
                             (make-syntactic-closure
                              scheme-syntactic-environment '()
                              `(if (not ,test) (begin ,@body)))))))
    `((and    . ,and-expander)
      (let    . ,let-expander)
      (let*   . ,let*-expander)
      (or     . ,or-expander)
      (when   . ,when-expander)
      (unless . ,unless-expander))))
