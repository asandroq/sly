;;;
;;; The Sly Scheme library
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

;; R5RS 6.1

(define (equal? x y)
  (cond
   ((and (pair? x)
         (pair? y))
    (and (eq? (car x)
              (car y))
         (equal? (cdr x)
                 (cdr y))))
   (else
    (eq? x y))))

;; R5RS 6.3.2

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define list (lambda args args))

(define (length lst)
  (let loop ((l lst)
             (i 0))
    (cond
     ((null? l) i)
     ((pair? l) (loop (cdr l) (add1 i)))
     (else (error "length applied to non-list" lst l)))))

(define (reverse lst)
  (let loop ((l lst)
             (res '()))
    (cond
     ((null? l) res)
     ((pair? l) (loop (cdr l) (cons (car l) res)))
     (else (error "reverse applied to non-list" lst)))))

(define (list-tail ls k)
  (if (= k 0)
      ls
      (list-tail (cdr ls) (sub1 k))))

(define (list-ref ls k)
  (car (list-tail ls k)))

(define (memq x lst)
  (cond
   ((null? lst) #f)
   ((eq? x (car lst)) lst)
   (else (memq x (cdr lst)))))

(define (memv x lst)
  (cond
   ((null? lst) #f)
   ((eqv? x (car lst)) lst)
   (else (memq x (cdr lst)))))

(define (member x lst)
  (cond
   ((null? lst) #f)
   ((equal? x (car lst)) lst)
   (else (member x (cdr lst)))))

;; R5RS 6.4

;; this is not a complete map implementation
(define (map proc l)
  (let loop ((l l)
             (res '()))
    (cond
     ((null? l) res)
     ((pair? l) (loop (cdr l)
                      (cons (proc (car l))
                            res)))
     (else (error "map applied to non-list")))))

;; Error handling

(define error
  (lambda args
    (let loop ((l args))
      (if (null? l)
          (abort)
          (begin
            (write (car l))
            (write " ")
            (loop (cdr l)))))))

;; Temporary bizarre solution

(define gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (add1 counter))
      (string->symbol
       (string-append "sly-gensym-"
                      (number->string counter))))))

