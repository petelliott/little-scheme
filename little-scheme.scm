;;;; little-scheme is a minimal (not yet) self hosted scheme interpreter.
;;;;
;;;; Copyright (C) 2021  Peter Elliott
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; scheme library functions that we reimplemented

(define (caar pair)
  (car (car pair)))

(define (cadr pair)
  (car (cdr pair)))

(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (map proc (cdr lst)))))

;; push a key-value pair to an alist
(define (acons key value alist)
  (cons (cons key value) alist))

;;; utilties

;; checks if form is a pair whose car is sym
(define (is-syntax? sym form)
  (and (pair? form)
       (eq? (car form) sym)))

;; lines up arguments and their variables names, then pushes them to the scope
(define (push-args names args scope)
  (cond
   ((and (null? names) (null? args)) scope)
   ((or (null? names) (null? args))
    (error "mismatch in number of arguments to function:" names args))
   (else (push-args (cdr names) (cdr args)
                    (acons (car names) (car args) scope)))))

;;; implementation of eval

(define (eval form scope)
  (cond
   ((is-syntax? 'if form)     (eval-if form scope))
   ((is-syntax? 'lambda form) (eval-lambda form scope))
   ((is-syntax? 'quote form)  (cadr form))
   ((is-syntax? 'begin form)  (eval-begin (cdr form) scope))
   ((pair? form)              (eval-fun form scope))
   ((symbol? form)            (eval-ref form scope))
   (else                      form)))

(define (eval-fun form scope)
  (apply (eval (car form) scope)
         (map (lambda (arg) (eval arg scope)) (cdr form))))

(define (eval-ref form scope)
  (cond
   ((null? scope) (error "variable does not exist:" form))
   ((eq? form (caar scope)) (cdar scope))
   (else (eval-ref form (cdr scope)))))

;;; built in syntax

(define (eval-if form scope)
  (if (eval (cadr form) scope)
      (eval (caddr form) scope)
      (eval (cadddr form) scope)))

(define (eval-lambda form scope)
  (lambda args
    (eval-begin (cddr form) (push-args (cadr form) args scope))))

;; eval-define returns a new scope, rather than a result
(define (eval-define form scope)
  (cond
   ((pair? (cadr form))
    (eval-define `(define ,(caadr form) (lambda ,(cdadr form) ,@(cddr form))) scope))
   ((symbol? (cadr form))
    ;; TODO make define bind recursivly
    (acons (cadr form) (eval (caddr form) scope) scope))
   (else (error "invalid name to define: " (cadr form)))))

;; Note: while most syntax functions take the whole form, begin takes the
;; cdr. this is because it is also used in lambda, and is recursive.
(define (eval-begin body scope)
  (cond
   ((null? body) (values))
   ((is-syntax? 'define (car body))
    (eval-begin (cdr body) (eval-define (car body) scope)))
   ((null? (cdr body)) (eval (car body) scope))
   (else
    (eval (car body) scope)
    (eval-begin (cdr body) scope))))

;;; the repl, etc.

(define toplevel-scope
  `((car . ,car)
    (cdr . ,cdr)
    (cons . ,cons)
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (<= . ,<=)))

(define (toplevel-eval form)
  (eval form toplevel-scope))

(define (scoped-repl render-prompt render-result scope)
  (define form (begin
                 (render-prompt)
                 (read)))
  (cond
   ((eof-object? form) (values))
   ((is-syntax? 'exit form) (values))
   ((is-syntax? 'define form)
    (scoped-repl render-prompt render-result (eval-define form scope)))
   (else
    (render-result (eval form scope))
    (scoped-repl render-prompt render-result scope))))

(define (repl)
  (scoped-repl (lambda () (display "> "))
               (lambda (result)
                 (display "=> ")
                 (write result)
                 (newline))
               toplevel-scope))


(define (file-repl)
  (scoped-repl (lambda () #f)
               (lambda (result) #f)
               toplevel-scope))
