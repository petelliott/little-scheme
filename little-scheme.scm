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
    (eval (caddr form) (push-args (cadr form) args scope))))

;;; the repl, etc.

(define toplevel-scope
  `((car . ,car)
    (cdr . ,cdr)
    (cons . ,cons)
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (<= . ,<=)))

(define (repl)
  (define form (read))
  (if form
      (begin
        (write (eval form toplevel-scope))
        (newline)
        (repl))
      #f))
