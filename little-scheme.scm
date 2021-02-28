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

;;; implementation of eval

(define (is-syntax? sym form)
  (and (pair? form)
       (eq? (car form) sym)))

(define (eval form scope)
  (cond
   ((is-syntax? 'if form) (eval-if form scope))
   ((is-syntax? 'lambda form) (eval-lambda form scope))
   ((is-syntax? 'quote form) (cadr form))
   ((pair? form) (eval-fun form scope))
   ((symbol? form) (eval-ref form scope))
   (else form)))


(define (eval-fun form scope)
  (apply (eval (car form) scope)
         (map (lambda (arg) (eval arg scope)) (cdr form))))

(define (eval-ref form scope)
  (cond
   ((null? scope) #f)
   ((eq? form (caar scope)) (cdar scope))
   (else (eval-ref form (cdr scope)))))

;;; built in syntax
(define (eval-if form scope)
  (if (eval (cadr form) scope)
      (eval (caddr form) scope)
      (eval (cadddr form) scope)))

(define (push-args names args scope)
  (if (null? names)
      scope
      (push-args (cdr names) (cdr args)
                 (cons (cons (car names) (car args)) scope))))

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

(define (top-eval form)
  (eval form toplevel-scope))

;;; examples

(define (add-example)
  (top-eval '((lambda (a b) (+ a b)) 5 6)))

(define (fib-example)
  (top-eval '((lambda (f n) (f f n))
               (lambda (self n)
                 (if (<= n 1)
                     n
                     (+ (self self (- n 1)) (self self (- n 2)))))
               12)))
