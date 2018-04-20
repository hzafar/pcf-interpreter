#lang racket

(require "helpers.rkt")

(provide (all-defined-out))

(define (rename-lam-var lam-expr)
  (let ((new-sym (gensym)))
    `(lam ,new-sym ,(substitute (cadr lam-expr) new-sym (caddr lam-expr)))))

(define (recursive-rename lam-expr)
  (cond ((empty? lam-expr) empty)
        ((equal? 'lam (car lam-expr)) (cons (rename-lam-var (car lam-expr))
                                     (recursive-rename (cdr lam-expr))))
        ((list? (car lam-expr)) (cons (recursive-rename (car lam-expr))
                                      (recursive-rename (cdr lam-expr))))
        (else (cons (car lam-expr) (recursive-rename (cdr lam-expr))))))