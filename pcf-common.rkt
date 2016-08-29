#lang racket

(require "helpers.rkt")

(provide (all-defined-out))

(define succ? (is? 'succ))
(define ifz? (is? 'ifz))
(define lam? (is? 'lam))
(define ap? (is? 'ap))
(define fix? (is? 'fix))

(define z 'z)

(define-syntax-rule (succ x)
  `(succ x))

(define (pred x) (if (succ? x) (cadr x) x))

(define-syntax-rule (ifz e e0 e1) `(ifz e e0 e1))
(define ifz-expr cadr)
(define ifz-e0 caddr)
(define ifz-e1 cadddr)

(define-syntax-rule (lam x e) (recursive-rename `(lam x e)))
(define lam-arg cadr)
(define lam-body caddr)

(define-syntax-rule (ap e1 e2) `(ap e1 e2))
(define ap-e1 cadr)
(define ap-e2 caddr)

(define-syntax-rule (fix e) `(fix e))
(define fix-expr cadr)

(define (rename-lam-var lam-expr)
  (let ((new-sym (gensym)))
    (lam ,new-sym ,(substitute (lam-arg lam-expr) new-sym (lam-body lam-expr)))))

(define (recursive-rename lam-expr)
  (cond ((empty? lam-expr) empty)
        ((lam? (car lam-expr)) (cons (rename-lam-var (car lam-expr))
                                     (recursive-rename (cdr lam-expr))))
        ((list? (car lam-expr)) (cons (recursive-rename (car lam-expr))
                                      (recursive-rename (cdr lam-expr))))
        (else (cons (car lam-expr) (recursive-rename (cdr lam-expr))))))