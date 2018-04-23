#lang racket

(provide (all-defined-out))

(define (num e)
  (if (equal? e 'z) 0 (add1 (num (cadr e)))))

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

(define (substitute arg val expr)
  (cond ((and (symbol? expr) (symbol=? arg expr)) val)
        ((empty? expr) empty)
        ((list? (car expr))
         (cons (substitute arg val (car expr))
               (substitute arg val (cdr expr))))
        ((eq? (car expr) arg) (cons val (substitute arg val (cdr expr))))
        (else (cons (car expr) (substitute arg val (cdr expr))))))

(define (adjoin elem expr S)
  (if (memq elem S)
      (let ((s (gensym)))
        (values s (rename elem s expr) (cons s S)))
      (values elem expr (cons elem S))))

(define (rename old new expr)
  (cond ((empty? expr) empty)
        ((list? (car expr)) (cons (rename old new (car expr))
                                  (rename old new (cdr expr))))
        ((eq? (car expr) old) (cons new (rename old new (cdr expr))))
        (else (cons (car expr) (rename old new (cdr expr))))))