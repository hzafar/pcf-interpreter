#lang racket

(provide ast->debruijn debruijn->hoas ast->hoas)

;; Working but not yet ideal abstract syntax transformations.

;; Convert quoted list 'lam forms to de Brujin indices.
(define (ast->debruijn lexpr)
  (define (lam-helper expr env)
    (define new-env (apply hash (flatten (hash-map env ( λ (k v) (list k (add1 v)))))))
    (list 'lamdb (helper (caddr expr) (hash-set new-env (cadr expr) 1))))
  (define (helper expr env)
    (cond [(empty? expr) empty]
          [(cons? expr)
           (if (equal? (car expr) 'lam)
               (lam-helper expr env)
               (cons (helper (car expr) env) (helper (cdr expr) env)))]
          [(hash-ref env expr #f) (hash-ref env expr)]
          [else expr]))
  (helper lexpr (hash)))

;; Convert de Bruijn indices to HOAS.
(define (debruijn->hoas lexpr)
  (define (lamdb-helper expr env)
    (define new-env (apply hash (append* (hash-map env ( λ (k v) (list (add1 k) v))))))
    (λ (x) (helper (cadr expr) (hash-set new-env 1 x))))
  (define (helper expr env)
    (cond [(empty? expr) empty]
          [(cons? expr)
           (if (equal? (car expr) 'lamdb)
               (lamdb-helper expr env)
               (cons (helper (car expr) env) (helper (cdr expr) env)))]
          [(hash-ref env expr #f) (hash-ref env expr)]
          [else expr]))
  (helper lexpr (hash)))

;; Convert quoted list to HOAS in one step.
(define (ast->hoas expr)
  (define (rename-key key)
    (λ (k v) (list (if (equal? k key) (gensym) k) v)))
  (define (lam-helper lexpr env)
    (define new-env (apply hash (append* (hash-map env (rename-key (cadr lexpr))))))
    (λ (x) (helper (caddr lexpr) (hash-set new-env (cadr lexpr) x))))
  (define (helper expr env)
    (cond [(empty? expr) empty]
          [(cons? expr)
           (if (equal? (car expr) 'lam)
               (lam-helper expr env)
               (cons (helper (car expr) env) (helper (cdr expr) env)))]
          [(hash-ref env expr #f) (hash-ref env expr)]
          [else expr]))
  (helper expr (hash)))