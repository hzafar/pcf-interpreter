#lang racket

(require
  "../convert.rkt"
  rackunit
  rackunit/log)

(check-equal? (ast->debruijn '(lam x x)) '(lamdb 1))
(check-equal? (ast->debruijn '(lam x (lam y (ifz x y (lam w x))))) '(lamdb (lamdb (ifz 2 1 (lamdb 3)))))
(check-equal? (ast->debruijn '(ap (lam x (lam x (succ x))) z)) '(ap (lamdb (lamdb (succ 1))) z))

(define f1 (debruijn->hoas '(lamdb 1)))
(check-equal? (f1 42) 42)

(define f2 (debruijn->hoas '(lamdb (lamdb (ifz 2 1 (lamdb 3))))))
(check-equal? (cadr ((f2 'a) 'b)) 'a)
(check-equal? (caddr ((f2 'a) 'b)) 'b)
(check-equal? ((cadddr ((f2 'a) 'b)) 'c) 'a)

(define f3 (cadr (debruijn->hoas '(ap (lamdb (lamdb (succ 1))) z))))
(check-equal? ((f3 'z) '(succ (succ z))) '(succ (succ (succ z))))

(define g1 (ast->hoas '(lam x x)))
(check-equal? (g1 42) 42)

(define g2 (ast->hoas '(lam x (lam xx (ifz x xx (lam xxx x))))))
(check-equal? (cadr ((g2 'a) 'b)) 'a)
(check-equal? (caddr ((g2 'a) 'b)) 'b)
(check-equal? ((cadddr ((g2 'a) 'b)) 'c) 'a)

(define g3 (cadr (ast->hoas '(ap (lam x (lam x (lam x (succ x)))) z))))
(check-equal? (((g3 'z) 'z) '(succ (succ z))) '(succ (succ (succ z))))

(test-log #:display? #t #:exit? #t)