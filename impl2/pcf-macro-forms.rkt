#lang racket

(provide (all-defined-out))

(define z 'z)

;; Define some macros that will compile PCF expressions to HOAS representations.
;; Really, lam and fix are the relevant ones, as only those bind any variables.

(define-syntax-rule (succ e) `(succ ,e))
(define-syntax-rule (ifz e e0 es) `(ifz ,e ,e0 ,es))
(define-syntax-rule (lam x body) ( λ (x) body))
(define-syntax-rule (ap fn arg) `(ap ,fn ,arg))
(define-syntax-rule (fix y e) `(fix ,( λ (y) e)))