#lang racket

(provide (all-defined-out))

(define z 'z)

;; Define some macros that will compile PCF expressions to HOAS representations.
;; Really, lam and fix are the relevant ones, as only those bind any variables.

(define-syntax-rule (succ e) `(succ ,e))
(define-syntax-rule (ifz e e0 es) `(ifz ,e ,e0 ,es))
(define-syntax-rule (lam x body) (lambda (x) body))
(define-syntax-rule (ap fn arg) `(ap ,fn ,arg))
(define-syntax-rule (fix y e) `(fix ,(lambda (y) e)))

; 19.2a-c
(define (val? e)
  (match e
    [(? procedure? f) #t]
    ['z #t]
    [`(succ ,(? val? v)) #t]
    [_ #f]))

(define (step e)
  (match e
    [(? val? v) v]
    [`(succ ,e) `(succ ,(step e))]
    [`(ifz z ,e0 ,e1) e0]
    [`(ifz (succ ,(? val? v)) ,e0 ,(? procedure? es)) (es v)]
    [`(ifz ,e ,e0 ,es) `(ifz ,(step e) ,e0 ,es)]
    [`(ap ,(? procedure? fn) ,(? val? arg)) (fn arg)]
    [`(ap ,(? procedure? fn) ,e) `(ap ,fn ,(step e))]
    [`(ap ,e1 ,e2) `(ap ,(step e1) ,e2)]
    [`(fix ,e) (e `(fix ,e))]
    [_ (error "Ill-formed expression: " e)]))

(define (evaluate e)
    (if (val? e) e
        (evaluate (step e))))