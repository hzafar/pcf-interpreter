#lang racket

(require
  "helpers.rkt"
  "pcf-common.rkt")

(provide (all-defined-out)
         (all-from-out "pcf-common.rkt"))

; 19.2a-c
(define (val? e)
  (match e
    [`(lam ,e ,b) #t]
    ['z #t]
    [`(succ ,(? val? v)) #t]
    [_ #f]))

(define (step e)
  (match e
    [(? val? v) v]
    [`(succ ,e) `(succ ,(step e))]
    [`(ifz z ,e0 ,e1) e0]
    [`(ifz (succ ,(? val? v)) ,e0 (lam ,x ,b)) (substitute x v b)]
    [`(ifz ,e ,e0 ,es) `(ifz ,(step e) ,e0 ,es)]
    [`(ap (lam ,x ,b) ,(? val? arg)) (substitute x arg b)]
    [`(ap ,(? val? fn) ,e) `(ap ,fn ,(step e))]
    [`(ap ,e1 ,e2) `(ap ,(step e1) ,e2)]
    [`(fix (lam ,x ,b)) (substitute x `(fix (lam ,x ,(rename-lam-var b))) b)]
    [_ (error "Ill-formed expression: " e)]))

(define (evaluate e)
    (if (val? e) e
        (evaluate (step e))))