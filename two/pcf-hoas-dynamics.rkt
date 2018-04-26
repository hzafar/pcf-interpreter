#lang racket

(provide (all-defined-out))

; 19.2a-c
(define (val? e)
  (match e
    [(? procedure? f) #t]
    ['z #t]
    [`(succ ,(? val? v)) #t]
    [_ #f]))

(define (step e)
  (printf "step: ~a\n" e)
  (match e
    [(? val? v) v]
    [`(succ ,e) `(succ ,(step e))]
    [`(ifz z ,e0 ,e1) (displayln "1") e0]
    [`(ifz (succ ,(? val? v)) ,e0 ,(? procedure? es)) (displayln "2") (syntax->datum (es v))]
    [`(ifz ,e ,e0 ,es) (displayln "3") `(ifz ,(step e) ,e0 ,es)]
    [`(ap ,(? procedure? fn) ,(? val? arg)) (fn arg)]
    [`(ap ,(? procedure? fn) ,e) `(ap ,fn ,(step e))]
    [`(ap ,e1 ,e2) `(ap ,(step e1) ,e2)]
    [`(fix ,e) (e `(fix ,e))]
    [_ (error "Ill-formed expression: " e)]))

(define (evaluate e)
    (if (val? e) e
        (evaluate (step e))))