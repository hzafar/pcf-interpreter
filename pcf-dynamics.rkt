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

(define (num e)
  (if (procedure? e) e (if (equal? e 'z) 0 (add1 (num (cadr e))))))

(define (evaluate e)
    (if (val? e) e
        (evaluate (step e))))