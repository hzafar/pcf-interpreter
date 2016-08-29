#lang racket

(require
  "helpers.rkt"
  "pcf-common.rkt")

(provide (all-defined-out)
         (all-from-out "pcf-common.rkt"))

; 19.2a-c
(define (val? e)
  (or (lam? e)
      (eq? z e)
      (and (succ? e) (val? (cadr e)))))

(define (step e)
  (cond ((val? e) e)
        ((succ? e) (succ ,(step (pred e)))) ; 19.3a
        ((ifz? e)
         (cond ((eq? (ifz-expr e) z) (ifz-e0 e)) ; 19.3c
               ((and (val? (ifz-expr e)) (succ? (ifz-expr e)))
                (substitute (lam-arg (ifz-e1 e)) (pred (ifz-expr e)) (lam-body (ifz-e1 e)))) ; 19.3d
               (else (ifz ,(step (ifz-expr e)) ,(ifz-e0 e) ,(ifz-e1 e))))); 19.3b
        ((ap? e)
         (cond ((val? (ap-e1 e))
                (if (val? (ap-e2 e))
                    (substitute (lam-arg (ap-e1 e)) (ap-e2 e) (lam-body (ap-e1 e))) ; 19.3g
                    (ap ,(ap-e1 e) ,(step (ap-e2 e))))) ; 19.3f
               (else (ap ,(step (ap-e1 e)) ,(ap-e2 e))))) ; 19.3e
        ((fix? e)
         (substitute (lam-arg (fix-expr e))
                     (fix ,(lam ,(lam-arg (fix-expr e))
                                ,(rename-lam-var (lam-body (fix-expr e)))))
                     (lam-body (fix-expr e)))) ; 19.3h
        (else (error "Ill-formed expression: " e))))

(define (evaluate e)
    (if (val? e) e
        (evaluate (step e))))