#lang racket

(require
  "helpers.rkt"
  (rename-in "spcf-common.rkt"
             (evaluate cmn:evaluate)))

(provide (all-defined-out)
         (all-from-out "spcf-common.rkt"))


(define (step e [S '()] [F (hash)])
  (define S-val? (λ (x) (spcf-val? x S)))
  (define S-mem? (λ (x) (memq x S)))
  (match e
    [(? S-val? e) (values e S F)]
    [`(succ ,e)
     (let-values ([(result S-prime F-prime) (step e S F)])
       (values `(succ ,result) S-prime F-prime))]
    [`(ifz z ,e0 es) (values e0 S F)]
    [`(ifz (succ ,(? S-val? v)) e0 (lam ,x ,b)) (values (substitute x v b) S F)]
    [`(ifz ,e ,e0 ,es)
     (let-values ([(result S-prime F-prime) (step e S F)])
                   (values `(ifz ,result ,e0 ,es) S-prime F-prime))]
    [`(ap (lam ,x ,b) ,(? S-val? arg)) (values (substitute x arg b) S F)]
    [`(ap ,(? S-val? e1) ,e2)
     (let-values ([(result S-prime F-prime) (step e2 S F)])
       (values `(ap ,e1 ,result) S-prime F-prime))]
    [`(ap ,e1 ,e2)
     (let-values ([(result S-prime F-prime) (step e1 S F)])
       (values `(ap ,result ,e2) S-prime F-prime))]
    [`(fix (lam ,x ,b)) (values (substitute x `(fix (lam ,x ,(rename-lam-var b))) b) S F)]
    [`(if-sym ,sym (quoted ,(? S-mem? s)) ,e1 ,e2)
     (if ((hash-ref F sym) s) (values e1 S F) (values e2 S F))]
    [`(if-sym ,sym ,e ,e1 ,e2)
     (let-values ([(result S-prime F-prime) (step e S F)])
       (values `(if-sym ,sym ,result ,e1 ,e2) S-prime F-prime))]
     [`(new ,sym ,(? S-val? e)) (values e S F)] ; think these are wrong, check later
     [`(new ,sym ,e)
      (let-values ([(s2 e2 S2) (adjoin sym e S)])
        (let-values ([(result S-prime F-prime) (step e2 S2 (hash-set F s2 (lambda (x) (eq? x s2))))])
          (values `(new ,s2 ,result) S F)))]
     [_ (error "Ill-formed expression: " e)]))

(define (evaluate e [S '()] [F '#hash()])
  (cmn:evaluate step e S F))