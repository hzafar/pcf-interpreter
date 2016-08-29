#lang racket

(require
  "helpers.rkt"
  "pcf-common.rkt"
  (rename-in "spcf-common.rkt"
             (evaluate cmn:evaluate)))

(provide (all-defined-out)
         (all-from-out "spcf-common.rkt"))

(define (step e [S '()] [F (hash)])
  (cond ((val? e S) (values e S F))
        ((succ? e)
         (let-values ([(result S-prime F-prime) (step (pred e) S F)])
           (values (succ ,result) S F)))
        ((ifz? e)
         (cond ((eq? (ifz-expr e) z) (values (ifz-e0 e) S F))
               ((and (val? (ifz-expr e) S) (succ? (ifz-expr e)))
                (values
                  (substitute (lam-arg (ifz-e1 e)) (pred (ifz-expr e)) (lam-body (ifz-e1 e)))
                  S F))
               (else
                 (let-values ([(result S-prime F-prime) (step (ifz-expr e) S F)])
                   (values (ifz ,result ,(ifz-e0 e) ,(ifz-e1 e)) S F)))))
        ((ap? e)
         (cond ((val? (ap-e1 e) S)
                (if (val? (ap-e2 e) S)
                    (values
                      (substitute (lam-arg (ap-e1 e)) (ap-e2 e) (lam-body (ap-e1 e)))
                      S F)
                    (let-values ([(result S-prime F-prime) (step (ap-e2 e) S F)])
                      (values (ap ,(ap-e1 e) ,result) S F))))
               (else
                 (let-values ([(result S-prime F-prime) (step (ap-e1 e) S F)])
                   (values (ap ,result ,(ap-e2 e)) S F)))))
        ((fix? e)
         (values
           (substitute (lam-arg (fix-expr e))
                       (fix ,(lam ,(lam-arg (fix-expr e))
                                  ,(rename-lam-var (lam-body (fix-expr e)))))
                       (lam-body (fix-expr e)))
            S F))
        ((if-sym? e)
         (if (and (quoted? (if-sym-expr e)) (memq (quoted-sym (if-sym-expr e)) S))
             (if ((hash-ref F (if-sym-symbol e)) (quoted-sym (if-sym-expr e)))
                 (values (if-sym-e1 e) S F)
                 (values (if-sym-e2 e) S F))
             (let-values ([(result S-prime F-prime) (step (if-sym-expr e) S F)])
               (values (if-sym ,(if-sym-symbol e) ,result ,(if-sym-e1 e) ,(if-sym-e2 e)) S F))))
        ((new? e)
         (if (val? (new-expr e) S)
             (values (new-expr e) S F)
             (begin
               (let-values ([(s2 e2 S2) (adjoin (new-sym e) (new-expr e) S)])
                 (let-values ([(result S-prime F-prime)
                               (step e2 S2 (hash-set F s2 (lambda (x) (eq? x s2))))])
                   (values (new ,s2 ,result) S F))))))
        (else (error "Ill-formed expression: " e))))

(define (evaluate e [S '()] [F '#hash()])
  (cmn:evaluate step e S F))