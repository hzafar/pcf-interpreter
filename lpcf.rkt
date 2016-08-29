#lang racket

(require
  "helpers.rkt"
  "pcf-common.rkt")

(provide (all-defined-out)
         (all-from-out "pcf-common.rkt"))

(define @? (is? '@))

(define-syntax-rule (@ s) `(@ s))
(define @-sym cadr)

(define (val? e S)
  (or (lam? e)
      (eq? z e)
      (and (succ? e) (@? (pred e)) (memq (@-sym (pred e)) S))))

(define (step e [S '()] [mu (hash)] [visited (hash)])
  (cond ((val? e S) (values e S mu visited))
        ((succ? e) ; e is not a symbol ref
         (let ((sym (gensym)))
           (values (succ ,(@ ,sym))
                   (cons sym S)
                   (hash-set mu sym (pred e))
                   visited)))
        ((ifz? e)
         (cond ((eq? (ifz-expr e) z) (values (ifz-e0 e) S mu visited))
               ((and (val? (ifz-expr e) S) (succ? (ifz-expr e)) (@? (pred (ifz-expr e))))
                (values
                  (substitute (lam-arg (ifz-e1 e)) (pred (ifz-expr e)) (lam-body (ifz-e1 e)))
                  S mu visited))
               (else
                 (let-values ([(result S-prime mu-prime v2) (step (ifz-expr e) S mu visited)])
                   (values (ifz ,result ,(ifz-e0 e) ,(ifz-e1 e))
                           S-prime mu-prime v2)))))
        ((ap? e)
         (cond ((val? (ap-e1 e) S)
                (let ((sym (gensym)))
                  (values
                    (substitute (lam-arg (ap-e1 e)) (@ ,sym) (lam-body (ap-e1 e)))
                    (cons sym S)
                    (hash-set mu sym (ap-e2 e))
                    visited)))
               (else
                 (let-values ([(result S-prime mu-prime v2) (step (ap-e1 e) S mu visited)])
                   (values (ap ,result ,(ap-e2 e))
                           S-prime mu-prime v2)))))
        ((fix? e)
         (let ((sym (gensym)))
           (values (@ ,sym)
                   (cons sym S)
                   (hash-set mu sym (substitute (lam-arg (fix-expr e))
                                                (@ ,sym)
                                                (lam-body (fix-expr e))))
                   visited)))
        ((@? e)
         (let ((@-expr (hash-ref mu (@-sym e)))) ; let it error if key not found
           (cond ((val? @-expr S)
                  (values @-expr S mu (hash-set visited (@-sym e) #f)))
                 ((hash-ref visited (@-sym e) #f) (error "Cannot make progress:" @-expr))
                 (else
                     (let-values ([(result S-prime mu-prime v2) (step @-expr S mu (hash-set visited (@-sym e) #t))])
                       (values e S-prime (hash-set mu-prime (@-sym e) result) v2))))))
        (else (error "Ill-formed expression: " e))))

(define (evaluate e [S '()] [mu (hash)] [visited (hash)])
 (if (val? e S)
     (begin
       (printf "~a\t|| µ: ~a\n" e mu)
        e)
     (let-values ([(result S-prime mu-prime v2) (step e S mu visited)])
       (evaluate result S-prime mu-prime v2))))