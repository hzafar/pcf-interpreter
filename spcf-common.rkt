#lang racket

(require
  "helpers.rkt"
  "pcf-common.rkt"
  (prefix-in pcf: "pcf.rkt"))

(provide (all-defined-out)
         (all-from-out "pcf-common.rkt"))

(define new? (is? 'new))
(define quoted? (is? 'quoted))
(define if-sym? (is? 'if-sym))

; 31.6a
(define (val? e S)
  (or (pcf:val? e)
      (and (quoted? e) (memq (quoted-sym e) S))))

(define-syntax-rule (new sym e) `(new sym e))
(define new-sym cadr)
(define new-expr caddr)

(define-syntax-rule (quoted sym) `(quoted sym))
(define quoted-sym cadr)

(define-syntax-rule (if-sym sym e e1 e2) `(if-sym sym e e1 e2))
(define if-sym-symbol cadr)
(define if-sym-expr caddr)
(define if-sym-e1 cadddr)
(define (if-sym-e2 e) (car (cddddr e)))

;; Avoid an infinite loop in the evaluator if there is a (particular type of)
;; type-incorrect program. May only be relevant for the scoped dynamics.
(define (not-reducible? step-fn e [S '()] [F '#hash()])
  (define (list-eq? l1 l2)
    (cond ((or (not (list? l1)) (not (list? l2))) #f)
          ((empty? l1) (empty? l2))
          ((and (list? (car l1)) (list? (car l2)))
           (and (list-eq? (car l1) (car l2))
                (list-eq? (cdr l1) (cdr l2))))
          ((eq? (car l1) (car l2)) (list-eq? (cdr l1) (cdr l2)))
          (else #f)))
  (let-values ([(result S-prime F-prime) (step-fn e S F)])
    (or (val? e S)
        (list-eq? e result))))

(define (evaluate step-fn e [S '()] [F (hash)])
  (if (not-reducible? step-fn e S F) e
      (let-values ([(result S-prime F-prime) (step-fn e S F)])
        (evaluate step-fn result S-prime F-prime))))