#lang racket

(require "helpers.rkt")

(provide (all-defined-out))

(define (spcf-val? e S)
  (match e
      [`(lam ,e ,b) #t]
      ['z #t]
      [`(succ ,e) (spcf-val? e S)] ; todo: possibly not correct, too lazy to check rn
      [`(quoted ,e) (memq e S)]
      [_ #f]))

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
    (or (spcf-val? e S)
        (list-eq? e result))))

(define (evaluate step-fn e [S '()] [F (hash)])
  (displayln e)
  (if (not-reducible? step-fn e S F) (begin (displayln "we're done!") e)
      (let-values ([(result S-prime F-prime) (step-fn e S F)])
        (evaluate step-fn result S-prime F-prime))))