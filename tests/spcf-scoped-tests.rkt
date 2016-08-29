#lang racket

(require
  rackunit
  rackunit/log
  "../spcf-scoped.rkt")

(let-values ([(result S F) (step (new A (new B (if-sym A (quoted B) z (succ z)))))])
  (check-equal? result (new A (new B (succ z))))
  (check-equal? S '())
  (check-equal? F '#hash()))

(let-values ([(result S F) (step (new A (new B (succ z))))])
  (check-equal? result (new A (succ z)))
  (check-equal? S '())
  (check-equal? F '#hash()))

(let-values ([(result S F) (step (new A (succ z)))])
  (check-equal? result (succ z))
  (check-equal? S '())
  (check-equal? F '#hash()))

(let-values ([(result S F) (step (new A (ap (lam x (if-sym A x z (succ z))) (quoted A))))])
  (check-equal? result (new A (if-sym A (quoted A) z (succ z))))
  (check-equal? S '())
  (check-equal? F '#hash()))

;; Cannot reference a symbol that has not been declared
(check-exn exn:fail? (lambda () (step (new A (if-sym A (quoted B) z (succ z))))))

(check-equal? (evaluate (new A (ap (lam x (if-sym A x z (succ z))) (quoted A)))) z)
(check-equal? (evaluate (new A (ap (lam x (new B (if-sym B x z (succ z)))) (quoted A)))) (succ z))

(define even-or-odd
  (ap (fix (lam f (lam s (lam x (ifz x (if-sym A s z (succ z))
                                       (lam w (ap (ap f (if-sym A s (quoted B) (quoted A))) w)))))))
      (quoted A)))

(check-equal? (evaluate (new A (new B (ap ,even-or-odd z)))) z)
(check-equal? (evaluate (new A (new B (ap ,even-or-odd (succ z))))) (succ z))
(check-equal? (evaluate (new A (new B (ap ,even-or-odd (succ (succ z)))))) z)

;; Symbol renaming
(step (new A (if-sym A (quoted A) z (succ z))) '(A) (hash-set (hash) 'A (lambda (x) (eq? x 'A))))

(test-log #:display? #t #:exit? #t)