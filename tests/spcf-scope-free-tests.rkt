#lang racket

(require
  rackunit
  rackunit/log
  "../spcf-scope-free.rkt")

(let-values ([(result S F) (step '(new A (new B (if-sym A (quoted B) z (succ z)))))])
  (check-equal? result '(new B (if-sym A (quoted B) z (succ z))))
  (check-equal? S '(A))
  (check-equal? (hash-keys F) '(A))

  (let-values ([(result2 S2 F2) (step result S F)])
    (check-equal? result2 '(if-sym A (quoted B) z (succ z)))
    (check-equal? (sort S2 string<? #:key symbol->string) '(A B))
    (check-equal? (sort (hash-keys F2) string<? #:key symbol->string) '(A B))

    (let-values ([(result3 S3 F3) (step result2 S2 F2)])
      (check-equal? result3 '(succ z))
      (check-equal? S3 S2)
      (check-equal? (hash-keys F3) (hash-keys F2)))))

;; Cannot reference a symbol that has not been declared
(let-values ([(result S F) (step '(new A (if-sym A (quoted B) z (succ z))))])
  (check-equal? result '(if-sym A (quoted B) z (succ z)))
  (check-equal? S '(A))
  (check-equal? (hash-keys F) '(A))
  (check-exn exn:fail? (lambda () (step result S F))))

(let-values ([(result S F) (step '(new B (quoted B)))])
  (check-equal? result '(quoted B))
  (check-equal? S '(B))
  (check-equal? (hash-keys F) '(B)))

(check-equal? (evaluate '(new A (ap (lam x (if-sym A x z (succ z))) (quoted A)))) 'z)
(check-equal? (evaluate '(new A (ap (lam x (new B (if-sym B x z (succ z)))) (quoted A)))) '(succ z))

;; Symbol renaming
(step '(new A (if-sym A (quoted A) z (succ z))) '(A) (hash-set (hash) 'A (lambda (x) (eq? x 'A))))

;; Not valid with scoped dynamics, but possible with scope-free
(check-equal? (evaluate '(ap (lam x (if-sym A x z (succ z))) (new A (quoted A)))) 'z)

;; Not valid even with scope-free dynamics
;; ???

(test-log #:display? #t #:exit? #t)