#lang racket

(require
  "../pcf.rkt"
  rackunit
  rackunit/log)

(check-equal? (val? z) #t)
(check-equal? (val? (succ (succ (succ z)))) #t)
(check-equal? (val? (lam x (succ (succ x)))) #t)
(check-equal? (val? (ap (lam x (succ x)) (succ z))) #f)

(check-equal? (step z) z)
(check-equal? (step (succ (succ z))) (succ (succ z)))
(check-equal? (step (lam x x)) (lam x x))

(check-equal? (step (ifz z z (lam x (succ x)))) z)
(check-equal? (step (ifz (succ z) z (lam x (succ x)))) (succ z))
(check-equal? (step (ifz (ap (lam x x) (succ (succ z))) z (lam x (succ x))))
              (ifz (succ (succ z)) z (lam x (succ x))))

(check-equal? (step (ap (lam x x) z)) z)
(check-equal? (step (ap (lam x x) (ap (lam x (succ x)) z))) (ap (lam x x) (succ z)))
(check-equal? (step (ap (ap (lam y (lam x (succ y))) (succ (succ z))) z))
              (ap (lam x (succ (succ (succ z)))) z))
(check-equal? (step (ap (ap (lam y (lam x (succ y))) (succ (succ z))) (ap (lam x x) z)))
              (ap (lam x (succ (succ (succ z)))) (ap (lam x x) z)))

;; Simple recursion test
(define recursive (fix (lam y (lam x (ifz x z (lam w (succ (ap y w))))))))

;(step (ap recursive z))
;(step (step (ap recursive z)))
;(step (step (step (ap recursive z))))
(check-equal? (evaluate (ap ,recursive z)) z)

;(step (ap recursive (succ z)))
;(step (step (ap recursive (succ z))))
;(step (step (step (ap recursive (succ z)))))
;(step (step (step (step (ap recursive (succ z))))))
;(step (step (step (step (step (ap recursive (succ z)))))))
;(step (step (step (step (step (step (ap recursive (succ z))))))))
(check-equal? (evaluate (ap ,recursive (succ z))) (succ z))

(test-log #:display? #t #:exit? #t)