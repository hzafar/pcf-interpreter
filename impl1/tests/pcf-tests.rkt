#lang racket

(require
  "../../pcf-dynamics.rkt"
  "../parser.rkt"
  "../convert.rkt"
  rackunit
  rackunit/log)

(define pcf-eval (compose ast->hoas parse))

(check-equal? (val? (pcf-eval "z")) #t)
(check-equal? (val? (pcf-eval "s(s(s(z)))")) #t)
(check-equal? (val? (pcf-eval "λ(x:nat) s(s(x))")) #t)
(check-equal? (val? (pcf-eval "ap(λ(x:nat) s(x), s(z))")) #f)

(check-equal? (step (pcf-eval "z")) 'z)
(check-equal? (step (pcf-eval "s(s(z))")) '(succ (succ z)))
(check-pred procedure? (step (pcf-eval "λ(x:nat) x")))

(check-equal? (step (pcf-eval "ifz z {z=>z|s(x)=>s(x)}")) 'z)
(check-equal? (step (pcf-eval "ifz s(z) {z=>z|s(x)=>s(x)}")) '(succ z))

(check-equal? (step (pcf-eval "ap(λ(x:nat) x, z)")) 'z)

;; Simple recursion test
(define recursive "fix y:nat->nat is λ(x:nat) ifz x {z=>z|s(w)=>s(ap(y, w))}")
(define recursive-ap (pcf-eval (string-append "ap(" recursive ",z)")))
(step (step recursive-ap))
(step (step (step recursive-ap)))
(check-equal? (evaluate recursive-ap) 'z)

(define recursive-ap-2 (pcf-eval (string-append "ap(" recursive ",s(z))")))
(step recursive-ap-2)
(step (step recursive-ap-2))
(step (step (step recursive-ap-2)))
(step (step (step (step recursive-ap-2))))
(step (step (step (step (step recursive-ap-2)))))
(step (step (step (step (step (step recursive-ap-2))))))
(check-equal? (evaluate recursive-ap-2) '(succ z))

(test-log #:display? #t #:exit? #t)