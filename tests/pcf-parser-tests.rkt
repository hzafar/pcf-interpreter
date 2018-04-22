#lang racket

(require
  "../parser.rkt"
  "../pcf.rkt"
  rackunit
  rackunit/log)

(define ev (compose num evaluate parse))

(check-equal? (parse "z") 'z)
(check-equal? (parse "s(z)") '(succ z))
(check-equal? (parse "λ(x:nat) x") '(lam x x))
(check-equal? (parse "λ(x:nat) s(s(x))") '(lam x (succ (succ x))))
(check-equal? (parse "ap(λ(x:nat) x, z)") '(ap (lam x x) z))
(check-equal? (parse "ifz z {z=>z|s(x)=>s(x)}") '(ifz z z (lam x (succ x))))
(check-equal? (parse "fix y:nat->nat is λ(x:nat) ifz x {z=>z|s(w)=>s(ap(y, w))}")
              '(fix (lam y (lam x (ifz x z (lam w (succ (ap y w))))))))

(check-equal? (ev "z") 0)
(check-equal? (ev "s(s(s(z)))") 3)
(check-equal? (ev "ap(λ(x:nat) s(x), s(z))") 2)
(check-equal? (ev "ap(λ(x:nat) x, ap(λ(x:nat) s(x), z))") 1)
(check-equal? (ev "ap(ap(λ(y:nat) λ(x:nat) s(y), s(s(z))), z)") 3)
(check-equal? (ev "ifz s(z) {z=>z|s(x)=>s(x)}") 1)
(check-equal? (ev "ifz ap(λ(x:nat) x, s(s(z))) {z=>z|s(x)=>s(x)}") 2)
(check-equal? (ev "ap(fix y:nat->nat is λ(x:nat) ifz x {z=>z|s(w)=>s(ap(y, w))}, z)") 0)
(check-equal? (ev "ap(fix y:nat->nat is λ(x:nat) ifz x {z=>z|s(w)=>s(ap(y, w))}, s(z))") 1)

(test-log #:display? #t #:exit? #t)