#lang racket

(require
  "../lpcf.rkt")

(let-values ([(result S mu V) (step z)])
  (printf "1. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (succ (succ z)))])
  (printf "2. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (lam x x))])
  (printf "3. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ifz z z (lam x (succ x))))])
  (printf "4. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ifz (succ z) z (lam x (succ x))))])
  (printf "5. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ifz (ap (lam x x) (succ (succ z))) z (lam x (succ x))))])
  (printf "6. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ap (lam x x) z))])
  (printf "7. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ap (lam x x) (ap (lam x (succ x)) z)))])
  (printf "8. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ap (ap (lam y (lam x (succ y))) (succ (succ z))) z))])
  (printf "9. ~a\t|| µ: ~a\n" result mu))

(let-values ([(result S mu V) (step (ap (ap (lam y (lam x (succ y))) (succ (succ z))) (ap (lam x x) z)))])
  (printf "10. ~a\t|| µ: ~a\n" result mu))

;; Simple recursion test
(define recursive (fix (lam y (lam x (ifz x z (lam w (succ (ap y w))))))))

(newline)
(evaluate (ap ,recursive z))

(newline)
(evaluate (ap ,recursive (succ z)))