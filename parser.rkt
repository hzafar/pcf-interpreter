#lang racket

(require
  data/applicative
  data/monad
  "lexer.rkt"
  megaparsack
  megaparsack/parser-tools/lex)

(provide parse)

;; Throws away type info atm.

(define type-arr/p (syntax/p (do (token/p 'FNARROW) [t <- type/p] (pure t))))
(define type-nat/p (syntax/p (do (token/p 'TYPENAT) (pure 'nat))))
(define type/p
    (syntax/p (do [t1 <- type-nat/p]
                  [ts <- (many*/p type-arr/p)]
                  (pure (list* t1 ts)))))

(define identifier/p (syntax/p (token/p 'IDENTIFIER)))
(define zero/p (syntax/p (do (token/p 'ZERO) (pure 'z))))
(define succ/p
  (syntax/p
    (do
      (token/p 'SUCC)
      (token/p 'OPENPAREN)
      [e <- expr/p]
      (token/p 'CLOSEPAREN)
      (pure (list 'succ e)))))

(define lambda/p
  (syntax/p
    (do
      (token/p 'LAMBDA)
      (token/p 'OPENPAREN)
      [var <- identifier/p]
      (token/p 'COLON)
      type/p
      (token/p 'CLOSEPAREN)
      [body <- expr/p]
      (pure (list 'lam var body)))))

(define ifz/p
  (syntax/p
    (do
      (token/p 'IFZ)
      [e <- expr/p]
      (token/p 'OPENBRACE)
      (token/p 'ZERO)
      (token/p 'HOOKEDARROW)
      [e0 <- expr/p]
      (token/p 'BAR)
      (token/p 'SUCC)
      (token/p 'OPENPAREN)
      [var <- identifier/p]
      (token/p 'CLOSEPAREN)
      (token/p 'HOOKEDARROW)
      [es <- expr/p]
      (pure (list 'ifz e e0 (list 'lam var es))))))

(define fix/p
  (syntax/p
    (do
      (token/p 'FIX)
      [var <- identifier/p]
      (token/p 'COLON)
      type/p
      (token/p 'IS)
      [e <- expr/p]
      (pure (list 'fix (list 'lam var e))))))

(define ap/p ;  =(
  (syntax/p
    (do
      (token/p 'AP)
      (token/p 'OPENPAREN)
      [fn <- expr/p]
      (token/p 'COMMA)
      [arg <- expr/p]
      (token/p 'CLOSEPAREN)
      (pure (list 'ap fn arg)))))

(define expr/p (or/p zero/p succ/p identifier/p lambda/p ifz/p fix/p ap/p))

(define (parse s)
    (syntax->datum (parse-result! (parse-tokens expr/p (lex s)))))

(parse "z")
(parse "s(s(z))")
(parse "λ(x:nat) x")
(parse "ifz s(z){z=>z|s(x)=>x}")
(parse "fix y:nat is λ(x:nat) ifz x{z=>z|s(w)=>ap(y, w)}")
(parse "ap(λ(x:nat) x, z)")