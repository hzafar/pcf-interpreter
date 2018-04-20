#lang racket

(require
  parser-tools/lex
  (rename-in parser-tools/lex-sre
             [or :or]
             [+ :+]))

(provide lex)

(define-tokens tkns (IDENTIFIER))
(define-empty-tokens tkns* (OPENPAREN CLOSEPAREN OPENBRACE CLOSEBRACE FNARROW HOOKEDARROW
                            TYPENAT ZERO SUCC IFZ BAR LAMBDA COLON FIX IS WHITESPACE
                            AP COMMA))

(define pcf-lexer
  (lexer-src-pos
    [#\( (token-OPENPAREN)]
    [#\) (token-CLOSEPAREN)]
    [#\{ (token-OPENBRACE)]
    [#\} (token-CLOSEBRACE)]
    ["->" (token-FNARROW)]
    ["=>" (token-HOOKEDARROW)]
    ["nat" (token-TYPENAT)]
    ["z" (token-ZERO)]
    ["s" (token-SUCC)]
    ["ifz" (token-IFZ)]
    ["fix" (token-FIX)]
    ["is" (token-IS)]
    ["ap" (token-AP)]
    ["," (token-COMMA)]
    [#\| (token-BAR)]
    [#\λ (token-LAMBDA)]
    [#\: (token-COLON)]
    [(:+ alphabetic) (token-IDENTIFIER (string->symbol lexeme))]
    [(:or whitespace blank) (token-WHITESPACE)]
    [(eof) eof]
))

(define (lex str)
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([t (pcf-lexer in)])
    (cond [(eof-object? (position-token-token t)) '()]
          [(symbol=? (token-name (position-token-token t)) 'WHITESPACE) (loop (pcf-lexer in))]
          [else (cons t (loop (pcf-lexer in)))])))