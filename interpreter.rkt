#lang racket

(require
  "lexer.rkt"
  "parser.rkt"
  "pcf.rkt")

(define (loop)
  (display "~> ")
  (let ([prog (read-line)])
    (if (eof-object? prog) (void) (begin (displayln (evaluate (parse prog))) (loop)))))

(loop)