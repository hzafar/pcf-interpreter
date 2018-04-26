#lang racket

(require
  "parser.rkt"
  "convert.rkt"
  "../pcf-dynamics.rkt")

;; Interprets expressions given in the book's syntax. For examples, see tests/pcf-parser-tests.rkt.

(define (loop)
  (display "~> ")
  (let ([prog (read-line)])
    (if (eof-object? prog) (void) (begin (displayln (num (evaluate (ast->hoas (parse prog))))) (loop)))))

(loop)