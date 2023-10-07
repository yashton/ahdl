#lang racket
(require brag/support)
(require "lexer.rkt" "parser.rkt")

(define filename "example_cpu.ahdl")
(define example-file (open-input-file filename))
(define token-thunk (tokenize filename example-file))
(define (iter n)
  (let ([token (n)])
    (if (eq? token eof)
      '()
      (begin (displayln token) (iter n)))))


;; (iter token-thunk)

(define another-stx (parse token-thunk))
(display (syntax->datum another-stx))
