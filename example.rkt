#lang racket/base

(require brag/support)
(require "lexer.rkt" "parser.rkt" "expander.rkt")

(define filename "examples/cpu.ahdl")
(define example-file (open-input-file filename))
(define token-thunk (tokenize filename example-file))
(define (iter n)
  (let ([token (n)])
    (if (eq? token eof)
      '()
      (begin (displayln token) (iter n)))))


;; (iter token-thunk)

(define parsed (parse token-thunk))
(print (syntax->datum parsed))
