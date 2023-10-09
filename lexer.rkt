#lang racket
(require (prefix-in brag- brag/support))
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))

(provide tokenize)

(define (tokenize path ip)
  (port-count-lines! ip)
  (lexer-file-path path)
  (define my-lexer
    (lexer-src-pos
     ["\n"
      (brag-token 'NEWLINE lexeme #:skip? #t)]
     [(:seq "//" (:* (char-complement "\n")))
      (brag-token 'COMMENT lexeme #:skip? #t)]
     [(:: "/*" (complement (:: any-string "*/" any-string)) "*/")
      (brag-token 'COMMENT lexeme #:skip? #t)]
     [(:+ whitespace)
      (brag-token 'WHITESPACE lexeme #:skip? #t)]
     [(:or "let" "if" "else" "match" "when" "as" "data" "ctrl" "clock" "address"
           "signed" "unsigned" "type" "enum" "union" "struct" "encoding"
           "module" "bind" "'?" "'0" "'1")
      (brag-token lexeme lexeme)]
     [(:seq "\"" (complement "\"") "\"")
      (brag-token 'ASCII lexeme)]
     [(:seq (:+ (char-set "01?")) "'b")
      (brag-token 'BINARY lexeme)]
     [(:seq (:+ (char-set "01234567?")) "'o")
      (brag-token 'OCTAL lexeme)]
     [(:seq (:+ (char-set "0123456789")) "'d")
      (brag-token 'DECIMAL lexeme)]
     [(:seq (:+ (char-set "0123456789abcdefABCDEF?")) "'x")
      (brag-token 'HEX lexeme)]
     ; https://www.seximal.net/
     [(:seq (:+ (char-set "012345?")) "'s")
      (brag-token 'SEXIMAL lexeme)]
     [(:seq (:or alphabetic numeric) "'n")
      (brag-token 'NIF lexeme)]

     [(:or ">=" "<=" "=>" "->" "<-" "!=" "==" "&&" "||" "::" ":=" "<<")
      (brag-token lexeme lexeme)]
     [(char-set "[]{}<>();&|^%*/+!~-,@:.") ;; unused symbols #$`
      (brag-token lexeme lexeme)]
     [(:+ numeric)
      (brag-token 'NUMBER (string->number lexeme))]
     [(:+ (:or alphabetic "_" numeric))
      (brag-token 'IDENTIFIER (string->symbol lexeme))]
     [(eof) 'eof]))
  (define (next-token)
    (let ([n (my-lexer ip)])
      (if (eq? 'eof (position-token-token n)) eof n)))
  next-token)
