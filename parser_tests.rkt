#lang racket
(require "parser.rkt" "lexer.rkt")

(module+ test
  (require rackunit rackunit/text-ui)
  (define (parse-test code)
    (let* ([token-thunk (tokenize "dummy" (open-input-string code))]
           [stx (parse token-thunk)])
      (syntax->datum stx)))

  (define (display-tokens code)
    (letrec ([token-thunk (tokenize "dummy" (open-input-string code))]
             [recurse (lambda (n)
                        (let ([token (n)])
                          (if (eq? token eof)
                              '()
                              (begin (displayln token) (recurse n)))))])
      (recurse token-thunk)))

  (define (parse-test-literal code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-expr code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-type-hint code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-type code)
    (match (parse-test code)
      [(list hardware d) d]))

  (define literal-tests
    (test-suite
     "literal parsing"
     [test-case "splat 0" (check-equal? (parse-test-literal "'0")
                                        '(splat_literal "'0"))]
     [test-case "splat 1" (check-equal? (parse-test-literal "'1") '
                                        (splat_literal "'1"))]
     [test-case "splat don't care" (check-equal? (parse-test-literal "'?")
                                                 '(splat_literal "'?"))]
     [test-case "splat 0 sized" (check-equal? (parse-test-literal "'0<2>")
                                              '(splat_literal "'0" 2))]
     [test-case "splat 1 sized" (check-equal? (parse-test-literal "'1<4>")
                                              '(splat_literal "'1" 4))]
     [test-case "splat don't care sized" (check-equal? (parse-test-literal "'?<3>")
                                                       '(splat_literal "'?" 3))]
     [test-case "ASCII" (check-equal? (parse-test-literal "\"hello world!\"")
                                      '(ascii_literal "\"hello world!\""))]
     [test-case "Binary" (check-equal? (parse-test-literal "1001'b")
                                       '(binary_literal "1001'b"))]
     [test-case "Binary sized" (check-equal? (parse-test-literal "101'b<2>")
                                             '(binary_literal "101'b" 2))]
     [test-case "Binary don't care" (check-equal? (parse-test-literal "?10?'b")
                                                  '(binary_literal "?10?'b"))]
     [test-case "Octal" (check-equal? (parse-test-literal "07'o")
                                      '(octal_literal "07'o"))]
     [test-case "Octal sized" (check-equal? (parse-test-literal "07'o<2>")
                                            '(octal_literal "07'o" 2))]
     [test-case "Octal don't care" (check-equal? (parse-test-literal "0?243'o")
                                                 '(octal_literal "0?243'o"))]
     [test-case "Decimal" (check-equal? (parse-test-literal "14097'd")
                                        '(decimal_literal "14097'd"))]
     [test-case "Decimal sized" (check-equal? (parse-test-literal "7'd<4>")
                                              '(decimal_literal "7'd" 4))]
     [test-case "Hex" (check-equal? (parse-test-literal "fda14097'x")
                                    '(hex_literal "fda14097'x"))]
     [test-case "Hex sized" (check-equal? (parse-test-literal "fda34'x<32>")
                                          '(hex_literal "fda34'x" 32))]
     [test-case "Hex don't care" (check-equal? (parse-test-literal "?fa92?'x")
                                               '(hex_literal "?fa92?'x"))]))

  (define type-tests
    (test-suite
     "type definition parsing"
     [test-case "sized data type"
       (check-equal? (parse-test-type "data<2>") '(data_type (encoding_unsigned 2)))]
     [test-case "implicit size data type"
       (check-equal? (parse-test-type "data") '(data_type))]
     [test-case "implicit size data type"
       (check-equal? (parse-test-type "data<unsigned>") '(data_type (encoding_unsigned)))]
     [test-case "signed datatype"
       (check-equal? (parse-test-type "data<signed<2>>") '(data_type (encoding_signed 2)))]
     [test-case "unsigned datatype"
       (check-equal? (parse-test-type "data<unsigned<2>>") '(data_type (encoding_unsigned 2)))]
     [test-case "implicit control type"
       (check-equal? (parse-test-type "ctrl") '(ctrl_type))]
     [test-case "sized control type"
       (check-equal? (parse-test-type "ctrl<2>") '(ctrl_type (encoding_unsigned 2)))]
     [test-case "redundant control type"
       (check-equal? (parse-test-type "ctrl<unsigned<2>>") '(ctrl_type (encoding_unsigned 2)))]
     [test-case "generic control type"
       (check-equal? (parse-test-type "ctrl<thing<2>>") '(ctrl_type (encoding_generic thing (encoding_unsigned 2))))]
     [test-case "generic data type"
       (check-equal? (parse-test-type "data<thing<T>>") '(data_type (encoding_generic thing (encoding_generic T))))]
     [test-case "type alias"
       (check-equal? (parse-test "type foo := thing1<unsigned<2>, thing2>")
                     '(hardware (type_alias foo (encoding_generic thing1 (encoding_unsigned 2) (encoding_generic thing2)))))]))

  (define type-hint-tests
    (test-suite
     "type definition parsing"
     [test-case "sized data type"
       (check-equal? (parse-test-type-hint "1'b::data<2>")
                     '(type_hint (binary_literal "1'b")
                                 (data_type (encoding_unsigned 2))))]
     [test-case "implicit size data type"
       (check-equal? (parse-test-type-hint "1'b::data")
                     '(type_hint (binary_literal "1'b") (data_type)))]
     [test-case "implicit size data type sized literal"
       (check-equal? (parse-test-type-hint "1'b<3>::data")
                     '(type_hint (binary_literal "1'b" 3) (data_type)))]
     [test-case "signed datatype"
       (check-equal? (parse-test-type-hint "1'b::data<signed<2>>")
                     '(type_hint (binary_literal "1'b")
                                 (data_type (encoding_signed 2))))]
     [test-case "unsigned datatype"
       (check-equal? (parse-test-type-hint "1'b::data<unsigned<2>>")   '(type_hint (binary_literal "1'b") (data_type (encoding_unsigned 2))))]
     [test-case "implicit control type"
       (check-equal? (parse-test-type-hint "1'b::ctrl")
                     '(type_hint (binary_literal "1'b") (ctrl_type)))]
     [test-case "sized control type"
       (check-equal? (parse-test-type-hint "1'b::ctrl<2>")   '(type_hint (binary_literal "1'b") (ctrl_type (encoding_unsigned 2))))]
     [test-case "struct/enum type"
       (check-equal? (parse-test-type-hint "foo::data<bar>")
                     '(type_hint (reference foo)
                                 (data_type (encoding_generic bar))))]
     [test-case "struct/enum type with generic"
       (check-equal? (parse-test-type-hint "foo::ctrl<bar<2>>")
                     '(type_hint (reference foo)
                                 (ctrl_type (encoding_generic bar (encoding_unsigned 2)))))]

     [test-case "struct/enum type with generic parameter"
       (check-equal? (parse-test-type-hint "foo::ctrl<bar<T>>")
                     '(type_hint (reference foo)
                                 (ctrl_type (encoding_generic bar (encoding_generic T)))))]))

  (define enum-def-tests
    (test-suite
     "enum definition parsing"
     [test-case "Basic enum"
       (check-equal? (parse-test "enum ops { add, sub, shiftl, shiftr, mult }")
                     '(hardware
                       (enum_def
                        ops
                        (id_name add)
                        (id_name sub)
                        (id_name shiftl)
                        (id_name shiftr)
                        (id_name mult))))]
     [test-case "enum with id"
       (check-equal? (parse-test "enum ops[id] { add[0], sub[5], shiftl[2], shiftr, mult }")
                     '(hardware
                       (enum_def
                        ops
                        (id_def id)
                        (id_name add 0)
                        (id_name sub 5)
                        (id_name shiftl 2)
                        (id_name shiftr)
                        (id_name mult))))]
    [test-case "enum with id with size"
       (check-equal? (parse-test "enum ops[id<3>] { add[0], sub[5], shiftl[2], shiftr, mult }")
                     '(hardware
                       (enum_def
                        ops
                        (id_def id 3)
                        (id_name add 0)
                        (id_name sub 5)
                        (id_name shiftl 2)
                        (id_name shiftr)
                        (id_name mult))))]))
  (define union-def-tests
    (test-suite
     "Tests for union definition"
     [test-case "Union with implicit sizes and id"
       (check-equal?
        (parse-test "union instruction { add(id, num::data<4>), beq(id, reg::ctrl<4>) }")
        '(hardware
          (union_def
           instruction
           (union_item
            add
            (union_item_member (id_name id))
            (union_item_member num (data_type (encoding_unsigned 4))))
           (union_item
            beq
            (union_item_member (id_name id))
            (union_item_member reg (ctrl_type (encoding_unsigned 4)))))))]
     [test-case "Union with explicit sizes and id"
       (check-equal?
        (parse-test "union instruction<32>[thing<2>] { add(thing[0], num::data<4>, '?), beq(thing, reg::ctrl<4>, '?) }")
        '(hardware
          (union_def
           instruction
           32
           (id_def thing 2)
           (union_item
            add
            (union_item_member (id_name thing 0))
            (union_item_member num (data_type (encoding_unsigned 4)))
            (union_item_member (splat_literal "'?")))
           (union_item
            beq
            (union_item_member (id_name thing))
            (union_item_member reg (ctrl_type (encoding_unsigned 4)))
            (union_item_member (splat_literal "'?"))))))]))
  (define conditional-tests
   (test-suite
     "Conditional block tests"
     [test-case "if/else"
       (check-equal? (parse-test-expr "if foo { 110'b } else { 111'b }")
                     '(if_expr (reference foo)
                               (binary_literal "110'b")
                               (binary_literal "111'b")))]
     [test-case "if/else if/else"
       (check-equal? (parse-test-expr "if foo { 110'b } else if bar { 111'b } else { 0'b }")
                     '(if_expr (reference foo)
                               (binary_literal "110'b")
                               (if_expr (reference bar)
                                        (binary_literal "111'b")
                                        (binary_literal "0'b"))))]
     [test-case "if/else if/else if/else"
       (check-equal? (parse-test-expr "if foo { 110'b }
                                      else if bar { 111'b }
                                      else if a == b { 1'b }
                                      else { 0'b }")
                     '(if_expr
                       (reference foo)
                       (binary_literal "110'b")
                       (if_expr
                        (reference bar)
                        (binary_literal "111'b")
                        (if_expr
                         (op_eq (reference a) (reference b))
                         (binary_literal "1'b")
                         (binary_literal "0'b")))))]))
  (define match-tests
    (test-suite
     "Basic match tests"
     [test-case "simple match"
       (check-equal? (parse-test-expr "match foo { 0'b => { 1'b } 1'b => { 0'b }}")
                     '(match_expr
                       (reference foo)
                       (match_expr_case (destructure (binary_literal "0'b"))
                                        (binary_literal "1'b"))
                       (match_expr_case (destructure (binary_literal "1'b"))
                                        (binary_literal "0'b"))))]
     [test-case "simple match without braces"
       (check-equal? (parse-test-expr "match foo {
                                         0'b => 1'b
                                         1'b => 0'b
                                       }")
                     '(match_expr
                       (reference foo)
                       (match_expr_case (destructure (binary_literal "0'b"))
                                        (binary_literal "1'b"))
                       (match_expr_case (destructure (binary_literal "1'b"))
                                        (binary_literal "0'b"))))]
     [test-case "simple match with guard and fallback"
       (check-equal? (parse-test-expr "match foo {
                                         a when a == 1'b => 1'b
                                         _ => 0'b
                                       }")
                     '(match_expr
                       (reference foo)
                       (match_expr_case
                        (destructure (reference a))
                        (op_eq (reference a) (binary_literal "1'b"))
                        (binary_literal "1'b"))
                       (match_expr_case (destructure (reference _)) (binary_literal "0'b"))))]))
  (define let-tests
    (test-suite
     "let and destructuring"
     [test-case
         "simple binding"
       (check-equal? (parse-test-expr "let a <- 1'b { a }")
                     '(let_expr
                       (let_left_bind
                        (left_binding a (binary_literal "1'b")))
                       (reference a)))]
     [test-case
         "multiple bindings"
       (check-equal? (parse-test-expr "let a <- d, b <- 1'b, c <- f & b { a + c }")
                     '(let_expr
                       (let_left_bind
                        (left_binding a (reference d))
                        (left_binding b (binary_literal "1'b"))
                        (left_binding c (op_band (reference f) (reference b))))
                       (op_add (reference a) (reference c))))]))


  (define ops-tests
    (test-suite
     "binary and unary operations"
     [test-case
         "precedence 0"
       (check-equal? (parse-test-expr "b.f<1> as data<foo>")
                     '(op_cast (op_index (op_member (reference b) f)
                                         1)
                               (data_type (encoding_generic foo))))]
     [test-case
         "precedence 0 > 1"
       (check-equal? (parse-test-expr "i * b.f")
                     '(op_mult (reference i) (op_member (reference b) f)))]
     [test-case
         "precedence 1 left to right associative multiplication"
       (check-equal? (parse-test-expr "i * j / k % l ")
                     '(op_mod
                       (op_div
                        (op_mult
                         (reference i)
                         (reference j))
                        (reference k))
                       (reference l)))]
     [test-case
         "precedence 1 > 2"
       (check-equal? (parse-test-expr "i * j + k % l ")
                     '(op_add (op_mult (reference i) (reference j))
                               (op_mod (reference k) (reference l))))]
     [test-case
         "left to right associative addition"
       (check-equal? (parse-test-expr "a + b - c")
                     '(op_sub (op_add (reference a) (reference b)) (reference c)))]
     [test-case
         "cast precedence"
       (check-equal? (parse-test-expr "a + b as data")
                     '(op_add (reference a)
                              (op_cast (reference b) (data_type))))]
     [test-case
         "unary precedence god test"
       (check-equal? (parse-test-expr "!~-+b.a<2:1><1>")
                     '(op_not
                       (op_bnot
                        (op_neg
                         (op_pos
                          (op_index
                           (op_slice
                            (op_member (reference b) a)
                            2 1)
                          1))))))]
     [test-case
         "precedence god test"
       (check-equal? (parse-test-expr "t || s && r | q ^ p & o != n == m >= l <= k > j < i >> h << g - f + e % d / c * !~-+b.a<2:1><1>")
                     '(op_or (reference t)
                       (op_and (reference s)
                        (op_bor (reference r)
                         (op_bxor (reference q)
                          (op_band (reference p)
                           (op_neq (reference o)
                            (op_eq (reference n)
                             (op_gte (reference m)
                              (op_lte (reference l)
                               (op_gt (reference k)
                                (op_lt (reference j)
                                 (op_shiftr (reference i)
                                  (op_shiftl (reference h)
                                   (op_sub (reference g)
                                    (op_add (reference f)
                                     (op_mod (reference e)
                                      (op_div (reference d)
                                       (op_mult (reference c)
                                        (op_not
                                         (op_bnot
                                          (op_neg
                                           (op_pos
                                            (op_index
                                             (op_slice
                                              (op_member (reference b) a)
                                              2 1)
                                             1))))))))))))))))))))))))]))

;;  (display-tokens "t || s && r | q ^ p & o != n == m >= l <= k > j < i >> h << g - f + e % d / c * !~-+b.a<2:1><1>")
  (run-tests ops-tests)
  (run-tests let-tests)
  (run-tests match-tests)
  (run-tests type-tests)
  (run-tests type-hint-tests)
  (run-tests conditional-tests)
  (run-tests literal-tests)
  (run-tests enum-def-tests)
  (run-tests union-def-tests))
