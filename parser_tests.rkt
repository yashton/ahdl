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
  (define (parse-test-bind code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-expr code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-module code)
    (match (parse-test code)
      [(list hardware d) d]))
  (define (parse-test-reference code)
    (match (parse-test (string-join (list "let x <- " code "{x}")))
      [(list hardware
             (list let_expr
                   (list let_left_bind
                         (list left_binding x d))
                   (list reference
                         (list ref_id x)))) d]))
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
     [test-case "Hex single symbol" (check-equal? (parse-test-literal "A'x")
                                                  '(hex_literal "A'x"))]
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
       (check-equal? (parse-test-type-hint "1'b:data<2>")
                     '(type_hint (binary_literal "1'b")
                                 (data_type (encoding_unsigned 2))))]
     [test-case "implicit size data type"
       (check-equal? (parse-test-type-hint "1'b:data")
                     '(type_hint (binary_literal "1'b") (data_type)))]
     [test-case "implicit size data type sized literal"
       (check-equal? (parse-test-type-hint "1'b<3>:data")
                     '(type_hint (binary_literal "1'b" 3) (data_type)))]
     [test-case "signed datatype"
       (check-equal? (parse-test-type-hint "1'b:data<signed<2>>")
                     '(type_hint (binary_literal "1'b")
                                 (data_type (encoding_signed 2))))]
     [test-case "unsigned datatype"
       (check-equal? (parse-test-type-hint "1'b:data<unsigned<2>>")   '(type_hint (binary_literal "1'b") (data_type (encoding_unsigned 2))))]
     [test-case "implicit control type"
       (check-equal? (parse-test-type-hint "1'b:ctrl")
                     '(type_hint (binary_literal "1'b") (ctrl_type)))]
     [test-case "sized control type"
       (check-equal? (parse-test-type-hint "1'b:ctrl<2>")   '(type_hint (binary_literal "1'b") (ctrl_type (encoding_unsigned 2))))]
     [test-case "struct/enum type"
       (check-equal? (parse-test-type-hint "foo:data<bar>")
                     '(type_hint (reference (ref_id foo))
                                 (data_type (encoding_generic bar))))]
     [test-case "struct/enum type with generic"
       (check-equal? (parse-test-type-hint "foo:ctrl<bar<2>>")
                     '(type_hint (reference (ref_id foo))
                                 (ctrl_type (encoding_generic bar (encoding_unsigned 2)))))]

     [test-case "struct/enum type with generic parameter"
       (check-equal? (parse-test-type-hint "foo:ctrl<bar<T>>")
                     '(type_hint (reference (ref_id foo))
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
        (parse-test "union instruction { add(id, num:data<4>), beq(id, reg:ctrl<4>) }")
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
        (parse-test "union instruction<32>[thing<2>] { add(thing[0], num:data<4>, '?), beq(thing, reg:ctrl<4>, '?) }")
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
  (define union-instance-tests
    (test-suite
     "union instance"
     [test-case
         "Union instance creation"
       (check-equal?
        (parse-test-expr "thing:second(A'x + foo, 1011'b, '?)")
        '(union_instance
          thing
          second
          (op_add (hex_literal "A'x") (reference (ref_id foo)))
          (binary_literal "1011'b")
          (splat_literal "'?")))]))
  (define struct-def-tests
    (test-suite
     "struct definition"
     [test-case "Struct with implicit sizes"
       (check-equal?
        (parse-test-expr "struct thing(left:data<4>, right:ctrl<4>)")
        '(struct_def
          thing
          (struct_item left (data_type (encoding_unsigned 4)))
          (struct_item right (ctrl_type (encoding_unsigned 4)))))]
     [test-case
         "Struct with explicit sizes and id"
       (check-equal?
        (parse-test "struct thing<12>(num:data<4>, reg:ctrl<4>, '?)")
        '(hardware
          (struct_def
           thing
           12
           (struct_item num (data_type (encoding_unsigned 4)))
           (struct_item reg (ctrl_type (encoding_unsigned 4)))
           (struct_item (splat_literal "'?")))))]))

  (define struct-instance-tests
    (test-suite
     "struct instance"
     [test-case
         "Struct instance creation"
       (check-equal?
        (parse-test-expr "thing(A'x + foo, 1011'b, '?)")
        '(struct_instance
          thing
          (op_add (hex_literal "A'x") (reference (ref_id foo)))
          (binary_literal "1011'b")
          (splat_literal "'?")))]))

  (define conditional-tests
    (test-suite
     "Conditional block tests"
     [test-case "if/else"
       (check-equal? (parse-test-expr "if foo { 110'b } else { 111'b }")
                     '(if_expr (reference (ref_id foo))
                               (binary_literal "110'b")
                               (binary_literal "111'b")))]
     [test-case "if/else if/else"
       (check-equal? (parse-test-expr "if foo { 110'b } else if bar { 111'b } else { 0'b }")
                     '(if_expr (reference (ref_id foo))
                               (binary_literal "110'b")
                               (if_expr (reference (ref_id bar))
                                        (binary_literal "111'b")
                                        (binary_literal "0'b"))))]
     [test-case "if/else if/else if/else"
       (check-equal? (parse-test-expr "if foo { 110'b }
                                      else if bar { 111'b }
                                      else if a == b { 1'b }
                                      else { 0'b }")
                     '(if_expr
                       (reference (ref_id foo))
                       (binary_literal "110'b")
                       (if_expr
                        (reference (ref_id bar))
                        (binary_literal "111'b")
                        (if_expr
                         (op_eq (reference (ref_id a)) (reference (ref_id b)))
                         (binary_literal "1'b")
                         (binary_literal "0'b")))))]))
  (define match-tests
    (test-suite
     "Basic match tests"
     [test-case "simple match"
       (check-equal? (parse-test-expr "match foo { 0'b => { 1'b } 1'b => { 0'b }}")
                     '(match_expr
                       (reference (ref_id foo))
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
                       (reference (ref_id foo))
                       (match_expr_case (destructure (binary_literal "0'b"))
                                        (binary_literal "1'b"))
                       (match_expr_case (destructure (binary_literal "1'b"))
                                        (binary_literal "0'b"))))]
     [test-case "enum match"
       (check-equal?
        (parse-test-expr "match foo { ops::one => { 1'b } ops::zero => { 0'b }}")
        '(match_expr
          (reference (ref_id foo))
          (match_expr_case (destructure (reference (ref_id ops one))) (binary_literal "1'b"))
          (match_expr_case (destructure (reference (ref_id ops zero))) (binary_literal "0'b"))))]
     [test-case "simple match with guard and fallback"
       (check-equal? (parse-test-expr "match foo {
                                         a when a == 1'b => 1'b
                                         _ => 0'b
                                       }")
                     '(match_expr
                       (reference (ref_id foo))
                       (match_expr_case
                        (destructure (reference (ref_id a)))
                        (op_eq (reference (ref_id a)) (binary_literal "1'b"))
                        (binary_literal "1'b"))
                       (match_expr_case (destructure (reference (ref_id _))) (binary_literal "0'b"))))]))
  (define let-tests
    (test-suite
     "let and destructuring"
     [test-case
         "simple binding"
       (check-equal? (parse-test-expr "let a <- 1'b { a }")
                     '(let_expr
                       (let_left_bind
                        (left_binding a (binary_literal "1'b")))
                       (reference (ref_id a))))]
     [test-case
         "multiple bindings"
       (check-equal? (parse-test-expr "let a <- d, b <- 1'b, c <- f & b { a + c }")
                     '(let_expr
                       (let_left_bind
                        (left_binding a (reference (ref_id d)))
                        (left_binding b (binary_literal "1'b"))
                        (left_binding c (op_band (reference (ref_id f)) (reference (ref_id b)))))
                       (op_add (reference (ref_id a)) (reference (ref_id c)))))]))


  (define ops-tests
    (test-suite
     "binary and unary operations"
     [test-case
         "precedence 0"
       (check-equal? (parse-test-expr "b.f<1> as data<foo>")
                     '(op_cast (op_index (op_member (reference (ref_id b)) f)
                                         1)
                               (data_type (encoding_generic foo))))]
     [test-case
         "precedence 0 > 1"
       (check-equal? (parse-test-expr "i * b.f")
                     '(op_mult (reference (ref_id i)) (op_member (reference (ref_id b)) f)))]
     [test-case
         "precedence 1 left to right associative multiplication"
       (check-equal? (parse-test-expr "i * j / k % l ")
                     '(op_mod
                       (op_div
                        (op_mult
                         (reference (ref_id i))
                         (reference (ref_id j)))
                        (reference (ref_id k)))
                       (reference (ref_id l))))]
     [test-case
         "precedence 1 > 2"
       (check-equal? (parse-test-expr "i * j + k % l ")
                     '(op_add (op_mult (reference (ref_id i)) (reference (ref_id j)))
                              (op_mod (reference (ref_id k)) (reference (ref_id l)))))]
     [test-case
         "left to right associative addition"
       (check-equal? (parse-test-expr "a + b - c")
                     '(op_sub (op_add (reference (ref_id a)) (reference (ref_id b))) (reference (ref_id c))))]
     [test-case
         "cast precedence"
       (check-equal? (parse-test-expr "a + b as data")
                     '(op_add (reference (ref_id a))
                              (op_cast (reference (ref_id b)) (data_type))))]
     [test-case
         "unary precedence god test"
       (check-equal? (parse-test-expr "!~-+b.a<2:1><1>")
                     '(op_not
                       (op_bnot
                        (op_neg
                         (op_pos
                          (op_index
                           (op_slice
                            (op_member (reference (ref_id b)) a)
                            2 1)
                           1))))))]
     [test-case
         "precedence god test"
       (check-equal? (parse-test-expr "t || s && r | q ^ p & o != n == m >= l <= k > j < i >> h << g - f + e % d / c * !~-+b.a<2:1><1>")
                     '(op_or
                       (reference (ref_id t))
                       (op_and
                        (reference (ref_id s))
                        (op_bor
                         (reference (ref_id r))
                         (op_bxor
                          (reference (ref_id q))
                          (op_band
                           (reference (ref_id p))
                           (op_neq
                            (reference (ref_id o))
                            (op_eq
                             (reference (ref_id n))
                             (op_gte
                              (reference (ref_id m))
                              (op_lte
                               (reference (ref_id l))
                               (op_gt
                                (reference (ref_id k))
                                (op_lt
                                 (reference (ref_id j))
                                 (op_shiftr
                                  (reference (ref_id i))
                                  (op_shiftl
                                   (reference (ref_id h))
                                   (op_sub
                                    (reference (ref_id g))
                                    (op_add
                                     (reference (ref_id f))
                                     (op_mod
                                      (reference (ref_id e))
                                      (op_div
                                       (reference (ref_id d))
                                       (op_mult
                                        (reference (ref_id c))
                                        (op_not
                                         (op_bnot
                                          (op_neg
                                           (op_pos
                                            (op_index
                                             (op_slice (op_member (reference (ref_id b)) a) 2 1)
                                             1))))))))))))))))))))))))]
     [test-case
         "precedence god reverse test"
       (check-equal? (parse-test-expr "a * b / c % d + e - f << g >> h < i > j <= k >= l == m != n & o ^ p | r && s || t")
                     '(op_or
                       (op_and
                        (op_bor
                         (op_bxor
                          (op_band
                           (op_neq
                            (op_eq
                             (op_gte
                              (op_lte
                               (op_gt
                                (op_lt
                                 (op_shiftr
                                  (op_shiftl
                                   (op_sub
                                    (op_add
                                     (op_mod
                                      (op_div
                                       (op_mult
                                        (reference (ref_id a))
                                        (reference (ref_id b)))
                                       (reference (ref_id c)))
                                      (reference (ref_id d)))
                                     (reference (ref_id e)))
                                    (reference (ref_id f)))
                                   (reference (ref_id g)))
                                  (reference (ref_id h)))
                                 (reference (ref_id i)))
                                (reference (ref_id j)))
                               (reference (ref_id k)))
                              (reference (ref_id l)))
                             (reference (ref_id m)))
                            (reference (ref_id n)))
                           (reference (ref_id o)))
                          (reference (ref_id p)))
                         (reference (ref_id r)))
                        (reference (ref_id s)))
                       (reference (ref_id t))))]))

  (define module-tests
    (test-suite
     "module definition"
     [test-case
         "Basic module"
       (check-equal?
        (parse-test-module
         "module add(left:data<16>; right:data<16>) => (output:data<17>) {}")
        '(module_def
          add
          (argument_list
           (argument left (data_type (encoding_unsigned 16)))
           (argument right (data_type (encoding_unsigned 16))))
          (argument_list (argument output (data_type (encoding_unsigned 17))))
          (module_body))
        )]
     [test-case
         "Module with template variable"
       (check-equal?
        (parse-test-module
         "module mux<S>[element<S>]
         (target@[element]:data<S>) => (output[element]:data<16>)
         {
             bind output[element] <= target@[element]
         }")
        '(module_def
          mux
          (template_def S)
          (addr_def (addr_id_def element S))
          (argument_list
           (argument
            target
            (addr_loc_ref (reference (ref_id element)))
            (data_type (encoding_generic S))))
          (argument_list
           (argument
            output
            (addr_use_ref (reference (ref_id element)))
            (data_type (encoding_unsigned 16))))
          (module_body
           (bind_def
            (bind_lhs (reference (ref_id output) (addr_use_ref (reference (ref_id element)))))
            (bind_rhs (reference (ref_id target) (addr_loc_ref (reference (ref_id element)))))))))]
     [test-case
         "Module with template variable and clock"
       (check-equal?
        (parse-test-module
         "module mux<S>[element<S>]@{clk}
         (target@[element]:data<S>) => (output[element]:data<16>)
         {
             bind output[element] <= target@[element]
         }")
        '(module_def
          mux
          (template_def S)
          (addr_def (addr_id_def element S))
          (clk_def clk)
          (argument_list
           (argument
            target
            (addr_loc_ref (reference (ref_id element)))
            (data_type (encoding_generic S))))
          (argument_list
           (argument
            output
            (addr_use_ref (reference (ref_id element)))
            (data_type (encoding_unsigned 16))))
          (module_body
           (bind_def
            (bind_lhs (reference (ref_id output) (addr_use_ref (reference (ref_id element)))))
            (bind_rhs (reference (ref_id target) (addr_loc_ref (reference (ref_id element)))))))))]))


  (define module-instance-tests
    (test-suite
     "module instance"
     [test-case
         "Simple instance"
       (check-equal?
        (parse-test-bind "bind y <= adder (a -> left; b -> right)")
        '(bind_def
          (bind_lhs (reference (ref_id y)))
          (bind_rhs
           (module_instance
            adder
            (module_input_binding
             (module_input_arg (right_binding (reference (ref_id a)) (reference (ref_id left))))
             (module_input_arg (right_binding (reference (ref_id b)) (reference (ref_id right)))))))))]
     [test-case
         "Instance with generics"
       (check-equal?
        (parse-test-bind "bind y <= adder (a -> left; b -> right)")
        '(bind_def
          (bind_lhs (reference (ref_id y)))
          (bind_rhs
           (module_instance
            adder
            (module_input_binding
             (module_input_arg (right_binding (reference (ref_id a)) (reference (ref_id left))))
             (module_input_arg (right_binding (reference (ref_id b)) (reference (ref_id right)))))))))]
     [test-case
         "Instance with generics and clock"
       (check-equal?
        (parse-test-bind "bind y <= adder<unsigned<3>>@{clk} (a -> left; b -> right)")
        '(bind_def
          (bind_lhs (reference (ref_id y)))
          (bind_rhs
           (module_instance
            adder
            (module_generics (encoding_unsigned 3))
            (module_clock_binding clk)
            (module_input_binding
             (module_input_arg (right_binding (reference (ref_id a)) (reference (ref_id left))))
             (module_input_arg (right_binding (reference (ref_id b)) (reference (ref_id right)))))))))]))


  (define binding-expr-tests
    (test-suite
     "binding expressions"
     [test-case
         "if/else bind"
       (check-equal?
        (parse-test-bind
         "bind * <= if set {
              target@{clk} -> addr@{clk+1}
          } else if incr {
              addr@{clk} + 1'b -> addr@{clk+1}
          } else {
              addr@{clk} -> addr@{clk+1}
          }")
        '(bind_def
          (bind_lhs "*")
          (bind_rhs
           (binding
            (if_bind
             (reference (ref_id set))
             (binding
              (right_binding
               (reference (ref_id target) (clk_ref clk))
               (reference (ref_id addr) (clk_ref clk "+" 1))))
             (if_bind
              (reference (ref_id incr))
              (binding
               (right_binding
                (op_add (reference (ref_id addr) (clk_ref clk)) (binary_literal "1'b"))
                (reference (ref_id addr) (clk_ref clk "+" 1))))
              (binding
               (right_binding
                (reference (ref_id addr) (clk_ref clk))
                (reference (ref_id addr) (clk_ref clk "+" 1))))))))))]))

  (define binding-tests
    (test-suite
     "top binding"
     [test-case
         "simple module instance"
       (check-equal?
        (parse-test "bind alu <= arith_logic_unit@{clk} (alu_op_a -> a; alu_op_b -> b; alu_op -> op)")
        '(hardware
          (bind_def
           (bind_lhs (reference (ref_id alu)))
           (bind_rhs
            (module_instance
             arith_logic_unit
             (module_clock_binding clk)
             (module_input_binding
              (module_input_arg (right_binding (reference (ref_id alu_op_a)) (reference (ref_id a))))
              (module_input_arg (right_binding (reference (ref_id alu_op_b)) (reference (ref_id b))))
              (module_input_arg (right_binding (reference (ref_id alu_op)) (reference (ref_id op))))))))))]))


  (define reference-tests
    (test-suite
     "reference instances"
     [test-case
         "Bare"
       (check-equal?
        (parse-test-reference "foo")
        '(reference (ref_id foo)))]
     [test-case
         "escaped keyword"
       (check-equal?
        (parse-test-reference "`data`")
        '(reference (ref_id data)))]
     [test-case
         "With addresss"
       (check-equal?
        (parse-test-reference "foo[addr]")
        '(reference (ref_id foo) (addr_use_ref (reference (ref_id addr)))))]
     [test-case
         "With addresss sub-expr"
       (check-equal?
        (parse-test-reference "foo[f'x]")
        '(reference (ref_id foo) (addr_use_ref (hex_literal "f'x"))))]
     [test-case
         "With addresss number"
       (check-equal?
        (parse-test-reference "foo[addr + 1'b]")
        '(reference (ref_id foo) (addr_use_ref (op_add (reference (ref_id addr)) (binary_literal "1'b")))))]
     [test-case
         "With address location"
       (check-equal?
        (parse-test-reference "foo@[addr]")
        '(reference (ref_id foo) (addr_loc_ref (reference (ref_id addr)))))]
     [test-case
         "With clock"
       (check-equal?
        (parse-test-reference "foo@{clk}")
        '(reference (ref_id foo) (clk_ref clk)))]
     [test-case
         "With clock offset"
       (check-equal?
        (parse-test-reference "foo@{clk-2}")
        '(reference (ref_id foo) (clk_ref clk "-" 2)))]
     [test-case
         "With address and clock"
       (check-equal?
        (parse-test-reference "foo[addr]@{clk}")
        '(reference (ref_id foo) (addr_use_ref (reference (ref_id addr))) (clk_ref clk)))]
     [test-case
         "With address and clock offset"
       (check-equal?
        (parse-test-reference "foo@[addr]@{clk + 2}")
        '(reference (ref_id foo) (addr_loc_ref (reference (ref_id addr))) (clk_ref clk "+" 2)))]))

  (define namespacing-tests
    (test-suite
     "namespacing"
     [test-case
         "basic use"
       (check-equal?
        (parse-test "use std")
        '(hardware (use_def std)))]
     [test-case
         "nested use"
       (check-equal?
        (parse-test "use std::adder")
        '(hardware (use_def std adder)))]
     [test-case
         "use splat"
       (check-equal?
        (parse-test "use std::*")
        '(hardware (use_def std "*")))]
     [test-case
         "use rebind"
       (check-equal?
        (parse-test "use std::{adder -> fa}")
        '(hardware (use_def std (use_aliases (use_alias adder fa)))))]
     [test-case
         "use multiple rebind"
       (check-equal?
        (parse-test "use std::{adder -> fa; mult -> m}")
        '(hardware (use_def std (use_aliases (use_alias adder fa) (use_alias mult m)))))]
     [test-case
         "simple namespace"
       (check-equal?
        (parse-test "namespace foo { enum bar {a,b}}")
        '(hardware (namespace_def foo (enum_def bar (id_name a) (id_name b)))))]
     [test-case
         "nested namespace"
       (check-equal?
        (parse-test "namespace foo { namespace bar {enum foo {a,b}}}")
        '(hardware (namespace_def foo (namespace_def bar (enum_def foo (id_name a) (id_name b))))))]
     [test-case
         "nested namespace alt"
       (check-equal?
        (parse-test "namespace foo::bar {enum foo {a,b}}")
        '(hardware (namespace_def foo bar (enum_def foo (id_name a) (id_name b)))))]))

  (run-tests namespacing-tests)
  (run-tests literal-tests)
  (run-tests type-tests)
  (run-tests type-hint-tests)
  (run-tests enum-def-tests)
  (run-tests union-def-tests)
  (run-tests union-instance-tests)
  (run-tests struct-def-tests)
  (run-tests struct-instance-tests)
  (run-tests conditional-tests)
  (run-tests match-tests)
  (run-tests let-tests)
  (run-tests ops-tests)
  (run-tests module-tests)
  (run-tests module-instance-tests)
  (run-tests binding-expr-tests)
  (run-tests binding-tests)
  (run-tests reference-tests))
