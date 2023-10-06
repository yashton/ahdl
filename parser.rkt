#lang brag

script: type_alias | union_def | enum_def | module_def | encoding_def
body:
expr: num | variable


;;;;;;;;;;;;;;;; Literals ;;;;;;;;;;;;;;;;
; Allowed to use a generic name
size_def: "<" INTEGER? | NAME ">"

literal: splat_literal | binary_literal | octal_literal | decimal_literal | hex_literal | ascii_char_literal
; Splats can be used to fill in the remaining parts of literal, or a struct.
splat_literal: ("'?" | "'1" | "'0") size_def?
; Literals should be clearly marked with their ordinality
binary_literal: BINARY "'b" size_def?
octal_literal: OCTAL "'o" size_def?
decimal_literal: INTEGER "'d" size_def?
hex_literal: HEX "'x" size_def?
ascii_char_literal: "\"" CHAR* "\"" size_def?

;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;
type: type_clk | type_addr | type_data | type_ctrl | type_generic
type_generic: NAME ("<" [type ("," type)*] ">")?
; Does control data need to be encoded?
type_ctrl: "ctrl" size_def?
; In case a clk or address needs to be passed as a value
type_clk: "clk"
type_addr: "addr" size_def?
type_data: "data" ("<" encoding ">")?
; Built in encodings are 2's compliment signed and unsigned ints of bit length.
; Leaving room for additional encodings (e.g. BCD, Gray, float)
; Is a union an encoding?
encoding_signed: ("signed" | "s") size_def | "s" NUMBER
encoding_unsigned: ("unsigned" | "u") size_def | "u" NUMBER | NUMBER
encoding: encoding_signed | encoding_unsigned | type_generic

;;;;;;;;;;;;;;;; Compositie type definitions ;;;;;;;;;;;;;;;;
type_alias: "type" NAME ":=" type

enum_def: "enum" NAME addr_def? "{" [id_name ("," id_name)*] "}"
id_name: NAME ("[" NUMBER "]")?

union_def: "union" NAME "<" INTEGER? ">" addr_def "{" [union_item ("," union_item)*] "}"
union_item_member: NAME "::" type | id_name
union_item: NAME "(" [union_item_member ("," union_item_member)*] ")"

encoding_def: "encoding" NAME
;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;
; Should allow positive edge, negative edge, or level trigger?
clk_def: "@" "{" ("pos" | "neg" | "lev")? NAME }
addr_def: "[" [NAME ("," NAME)*] "]"

addr_bind: "@" "[" NAME "]"

module_def: "module" NAME template? addr_def? clk_def? argument_list "=>" argument_list "{" module_body "}"

module_body: module_def | enum_def | union_def | type_alias |  bind_def
argument_list: "(" [argument ("," argument)*] ")"
argument: NAME addr_bind? "::" type

bind_def: "bind" (NAME? "(" bindings ")" | bindings) "<=" (module_instance | expr)
bindings: [left_binding (";" left_binding)*]

; Look into numeric parameters
generic_defs: [type_generic ("," type_generic)]
module_instance: NAME "<" generic_defs ">" "@" "{" [NAME ("," NAME)*] "}" "(" right_binding ")"
module
;;;;;;;;;;;;;;;; Expressions ;;;;;;;;;;;;;;;;
; Expressions have two types - value or bindings list.
; bindings can't be used with operators, only bound
; when a top bind operation happens:
; if the RHS is a value, it is bound to the LHS (with destructure)
; if the RHS is a bindings, the bindings are applied to the outer scope

expr: let_expr | if_expr | match_expr | reference_expr | binding_expr | binops | unops | slice_op | index_op | cast_op | type_hint

binops: expr ("&" | "|" | "^" | "%" | "*" | "/" | "+" | "-" | "==" | "!=" | ">" | "<" | ">=" | "<=") expr
unops: ("!" | "~" | "-") expr

; Similar to python slices, but should be MSB.
slice_op: expr "<" INTEGER? ":" INTEGER? ">"
index_op: expr "<" INTEGER ">"

cast_op: expr "as" type

type_hint: expr "::" type
group_expr: "(" expr ")"
tuple_expr: "(" [expr ("," expr)+] ")"

left_binding: (NAME | destructure) "<-" expr
right_binding: expr "->" reference
let_expr: "let" (let_bind | "(" let_bind ")") "{" expr "}"
let_bind: [left_binding ("," left_binding)*]
destructure: "(" [destructure_item, ("," destructure_item)*] ")")
destructure_item: NAME | (NAME "::" type) | destructure | (NAME destructure) | literal

reference: NAME addr_ref? clk_ref?
addr_ref: "[" expr "]"
clk_ref: "@{" NAME (("+"|"-") INTEGER)"}"

if_expr: "if" expr "{" expr "}" ("else" "if" "{" expr "}")* "else" "{" expr "}"
match_expr: "match" expr "{" [match_case (match_case)*] "}"
match_case: match_clause "{" expr "}"
match_clause: destructure ("if" expr)?
; The type of a bind_block is a binding list. Non-bindings are an error
bind_block: [expr (";" expr)+]
