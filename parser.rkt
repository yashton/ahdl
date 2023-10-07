#lang brag
hardware: (enum_def | union_def | struct_def | module_def | type_alias | (type | expr))*
;; hardware: (type_alias | union_def | enum_def | module_def | encoding_def)*

;; ;;;;;;;;;;;;;;;; Literals ;;;;;;;;;;;;;;;;
;; ; Allowed to use a generic name
@size_def: /"<" (NUMBER? | IDENTIFIER)? /">"

@literal: splat_literal | binary_literal | octal_literal | decimal_literal | hex_literal | ascii_literal
; Splats can be used to fill in the remaining parts of literal, or a struct, or
; as a way to fill a value.
splat_literal: ("'?" | "'1" | "'0") size_def?
; Literals should be clearly marked with their ordinality. Size can be inferred
binary_literal: BINARY size_def?
octal_literal: OCTAL size_def?
decimal_literal: DECIMAL size_def?
hex_literal: HEX size_def?
ascii_literal: ASCII size_def?

;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;
; should subtype be allow as implicit data<>?
@type: clk_type | addr_type | data_type | ctrl_type
; In case a clk or address needs to be passed as a value
clk_type: /"clock"
addr_type: /"address" size_def?
ctrl_type: /"ctrl" (/"<" subtype /">")?
data_type: /"data" (/"<" subtype /">")?

; generics
@subtype: encoding_signed | encoding_unsigned | encoding_generic
encoding_generic: IDENTIFIER (/"<" [subtype (/"," subtype)*] /">")?
encoding_signed: /("signed" | "s") (/"<" NUMBER /">")?
encoding_unsigned: /("unsigned" | "u") | (/("unsigned" | "u") /"<" NUMBER /">") | NUMBER
; Built in encodings are 2's compliment signed and unsigned ints of bit length.
; also aliases, unions, and structs
; Leaving room for additional encodings (e.g. BCD, Gray, float) through generics

;;;;;;;;;;;;;;;; Composite type definitions ;;;;;;;;;;;;;;;;
type_alias: /"type" IDENTIFIER /":=" subtype

enum_def: /"enum" IDENTIFIER id_def? /"{" [id_name (/"," id_name)*] /"}"
id_name: IDENTIFIER (/"[" NUMBER /"]")?
id_def: /"[" IDENTIFIER (/"<" NUMBER /">")? /"]"

union_def: /"union" IDENTIFIER (/"<" NUMBER /">")? id_def? /"{" [union_item (/"," union_item)*] /"}"
union_item: IDENTIFIER /"(" [union_item_member (/"," union_item_member)*] /")"
union_item_member: (IDENTIFIER /"::" type) | id_name | literal

struct_def: /"" IDENTIFIER (/"<" NUMBER /">")? id_def? /"{" [struct_item (/"," struct_item)*] /"}"
struct_item: (IDENTIFIER /"::" type) | literal

;; encoding_def: "encoding" IDENTIFIER
;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;
; Should allow positive edge, negative edge, or level trigger?
clk_def: "@" "{" ("pos" | "neg" | "lev")? IDENTIFIER "}"
addr_def: "[" [id_name ("," id_name)*] "]"

; TODO extend to relative parameters
template_def: "<" [IDENTIFIER ("," IDENTIFIER)*] ">"
addr_bind: "@" "[" IDENTIFIER "]"

module_def: /"module" IDENTIFIER template_def? addr_def? clk_def? argument_list "=>" argument_list "{" module_body "}"
module_body: module_def | enum_def | union_def | type_alias |  bind_def
argument_list: "(" [argument ("," argument)*] ")"
argument: IDENTIFIER addr_bind? "::" type

bind_def: /"bind" (IDENTIFIER? /"(" bind_args /")" | bind_args | "*") /"<=" (module_instance | expr | bindset)
bind_args: [left_binding (/";" left_binding)*]

; Look into numeric parameters
module_instance: IDENTIFIER "<" generic_defs ">" "@" "{" [IDENTIFIER ("," IDENTIFIER)*] "}" "(" [right_binding ("," right_binding)*] ")"
generic_defs: [encoding_generic ("," encoding_generic)]
;;;;;;;;;;;;;;;; Expressions ;;;;;;;;;;;;;;;;
; Expressions have two types - value expression (expr) or binding expressions (bind)
; bindings can't be used with operators
; when a top bind operation happens:
; if the RHS is a value, it is bound to the LHS (with destructure)
; if the RHS is a bindings, the bindings are applied to the outer scope
reference: IDENTIFIER addr_ref? clk_ref?
addr_ref: /"[" expr /"]"
clk_ref: /"@{" IDENTIFIER (("+"|"-") NUMBER)? /"}"

@expr: tuple_expr | let_expr | if_expr | match_expr | or_expr | primary_expr | type_hint
type_hint: expr /"::" type

;;;;;;;;;;;;;;;; Operators ;;;;;;;;;;;;;;;;
;; This matches C precedence
@primary_expr: literal | reference | group_expr
@group_expr: /"(" expr /")"

@postfix_expr: primary_expr | op_member
op_member: postfix_expr /"." IDENTIFIER

; Similar to python slices, but should be MSB.
@index_expr: postfix_expr | op_index | op_slice
op_index: index_expr /"<" NUMBER /">"
op_slice: index_expr /"<" NUMBER? /":" NUMBER? /">"

@unary_expr: index_expr | op_cast | op_pos | op_neg | op_bnot | op_not
op_pos: /"+" cast_expr
op_neg: /"-" cast_expr
op_bnot: /"~" cast_expr
op_not: /"!" cast_expr

@cast_expr: unary_expr | op_cast
op_cast: cast_expr /"as" type

@mult_expr: cast_expr | op_mult
op_mult: mult_expr /"*" cast_expr

@div_expr: mult_expr | op_div
op_div: div_expr /"/" mult_expr

@mod_expr: div_expr | op_mod
op_mod: mod_expr /"%" div_expr

@add_expr: mod_expr | op_add
op_add: add_expr /"+" mod_expr

@sub_expr: add_expr | op_sub
op_sub: sub_expr /"-" add_expr

@shiftl_expr: sub_expr | op_shiftl
op_shiftl: shiftl_expr /"<<" sub_expr

; Having problems with ">>" cropping up in generics and messing with the lexer
@shiftr_expr: shiftl_expr | op_shiftr
op_shiftr: shiftr_expr /">" /">" shiftl_expr

@lt_expr: shiftr_expr | op_lt
op_lt: lt_expr /"<" shiftr_expr

@gt_expr: lt_expr | op_gt
op_gt: gt_expr /">" lt_expr

@lte_expr: gt_expr | op_lte
op_lte: lte_expr /"<=" gt_expr

@gte_expr: lte_expr | op_gte
op_gte: gte_expr /">=" lte_expr

@eq_expr: gte_expr | op_eq
op_eq: eq_expr /"==" gte_expr

@neq_expr: eq_expr | op_neq
op_neq: neq_expr /"!=" eq_expr

@band_expr: neq_expr | op_band
op_band: band_expr /"&" neq_expr

@bxor_expr: band_expr | op_bxor
op_bxor: bxor_expr /"^" band_expr

@bor_expr: bxor_expr | op_bor
op_bor: bor_expr /"|" bxor_expr

@and_expr: bor_expr | op_and
op_and: and_expr /"&&" bor_expr

@or_expr: and_expr | op_or
op_or: or_expr /"||" and_expr

;;;;;;;;;;;;;;;; Let binding and destructuring ;;;;;;;;;;;;;;;;
left_binding: (IDENTIFIER | destructure) /"<-" expr

let_expr: let_clause /"{" expr /"}"
@let_clause: /"let" (let_left_bind | /"(" let_left_bind /")")
let_left_bind: [left_binding (/"," left_binding)*]
;; destructure: "(" [destructure_item, ("," destructure_item)*] ")"
;; destructure_item: IDENTIFIER | (IDENTIFIER "::" type) | destructure | (IDENTIFIER destructure) | literal
destructure: literal | reference
tuple_expr: /"(" [expr (/"," expr)+] /")"

;;;;;;;;;;;;;;;; Conditional ;;;;;;;;;;;;;;;;
if_expr: /"if" expr /"{" expr /"}" /"else" else_expr
@else_expr: (if_expr | (/"{" expr /"}"))

match_expr: /"match" expr /"{" [match_expr_case (match_expr_case)*] /"}"
match_expr_case: match_clause /"=>" ((/"{" expr /"}") | expr)
@match_clause: destructure (/"when" expr)?

;;;;;;;;;;;;;;;; Binding ;;;;;;;;;;;;;;;;
binding: right_binding | let_bind | if_bind | match_bind | bindset
bindset: [binding (/";" binding)*]
right_binding: expr /"->" reference
let_bind: let_clause /"{" binding /"}"
if_bind: /"if" binding /"{" binding /"}" /"else" else_bind
@else_bind: (if_bind | (/"{" binding /"}"))
match_bind: /"match" expr /"{" [match_case_bind (match_case_bind)*] /"}"
match_case_bind: match_clause /"{" binding /"}"
