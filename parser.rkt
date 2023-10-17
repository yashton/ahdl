#lang brag
hardware: statement*

@statement: enum_def | union_def | struct_def | module_def | type_def | namespace_def | use_def | const_def
;;;;;;;;;;;;;;;; Namespaces ;;;;;;;;;;;;;;;;
use_def: /"use" namespace_ref use_aliases?
use_aliases: (/"::" /"{" [use_alias (/";" use_alias)*] /"}")?
use_alias: IDENTIFIER /"->" IDENTIFIER
@namespace_ref: IDENTIFIER | "*" | op_namespace
@op_namespace: IDENTIFIER /"::" namespace_ref
namespace_id: [IDENTIFIER (/"::" IDENTIFIER)*]
namespace_def: /"namespace" namespace_id /"{" statement* /"}"

;;;;;;;;;;;;;;;; Literals ;;;;;;;;;;;;;;;;
; Allowed to use a generic name
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

const_def: /"const" IDENTIFIER "<=" expr

;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;
; Need to introduce imports and namespaces

; should subtype be allow as implicit data<>?
@type: clk_type | addr_type | data_type | ctrl_type | subtype
; In case a clk or address needs to be passed as a value
clk_type: /"clock"
addr_type: /"address" size_def?
ctrl_type: /"ctrl" (/"<" subtype /">")?
data_type: /"data" (/"<" subtype /">")?

; generics
@subtype: encoding_singed | encoding_unsinged | encoding_generic
encoding_generic: IDENTIFIER (/"<" [subtype (/"," subtype)*] /">")?
encoding_singed: /("signed" | "s") size_def?
encoding_unsinged: /("unsigned" | "u") | (/("unsigned" | "u") size_def?) | NUMBER
; Built in encodings are 2's compliment signed and unsigned ints of bit length.
; also aliases, unions, and structs
; Leaving room for additional encodings (e.g. BCD, Gray, float) through generics

;;;;;;;;;;;;;;;; Composite type definitions ;;;;;;;;;;;;;;;;
type_def: /"type" IDENTIFIER /":=" subtype

;;;;;;;; Enums ;;;;;;;;
enum_def: /"enum" IDENTIFIER id_def? /"{" [id_name (/"," id_name)*] /"}"
id_name: IDENTIFIER (/"[" NUMBER /"]")?
id_def: /"[" IDENTIFIER size_def? /"]"

;;;;;;;; Unions ;;;;;;;;
union_def: /"union" IDENTIFIER size_def? id_def? /"{" [union_item (/"," union_item)*] /"}"
union_item: IDENTIFIER /"(" [union_item_member (/"," union_item_member)*] /")"
union_item_member: (IDENTIFIER /":" type) | id_name | literal

;;;;;;;; Structs ;;;;;;;;
struct_def: /"struct" IDENTIFIER size_def? /"(" [struct_item (/"," struct_item)*] /")"
struct_item: (IDENTIFIER /":" type) | literal

;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;
; Should allow positive edge, negative edge, or level trigger?
clk_def: /"@" /"{" ("pos" | "neg" | "lev")? IDENTIFIER /"}"
addr_id_def: IDENTIFIER size_def?
addr_def: /"[" [addr_id_def (/"," addr_id_def)*] /"]"

template_def: /"<" [templ_param_def (/"," templ_param_def)*] /">"
templ_param_def: IDENTIFIER | op_templ_assign
addr_bind: "@"? /"[" IDENTIFIER /"]"

;;;;;;;;;;;;;;;; Bind ;;;;;;;;;;;;;;;;
bind_def: /"bind" bind_lhs /"<=" binding
default_def: "default" bind_def
reset_def: "reset" "@{" IDENTIFIER "}" bind_def

bind_expr_def: /"bind" reference /"<=" expr
bind_lhs: (IDENTIFIER? /"(" bind_args /")" | bind_args | "*" | reference)
bind_args: [left_binding (/";" left_binding)*]

;;;;;;;;;;;;;;;; Modules ;;;;;;;;;;;;;;;;
module_def: /"module" IDENTIFIER template_def? addr_def? clk_def? argument_list /"=>" argument_list /"{" module_body_defs /"}"
module_body_defs: (module_def | enum_def | union_def | struct_def | type_def | bind_def | bind_expr_def | use_def | if_templ_def | for_templ_def | reset_def | default_def | const_def)*
argument_list: /"(" [argument (/";" argument)*] /")"
argument: IDENTIFIER addr_ref? /":" type

module_instance: IDENTIFIER module_generics?  module_clock_binding? module_input_binding
module_clock_binding: /"@" /"{" [IDENTIFIER (/"," IDENTIFIER)*] /"}"
module_input_arg: right_binding | reference
module_input_binding: /"(" ([module_input_arg (/";" module_input_arg)*])? /")"
module_generics: /"<" [type (/"," type)*] /">"
;;;;;;;;;;;;;;;; Expressions ;;;;;;;;;;;;;;;;
; Expressions have two types - value expression (expr) or binding expressions (bind)
; bindings can't be used with operators
; when a top bind operation happens, if the RHS is a bindings, the bindings are applied to the outer scope
reference: ref_id addr_ref? clk_ref?
ref_id: IDENTIFIER (/"::" IDENTIFIER)*
@addr_ref: addr_use_ref | addr_loc_ref
addr_use_ref: /"[" expr /"]"
addr_loc_ref: /"@" /"[" expr /"]"
clk_ref: /"@" /"{" IDENTIFIER (("+"|"-") NUMBER)? /"}"

@expr: let_expr | if_expr | match_expr | or_expr | type_hint | templ_expr
type_hint: expr /":" type

;;;;;;;;;;;;;;;; Operators ;;;;;;;;;;;;;;;;
;; This matches C precedence
@primary_expr: literal | reference | group_expr | join_expr
@group_expr: /"(" expr /")"
join_expr: /"{" [expr (/"," expr)+] /"}"

@instance_expr: primary_expr | struct_instance | union_instance
union_instance: IDENTIFIER /":" IDENTIFIER /"(" [expr (/"," expr)*] /")"
struct_instance: IDENTIFIER /"(" [expr (/"," expr)*] /")"

@postfix_expr: instance_expr | op_member
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
left_binding: (IDENTIFIER | destructure) /"<-" (expr | binding)
let_expr: let_clause /"{" expr /"}"
@let_clause: /"let" (let_items | /"(" let_items /")")
let_item: left_binding | destructure_binding
let_items: [let_item (/"," let_item)*]

destructure: reference | literal | destructure_struct | destructure_union
destructure_struct: IDENTIFIER /"(" [destructure (/"," destructure)*] /")"
;; Same syntax for unions to use in match. Unions are a compile error if used in let (ambiguous)
destructure_union: @destructure_struct

destructure_binding: bind_lhs /"<=" binding
;;;;;;;;;;;;;;;; Conditional ;;;;;;;;;;;;;;;;
if_expr: /"if" expr /"{" expr /"}" /"else" else_expr
@else_expr: (if_expr | (/"{" expr /"}"))

match_expr: /"match" expr /"{" [match_expr_case (match_expr_case)*] /"}"
match_expr_case: match_clause /"=>" ((/"{" expr /"}") | expr)
@match_clause: destructure (/"when" expr)?

;;;;;;;;;;;;;;;; Binding expressions ;;;;;;;;;;;;;;;;
binding: right_binding | let_bind | if_bind | match_bind | bindset | module_instance | templ_bind

bindset: ([binding (/";" binding)*] /";"?) | /";"
right_binding: (expr /"->" reference) | (binding /"->" reference)
let_bind: let_clause /"{" binding /"}"
if_bind: /"if" expr /"{" binding /"}" /"else" else_bind
@else_bind: (if_bind | (/"{" binding /"}"))
match_bind: /"match" expr /"{" [match_case_bind (match_case_bind)*] /"}"
match_case_bind: match_clause "=>" ((/"{" binding /"}") | binding)

;;;;;;;;;;;;;;;; Template expressions ;;;;;;;;;;;;;;;;
; This is written out explicitly because the types are incompatible with other expressions
templ_bind: if_templ_bind | for_templ_bind | let_templ_bind
templ_expr: if_templ_expr | let_templ_expr

op_templ_assign: IDENTIFIER /"=" op_templ_or

templ_op: if_templ | or_templ | op_templ_or

primary_templ: IDENTIFIER | NUMBER | group_templ
@group_templ: /"(" templ_op /")"
@unary_templ: primary_templ | op_templ_pos | op_templ_neg | op_templ_not
op_templ_pos: /"+" primary_templ
op_templ_neg: /"-" primary_templ
op_templ_not: /"!" primary_templ

@mult_templ: unary_templ | op_templ_mult
op_templ_mult: mult_templ /"*" unary_templ

@div_templ: mult_templ | op_templ_div
op_templ_div: div_templ /"/" mult_templ

@mod_templ: div_templ | op_templ_mod
op_templ_mod: mod_templ /"%" div_templ

@add_templ: mod_templ | op_templ_add
op_templ_add: add_templ /"+" mod_templ

@sub_templ: add_templ | op_templ_sub
op_templ_sub: sub_templ /"-" add_templ

@lt_templ: add_templ | op_templ_lt
op_templ_lt: lt_templ /"<" add_templ

@gt_templ: lt_templ | op_templ_gt
op_templ_gt: gt_templ /">" lt_templ

@lte_templ: gt_templ | op_templ_lte
op_templ_lte: lte_templ /"<=" gt_templ

@gte_templ: lte_templ | op_templ_gte
op_templ_gte: gte_templ /">=" lte_templ

@eq_templ: gte_templ | op_templ_eq
op_templ_eq: eq_templ /"==" gte_templ

@neq_templ: eq_templ | op_templ_neq
op_templ_neq: neq_templ /"!=" eq_templ

@and_templ: neq_templ | op_templ_and
op_templ_and: and_templ /"&&" neq_templ

@or_templ: and_templ | op_templ_or
op_templ_or: or_templ /"||" and_templ

let_templ_clause: IDENTIFIER /"<-" templ_expr
let_templ_bind: let_templ_clause /"{" binding /"}"
let_templ_expr: let_templ_clause /"{" expr /"}"

if_templ_bind: /"if" templ_op /"{" binding /"}" /"else" else_templ_bind
@else_templ_bind: (if_templ_bind | (/"{" binding /"}"))

if_templ_expr: /"if" templ_op /"{" expr /"}" /"else" else_templ_expr
@else_templ_expr: (if_templ_expr | (/"{" expr /"}"))

if_templ: /"if" templ_op /"{" templ_op /"}" /"else" else_templ
@else_templ: (if_templ | (/"{" templ_op /"}"))

if_templ_def: /"if" templ_op /"{" module_body_defs /"}" /"else" else_templ_def
@else_templ_def: (if_templ_def | (/"{" module_body_defs /"}"))

templ_generator: "#range" /"(" primary_templ /"," primary_templ (/"," primary_templ)? /")"

for_templ_bind: /"for" IDENTIFIER /"in" templ_generator /"{" binding /"}"
for_templ_def: /"for" IDENTIFIER /"in" templ_generator /"{" module_body_defs /"}"
