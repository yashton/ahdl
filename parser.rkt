#lang brag
hardware: statement*

@statement: enum_def | union_def | struct_def | device_def | type_def | namespace_def | use_def | const_def
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

@literal: splat_literal | binary_literal | seximal_literal | octal_literal | decimal_literal | hex_literal | ascii_literal | nif_literal
; Splats can be used to fill in the remaining parts of literal, or a struct, or
; as a way to fill a value.
splat_literal: ("'?" | "'1" | "'0") size_def?
; Literals should be clearly marked with their ordinality. Size can be inferred
binary_literal: BINARY size_def?
seximal_literal: SEXIMAL size_def?
octal_literal: OCTAL size_def?
decimal_literal: DECIMAL size_def?
hex_literal: HEX size_def?
ascii_literal: ASCII size_def?
nif_literal: NIF size_def?

const_def: /"const" IDENTIFIER /"<=" expr

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
type_def: /"type" IDENTIFIER /":=" type

;;;;;;;; Enums ;;;;;;;;
enum_def: /"enum" IDENTIFIER enum_id enum_members
enum_members: /"{" [id_name (/"," id_name)*] /"}"
enum_id: id_def?
id_name: IDENTIFIER (/"[" NUMBER /"]")?
id_def: /"[" IDENTIFIER size_def? /"]"

;;;;;;;; Unions ;;;;;;;;
union_def: /"union" IDENTIFIER union_size union_id union_variants
union_size: size_def?
union_id: id_def?
union_variants: /"{" [union_variant (/"," union_variant)*] /"}"
union_variant: IDENTIFIER /"(" [union_variant_member (/"," union_variant_member)*] /")"
union_variant_member: (IDENTIFIER /":" type) | id_name | literal

;;;;;;;; Structs ;;;;;;;;
struct_def: /"struct" IDENTIFIER struct_size struct_members
struct_size: size_def?
struct_members: /"(" [struct_member (/"," struct_member)*] /")"
struct_member: (IDENTIFIER /":" type) | literal

;;;;;;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;
; Should allow positive edge, negative edge, or level trigger?
clk_id: ("pos" | "neg" | "lev")? IDENTIFIER
clk_def: /"@" /"{" [clk_id (/"," clk_id)*]? /"}" | ()
addr_id_def: IDENTIFIER size_def?
addr_def: /"[" [addr_id_def (/"," addr_id_def)*]? /"]" | ()

template_def: /"<" [templ_param_def (/"," templ_param_def)*]? /">" | ()
templ_param_def: IDENTIFIER
addr_bind: "@"? /"[" IDENTIFIER /"]"

;;;;;;;;;;;;;;;; Bind ;;;;;;;;;;;;;;;;
bind_def: /"bind" destructure /"<=" expr
default_def: "default" bind_def
reset_def: "reset" "@{" IDENTIFIER "}" bind_def

;;;;;;;;;;;;;;;; Devices ;;;;;;;;;;;;;;;;
device_def: /"device" IDENTIFIER template_def addr_def clk_def argument_list /"=>" argument_list /"{" device_body_defs /"}"
device_body_defs: (device_def | enum_def | union_def | struct_def | type_def | bind_def | use_def | default_def | reset_def | const_def)*
argument_list: /"(" [argument (/";" argument)*] /")"
argument: IDENTIFIER addr_ref? /":" type

device_instance: IDENTIFIER device_generics device_clock_binding device_input_binding
device_clock_binding: (/"@" /"{" [IDENTIFIER (/"," IDENTIFIER)*] /"}")?
input_binding: reference /"->" reference
device_input_arg: input_binding | reference
device_input_binding: /"(" ([device_input_arg (/";" device_input_arg)*])? /")"
device_generics: (/"<" [type (/"," type)*] /">")?
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

;;;;;;;;;;;;;;;; Operators ;;;;;;;;;;;;;;;;
;; This matches C precedence
@primary_expr: literal | reference | group_expr | join_expr
@group_expr: /"(" expr /")"
join_expr: /"{" [expr (/"," expr)+] /"}"

@instance_expr: primary_expr | struct_instance | union_instance | device_instance
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

@block_expr: let_expr | if_expr | match_expr | for_expr | or_expr

;;;;;;;;;;;;;;;; Let binding and destructuring ;;;;;;;;;;;;;;;;
let_expr: let_clause /"{" expr /"}"
@let_clause: /"let" (let_items | /"(" let_items /")")
let_items: [let_item (/"," let_item)*]
@let_item: left_binding | destructure_binding

left_binding: destructure /"<-" expr
destructure: reference | literal | destructure_struct | destructure_union | bind_lhs
destructure_struct: IDENTIFIER /"(" [destructure (/"," destructure)*] /")"
;; Same syntax for unions to use in match. Unions are a compile error if used in let (ambiguous)
destructure_union: @destructure_struct
destructure_binding: bind_lhs /"<=" expr

bind_lhs: (IDENTIFIER? /"(" bind_args /")" | bind_args | "*" | reference)
bind_args: [left_binding (/";" left_binding)*]

;;;;;;;;;;;;;;;; Conditional ;;;;;;;;;;;;;;;;
if_expr: /"if" expr /"{" expr /"}" /"else" else_expr
@else_expr: (if_expr | (/"{" expr /"}"))

match_expr: /"match" expr /"{" [match_expr_case (match_expr_case)*] /"}"
match_expr_case: match_clause /"=>" ((/"{" expr /"}") | expr)
match_clause: destructure (/"when" expr)?

;;;;;;;;;;;;;;;; Template expressions ;;;;;;;;;;;;;;;;
@templ_generator: range_generator | list_generator
range_generator: /"#range" /"(" primary_expr /"," primary_expr (/"," primary_expr)? /")"
list_generator: "/#[" [expr ("," expr)*] "/]"
for_expr: /"for" IDENTIFIER /"in" templ_generator /"{" expr /"}"

;;;;;;;;;;;;;;;; Binding expressions ;;;;;;;;;;;;;;;;
@type_hint_expr: type_hint | block_expr
type_hint: block_expr /":" type

@bind_expr: /"{" bindset /"}" | bindset | right_binding | type_hint_expr
bindset: ([bind_expr (/";" bind_expr)*] /";"?)
right_binding: (type_hint_expr /"->" reference)

@expr: bind_expr
