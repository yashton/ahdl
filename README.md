# Notes on the language
## Types
Top level types are binding scopes, net references, clocks, and template parameters.

Binding scopes consist of a set of references bound to expressions.

All logic is represented by value expressions.

### Net types
Net references have two types: data and control (ctrl). Both types are generic over subtypes. Core subtypes are unsigned<size> and signed<size>. Derived types are _encodings_ of these two types. Built in encodings are enums, structs and unions.

Enums are a set of named identifier values. Id values can be specified explicitly or implicitly generated.

Structs represent named and typed slices of a set of unsigned bits. These are analogous to C structs, though are not required to align with byte or word boundaries.

Unions represent a set of multiple related interpretations of the bits. These are analogous to C unions in interpreted stored data, and algebraic data types from functional languages. Unions have a dedicated id field which allows differentiation at runtime.
## Expressions and definitions
There are three kinds of statements.
1. Definitions are `enum`, `module`, `struct`, `union`, `namespace`, and `use`.
2. The `bind` statement introduces references from inside a binding into the current scope.
3. Other keywords, operators, and references are expressions.

## Core language model - binding
Expressions have two types - value expression (expr) or binding expressions (bind)

References (and implicitly literals) are net names bound in the current scope.

Bindings can use `let`, `if`, and `match`, but bindings can't be used as values without the binding being extracted to the scope as a reference. Branches cannot have different expression types.

This corresponds to combinational/arithmetic logic (value expr) and net connections (bind expr)
Value expressions could also be considered a syntactic sugar around binding a single reference to the anonymous output reference of the expression.

Modules are named scopes, with input and output references.
Inner modules close over the scope of the outer module.
Binding expressions are anonymous scopes, that also close over the outer scope.

Bindings expressions behave like a reversed scope. The result of a binding expression is a set of references. For each reference it includes the expression that drives it, made up of the network of possible conditional paths and values that feed it. References must have a non-ambiguous input network, with a single driver. Multiple drivers is possible due to the terminal list of bindings separated by semicolons. The use of a binding in more than one branch of a binding list is an error. Bind of a reference in multiple mutually exclusive branches is allowed, i.e. in if/else or match.
## Value expressions
Each value expression corresponds to some core device.

`if` and `match` are conditional logic statements.

`let` is used for adding a net reference to the local reference scope.

Arithmetic and logical operators correspond to implementing devices.
## Addresses
Addresses are special bindings that indicate some addressable parameter. For example, switching a mux or accessing a register. The type of address is always `ctrl<unsigned<>>`
`let f <- mux[a] {...}`
## Clocks and reset
Clocks are special type that indicate schedule sensitivity. References can specify relationship to the clock with `a@{clk}`, or with an positive or negative offset `a@{clk + 1}`. Clocks cannot be used as references.

Clock signals can also be used for reset signals used in the `reset bind` binding statement.
## Template variables and operators
Template variables are used for type parameterization (generics). They can also be used in generative constructs.

Template numeric values do not have a radix.

Template generator #range(begin, end, step) is available.

`let #templ <- #tmpl {}` can be used with both bindings and values expressions.

`if #templ_cond {...} else {...}`

Following constructs are allowed. The inner body of the expression yields a binding

`for #var in #generator {...}`
## Verilog compatibility
Use the backtick to escape. Limited to verilog module instantiations. Use with `bind`
## Default bindings
Special bind block with bindings if a binding is missing in other code paths.
```default bind * <= {...}```
## Reset bindings
Special bind block with bindings to value on reset
```reset@{rst} bind * <= {...}```
