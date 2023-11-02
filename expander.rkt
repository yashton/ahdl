#lang racket
(require (for-syntax syntax/parse)
         (for-syntax racket/syntax)
         (for-syntax racket/string)
         (for-syntax racket/set)
         (for-syntax syntax/stx)
         (for-syntax syntax/id-set))

(provide hardware #%module-begin #%app #%datum
         namespace_def use_def
         ;;const_def
         bind_def
         )

;; const_def -> bound value
;; device_def -> (constructor template_vars) -> (bindable context))
;; namespace_def -> module
;; use_def -> require
;; bind_def -> input scope -> (bindable context) -> binding

;; evaluate templates

;; Match desugar
;; -- change to equality if/else with let bindings
;; right binding lhs desugar
;; -- any non-reference expression gets wrapped in a let
;; expand let item list to nested lists
;;;; TODO
;; desugar destructure to nested lets

;; start a device graph.
;; add PI/POs nets to graph.
;; for all binds:
;; -- add any other new bind nets from lhs binding aliases and expansions to graph
;; -- duplicates are allowed, will be resolved during schedule verification.
;; add references to all graph nets to reference scope

;; breadth first bind traverse
;; all module level graph net references should be in scope.
;; take every let or conditional expression, add to device graph.
;; -- generate-temporary should be globally unique(?check?), use as top level net name
;; replace if conditional expressions with the generated net reference.
;; replace let right hand expressions with generated references
;; gather all right binding references
;; remaining expression should consist entirely of let aliases and if/else terminating in bindsets

;; depth first bind traverse
;; case bindset.
;; -- flatmap on each binding in the bindset,
;; ---- if it is a right_binding, rewrite to bind_expr_def
;; ---- i.e. (right_binding ref bind) => (bind_expr_def bind ref)
;; ---- otherwise recurse
;; -- return list of bind_expr_def
;; case if/else.
;; -- recurse left and right.
;; -- outer join the two lists. use a 'default place holder if doesn't exist
;; ---- double binding is an error
;; -- rewrite the two bind_expr_def into a single
;; -- (if c (bind_expr_def a x) (bind_expr_def a y)) => (bind_expr_def a (if c x y))
;; -- substitute default binding if defined.
;; -- if no default binding is defined and an expression is incomplete, error.
;; -- do bind lhs destructure assignment
;; result should be a set of bind_expr_def for each possible binding
;; -- presence in multiple bind_defs constitutes a double binding

;; gather clocked nets, verify schedule (all bind times > use times)

;;;;;;;;;;;;;;;; Desugar ;;;;;;;;;;;;;;;;
;; expand let item lists to nested let
(define-for-syntax (desugar-match-expr-case top_id cases)
  (with-syntax ([top top_id])
    (if (stx-null? cases)
        #'(bindset)
        (syntax-case (stx-car cases) (match_expr_case match_clause)
          [(match_expr_case (match_clause destruct condition) body)
           (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))]
                         [de-body (desugar #'body)])
             ;; in principle could move the destruct as a let around the if and mess with scope.
             ;; destructure twice is no big deal to duplicate, just creating references
             #'(if_expr (match_expr_cond top destruct condition)
                        (let_expr (left_binding destruct top) de-body)
                        inner))]
          [(match_expr_case (match_clause destruct) body)
           (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))]
                         [de-body (desugar #'body)])
             #'(if_expr (match_expr_cond top destruct)
                        (let_expr (left_binding destruct top) de-body)
                        inner))]))))

(define-for-syntax (ref-id stx)
  (syntax-case stx ()
    [(reference s) #'s]
    [_ stx]))

(define-syntax (reference stx)
  (syntax-case stx ()
    [(_ s) #'s]))

(define-for-syntax (desugar stx)
  (syntax-case stx (reference match_expr if_expr let_expr let_items left_binding right_binding)
;; Desugar destructure to nested let
;; probably need to delay to next phase (treat as decoder)
    [(let_expr (let_items thing) body)
     (with-syntax ([de-body (desugar #'body)]
                   [de-thing (desugar #'thing)])
       #'(let_expr de-thing de-body))]
    [(let_expr (let_items first rest ...) body)
     (with-syntax ([de-body (desugar #'(let_expr (let_items rest ...) body))]
                   [de-first (desugar #'first)])
       #'(let_expr de-first de-body))]
;; Desugar match to if/else
    [(match_expr expr cases ...)
     (with-syntax ([symbol (generate-temporary)]
                   [de-expr (desugar #'expr)])
       (with-syntax ([ifs (desugar-match-expr-case #'symbol (stx->list #'(cases ...)))])
         #'(let_expr (left_binding symbol de-expr) ifs)))]
;; Expand right binding lhs expressions to a let
    [(right_binding (reference _ ...) _) stx]
    [(right_binding lhs rhs)
     (with-syntax ([symbol (generate-temporary)]
                   [de-lhs (desugar #'lhs)])
       #'(let_expr (left_binding symbol de-lhs) (right_binding symbol rhs)))]
    [(if_expr cond then else)
     (with-syntax ([de-cond (desugar #'cond)]
                   [de-then (desugar #'then)]
                   [de-else (desugar #'else)])
       #'(if_expr de-cond de-then de-else))]
    [x #'x]))

;;;;;;;;;;;;;;;; Statements ;;;;;;;;;;;;;;;;
(define-syntax (hardware stx)
  (syntax-case stx ()
   [(_) #''()] ; void fallback for an empty file
   [(_ defs ...)  #'(begin defs ...)]))

(define-syntax (namespace_def stx)
  (syntax-case stx (namespace_id)
    [(_ (namespace_id id) defs ...)
     #'(module* id #f defs ...)]
    [(_ (namespace_id id inner ...) defs ...)
     #'(module* id #f (namespace_def (namespace_id inner ...) defs ...))]))

(define-syntax (use_def stx)
  (syntax-case stx (use_aliases)
    [(_ paths ... (use_aliases (use_alias alias ...) ...))
     (with-syntax ([mod (syntax-local-eval #'(string-join (paths ...) "/"))])
       #'(require (rename-in (lib mod) (alias ...) ...)))]
     [(_ paths ...)
     (with-syntax ([mod (syntax-local-eval #'(string-join (paths ...) "/"))])
        #'(require (lib mod)))]))

;;;;;;;;;;;;;;;; Binding ;;;;;;;;;;;;;;;;
(define-for-syntax (raise-missing-bind bind)
  (raise-syntax-error
   #f "Conditional bound value had no corresponding bind or default" bind))

(define-for-syntax (raise-double-bind bind)
  (raise-syntax-error
   #f "Reference bound twice" bind))

(define-for-syntax (wrap-bind assign ident)
  (lambda (dev)
    (syntax-case dev (bind_expr)
      [(bind_expr a b)
       (with-syntax ([assign assign]
                     [ident ident])
         #'(bind_expr a (let_expr (left_binding assign ident) b)))])))

;; These should be the only cases left after desugar and destructuring
(define-for-syntax (extract-devices expr)
  (syntax-case expr (if_expr let_expr right_binding bindset)
    [(if_expr cond then else)
     (with-syntax ([ident (generate-temporary #'cond)]
                   [(then-expr (then-devs ...) (then-binds ...)) (extract-devices #'then)]
                   [(else-expr (else-devs ...) (else-binds ...)) (extract-devices #'else)])
       #'((if_expr ident then-expr else-expr)
          ((bind_expr ident cond) then-devs ... else-devs ...)
          (then-binds ... else-binds ...)))]
    [(let_expr (left_binding assign expr) body)
     (with-syntax ([ident (generate-temporary #'cond)]
                   [(body-expr (body-devs ...) body-binds) (extract-devices #'body)])
         (with-syntax ([(devs ...) (stx-map (wrap-bind #'assign #'ident)
                                            #'(body-devs ...))])
           #'((let_expr (left_binding assign ident) body-expr)
              ((bind_expr ident expr) devs ...)
              body-binds)))]
    [(bindset binding bindings ...)
     (with-syntax ([(expr (devs ...) (binds ...)) (extract-devices #'binding)]
                   [((bindset rest_expr ...) (rest_devs ...) (rest_bind ...))
                    (extract-devices #'(bindset bindings ...))])
       #'((bindset bindings ... rest_expr ...)
          (devs ... rest_devs ...)
          (binds ... rest_bind ...)))]
    [(bindset bind)
     (extract-devices #'bind)]
    [(right_binding left right)
     #'((right_binding left right)
        ()
        (right))]
    [other #'(other () ())]))

(define-for-syntax (extract-expr expr)
  (syntax-case expr (bind_expr if_expr let_expr)
    [(bind_expr id body)
     (with-syntax ([body-expr (extract-expr #'body)])
       #'(bind_expr id body-expr))]
    [(if_expr cond then else)
     (with-syntax ([then-expr (extract-expr #'then)]
                   [else-expr (extract-expr #'else)])
       #'(op_mux cond then-expr else-expr))]
    [(let_expr (left_binding id value) body)
     (with-syntax ([body-expr (extract-expr #'body)]
                   [val-expr (extract-expr #'value)]
                   [ident (ref-id #'id)])
       #'(let ([ident val-expr]) body-expr))]
    [(op left center right)
     (with-syntax ([left-expr (extract-expr #'left)]
                   [center-expr (extract-expr #'center)]
                   [right-expr (extract-expr #'right)])
       #'(op left-expr center-expr right-expr))]
    [(op left right)
     (with-syntax ([left-expr (extract-expr #'left)]
                   [right-expr (extract-expr #'right)])
       #'(op left-expr right-expr))]
    [(op value)
     (with-syntax ([value-expr (extract-expr #'value)])
       #'(op value-expr))]
    [x #'x]))

(define-for-syntax (is-default? stx)
  (equal? (syntax->datum stx) 'default))

(define-for-syntax (extract-binding binding expr)
  (syntax-case expr (bind_def if_expr let_expr right_binding bindset default)
    [(if_expr cond then else)
     (with-syntax ([then-bind (extract-binding binding #'then)]
                   [else-bind (extract-binding binding #'else)])
       (if (and (is-default? #'then-bind)
                (is-default? #'else-bind))
           #''default
           #'(op_mux cond then-bind else-bind)))]
    [(let_expr (left_binding id value) body)
     (with-syntax ([body-bind (extract-binding binding #'body)]
                   [ident (ref-id #'id)])
       (if (is-default? #'body-bind)
           #''default
           #'(let ([ident value]) body-bind)))]
    [(bindset bind binds ...)
     (with-syntax ([extracted (extract-binding binding #'bind)]
                   [rest_extracted (extract-binding binding #'(bindset binds ...))])
       (if (and (not (is-default? #'extracted))
                (not (is-default? #'rest_extracted)))
           (raise-double-bind #'bind)
           (if (not (is-default? #'extracted))
               #'extracted
               (if (not (is-default? #'rest_extracted))
                   #'rest_extracted
                   #''default))))]
    [(bindset bind)
     (extract-binding binding #'bind)]
    [(right_binding left right)
     (if (equal? binding (ref-id #'right))
         #'left
         #'default)]))

(define-for-syntax (unique-ids bound)
  (foldl (lambda (b s) (free-id-set-add s (ref-id b)))
         (immutable-free-id-set)
         (stx->list bound)))

(define-syntax (bind_def stx)
  (syntax-case stx ()
    [(_ lhs expr)
     (with-syntax ([(tree (dev ...) bound) (extract-devices (desugar #'expr))])
       (with-syntax ([(bind ...) (set-map
                                  (unique-ids #'bound)
                                  (lambda (ref)
                                    (with-syntax ([inner (extract-binding ref #'tree)]
                                                  [id ref])
                                      #'(bind_expr id inner))))]
                     [(expr ...) (stx-map extract-expr #'(dev ...))])
         #'(begin expr ... bind ...)))]))

;;;;;;;;;;;;;;;; Nodes ;;;;;;;;;;;;;;;;
(provide
 (struct-out op_pos)
 (struct-out op_neg)
 (struct-out op_bnot)
 (struct-out op_not)
 (struct-out op_mult)
 (struct-out op_div)
 (struct-out op_mod)
 (struct-out op_add)
 (struct-out op_sub)
 (struct-out op_shiftl)
 (struct-out op_shiftr)
 (struct-out op_lt)
 (struct-out op_gt)
 (struct-out op_lte)
 (struct-out op_gte)
 (struct-out op_eq)
 (struct-out op_neq)
 (struct-out op_band)
 (struct-out op_bxor)
 (struct-out op_bor)
 (struct-out op_and)
 (struct-out op_or)
 (struct-out op_mux)
 (struct-out op_index)
 (struct-out op_slice)
 reference
;; (struct-out reference)
 (struct-out enum_def)
 (struct-out struct_def)
 (struct-out union_def)
 (struct-out union_variant)
 (struct-out device_instance)
 (struct-out struct_instance)
 (struct-out union_instance)
 (struct-out splat_literal)
 (struct-out binary_literal)
 (struct-out seximal_literal)
 (struct-out octal_literal)
 (struct-out decimal_literal)
 (struct-out hex_literal)
 (struct-out ascii_literal)
 (struct-out nif_literal))

(struct op_pos (left))
(struct op_neg (left))
(struct op_bnot (left))
(struct op_not (left))
(struct op_mult (left right))
(struct op_div (left right))
(struct op_mod (left right))
(struct op_add (left right))
(struct op_sub (left right))
(struct op_shiftl (left right))
(struct op_shiftr (left right))
(struct op_lt (left right))
(struct op_gt (left right))
(struct op_lte (left right))
(struct op_gte (left right))
(struct op_eq (left right))
(struct op_neq (left right))
(struct op_band (left right))
(struct op_bxor (left right))
(struct op_bor (left right))
(struct op_and (left right))
(struct op_or (left right))
(struct op_mux (switch left right))
(struct op_index (data index))
(struct op_slice (data msb lsb))
;; (struct reference (name address clock))
(struct enum_def (name id members))
(struct struct_def (name size members))
(struct union_def (name size id variants))
(struct union_variant (name members))
(struct device_instance (name template clk inputs))
(struct struct_instance (name members))
(struct union_instance (union name members))
(struct bool_literal (value))
(struct splat_literal (value))
(struct binary_literal (value))
(struct seximal_literal (value))
(struct octal_literal (value))
(struct decimal_literal (value))
(struct hex_literal (value))
(struct ascii_literal (value))
(struct nif_literal (value))

(provide device_def)
(define-syntax (device_def stx)
  (syntax-case stx (template_def addr_def clk_def device_body_defs argument_list argument)
    [(_ ... (device_body_defs defs ...))
     #'(begin defs ...)]))

(define-syntax (destructure stx) (generate-temporary stx))

(define-syntax (bind_expr stx)
  (syntax-case stx (reference)
    [(_ id expr) #'(define id expr)]))
