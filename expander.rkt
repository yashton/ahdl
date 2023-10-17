#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/string))
(require (for-syntax syntax/stx))
(require "ast.rkt")

(provide hardware #%module-begin #%app #%datum
         namespace_def use_def
         device_def bind_expr_def
         enum_def id_name
         struct_def
         let_expr match_expr
         let_bind match_bind
         bind_def right_binding binding
         (struct-out enum))
;; Device instantiation algorithm
;; resolve template expressions
;; Match desugar
;; -- change to equality if/else with let bindings
;; right binding lhs desugar
;; -- any non-reference expression gets wrapped in a let
;; make every let expression into a device, replace with output (preserve names if possible)
;; make every conditional expression into a device, replace with output
;; substitute all inner binding references with the outer device binding.

;; for each device binding reference:
;; copy expression tree (include default and reset bindings)
;; find all branches that have the one reference, (regardless of timing annotation)
;; prune the others
;; verify schedule (all output times > inputs)

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

;;;;;;;;;;;;;;;; Enum definitions ;;;;;;;;;;;;;;;;
(define-syntax (enum_def stx)
  (syntax-case stx ()
    [(_ name values ...)
     #'(module* name #f
         values ...)]))

(define-syntax (id_name stx)
  (syntax-case stx ()
    [(_ name) #'(begin (provide name) (define name (enum name '())))]
    [(_ name id) #'(begin (provide name) (define name (enum name id)))]))

;;;;;;;;;;;;;;;; Device definitions ;;;;;;;;;;;;;;;;
(define-syntax (device_def stx)
  (syntax-case stx (template_def addr_def clk_def device_body_defs argument_list argument)
    [(_ name types ...
        (argument_list (argument input_name input_type ...) ...)
        (argument_list (argument output_name output_type ...) ...)
        (device_body_defs defs ...))
        #'(module* name #f
            (define inputs '(input_name ...))
            defs ...
            (define outputs '(output_name ...)))]))

;;;;;;;;;;;;;;;; Struct definitions ;;;;;;;;;;;;;;;;
(define-syntax (struct_def stx)
  (syntax-case stx (struct_item)
    [(_ name size (struct_item item_name type) ...) #'(struct name (item_name ...))]
    [(_ name (struct_item item_name type) ...) #'(struct name (item_name ...))]))

;;;;;;;;;;;;;;;; Desugar ;;;;;;;;;;;;;;;;
;; expand let item lists to nested let
(define-syntax (let_expr stx)
  (syntax-case stx (let_items)
    [(_ (let_items thing) body)
     #'(let_expr thing body)]
    [(_ (let_items first rest ...) body)
     #'(let_expr first (let_expr (let_items rest ...) body))]))

(define-syntax (let_bind stx)
  (syntax-case stx (let_items)
    [(_ (let_items thing) body)
     #'(let_bind thing body)]
    [(_ (let_items first rest ...) body)
     #'(let_bind first (let_bind (let_items rest ...) body))]))

;; Desugar destructure to nested let
;; probably need to delay to next phase (treat as decoder)

;; Desugar match to if/else
(define-syntax (match_expr stx)
  (syntax-case stx ()
    [(_ expr cases ...)
     (with-syntax ([symbol (generate-temporary)])
       (with-syntax ([ifs (desugar-match-expr-case #'symbol (stx->list #'(cases ...)))])
         #'(let_expr (left_binding symbol expr) ifs)))]))

(define-for-syntax (desugar-match-expr-case top_id cases)
  (if (stx-null? cases)
      #'(bindset)
      (syntax-case (stx-car cases) (match_expr_case match_clause)
        [(match_expr_case (match_clause destruct condition) body)
         (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_expr (match_expr_cond top destruct condition)
                        (let_expr (left_binding destruct top) body)
                        inner)))]
        [(match_expr_case (match_clause destruct) body)
         (with-syntax ([inner (desugar-match-expr-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_expr (match_expr_cond top destruct)
                        (let_expr (left_binding destruct top) body)
                        inner)))])))

(define-syntax (match_bind stx)
  (syntax-case stx ()
    [(_ expr cases ...)
     (with-syntax ([symbol (generate-temporary)])
       (with-syntax ([ifs (desugar-match-bind-case #'symbol (stx->list #'(cases ...)))])
         #'(let_bind (left_binding symbol expr) ifs)))]))

(define-for-syntax (desugar-match-bind-case top_id cases)
  (if (stx-null? cases)
      #'(bindset)
      (syntax-case (stx-car cases) (match_bind_case match_clause)
        [(match_bind_case (match_clause destruct condition) body)
         (with-syntax ([inner (desugar-match-bind-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_bind (match_bind_cond top destruct condition)
                        (let_bind (left_binding destruct top) body)
                        inner)))]
        [(match_bind_case (match_clause destruct) body)
         (with-syntax ([inner (desugar-match-bind-case top_id (stx-cdr cases))])
           (with-syntax ([top top_id])
             #'(if_bind (match_bind_cond top destruct)
                        (let_bind (left_binding destruct top) body)
                        inner)))])))


;; Expand right binding lhs expressions to a let
(define-syntax (right_binding stx)
  (syntax-case stx (reference let_bind left_binding right_binding)
    [(_ (reference _ ...) _) stx]
    [(_ lhs rhs)
     (with-syntax ([symbol (generate-temporary)])
       #'(let_bind (left_binding symbol lhs) (right_binding symbol rhs)))]))

;;;;;;;;;;;;;;;; Constant declarations ;;;;;;;;;;;;;;;;

;; (define-syntax (const_def name expr)
;;   (define name expr))

;; (define-syntax (union_def name items)
;;   (struct name (map items (struct item members))))
;; (define-syntax (type_def name items)
;;   (define name items))

;;;;;;;;;;;;;;;; Binding ;;;;;;;;;;;;;;;;
(define-syntax (bind_expr_def stx)
  (syntax-case stx ()
    [(_ ref expr)
     (with-syntax ([ident (generate-temporary #'ref)])
       #'(define ident expr))]))

(define-syntax (bind_def stx)
  (syntax-case stx ()
    [(_ lhs expr)
     (with-syntax ([ident (generate-temporary #'ref)])
       #'(define ident expr))]))

(define-syntax (binding stx)
  (syntax-case stx ()
    [(_ expr)
     #'(list expr)]))

;; ;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;
;; (define-syntax (if_templ_def clause then else)
;;    (if clause then else))
;; ;;(define bindings (foreach (define for_templ_def)))
;; ;;;
