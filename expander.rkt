#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/string))
(require "ast.rkt")

;; Module instantiation algorithm
;; resolve template expressions
;; Match desugar
;; -- change to equality if/else with let bindings
;; right binding lhs desugar
;; -- any non-reference expression gets wrapped in a let
;; make every let expression into a device, replace with output (preserve names if possible)
;; make every conditional expression into a device, replace with output
;; substitute all inner binding references with the outer module binding.

;; for each module binding reference:
;; copy expression tree (include default and reset bindings)
;; find all branches that have the one reference, (regardless of timing annotation)
;; prune the others
;; verify schedule (all output times > inputs)


(provide hardware #%module-begin #%app #%datum
         namespace_def use_def
         module_def
         enum_def id_name
         struct_def
         (struct-out enum))

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

;;;;;;;;;;;;;;;; Module definitions ;;;;;;;;;;;;;;;;
(define-syntax (module_def stx)
  (syntax-case stx (template_def addr_def clk_def module_body_defs argument_list argument)
    [(_ name types ...
        (argument_list (argument input_name input_type ...) ...)
        (argument_list (argument output_name output_type ...) ...)
        (module_body_defs defs ...))
        #'(module* name #f
            (define inputs '(input_name ...))
            defs ...
            (define outputs '(output_name ...)))]))

;;;;;;;;;;;;;;;; Struct definitions ;;;;;;;;;;;;;;;;
(define-syntax (struct_def stx)
  (syntax-case stx (struct_item)
    [(_ name size (struct_item item_name type) ...) #'(struct name (item_name ...))]
    [(_ name (struct_item item_name type) ...) #'(struct name (item_name ...))]))

;;;;;;;;;;;;;;;; Constant declarations ;;;;;;;;;;;;;;;;

;; (define-syntax (const_def name expr)
;;   (define name expr))

;; (define-syntax (union_def name items)
;;   (struct name (map items (struct item members))))
;; (define-syntax (type_def name items)
;;   (define name items))

;; ;;;;;;;;;;;;;;;; Binds ;;;;;;;;;;;;;;;;
;; (define-syntax (bind_expr_def ref expr)
;;   (define ref expr))
;; ;; (bind_def,
;;  ;default_def
;;  ;reset_def

;; ;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;
;; (define-syntax (if_templ_def clause then else)
;;    (if clause then else))
;; ;;(define bindings (foreach (define for_templ_def)))
;; ;;;
