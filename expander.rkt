#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/string))
(require (for-syntax syntax/stx))
(require "desugar.rkt")

(provide hardware #%module-begin #%app #%datum
         namespace_def use_def
         device_def bind_expr_def
         enum_def id_name
         struct_def
         let_expr let_templ_expr match_expr
         let_bind let_templ_bind match_bind
         let_templ
         bind_def right_binding binding
         (struct-out enum))

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
;; -- generate-temporary should be globally unique(?check?), use as net name
;; replace if conditional expressions with the generated net reference.
;; substitute let references with generated references and remove let clauses.
;; remaining expression should consist entirely of let aliases and if/else terminating in bindsets of right_bindings

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

;;;;;;;;;;;;;;;; Constant declarations ;;;;;;;;;;;;;;;;

;; (define-syntax (const_def name expr)
;;   (define name expr))

;; (define-syntax (union_def name items)
;;   (struct name (map items (struct item members))))
;; (define-syntax (type_def name items)
;;   (define name items))

;;;;;;;;;;;;;;;; Binding ;;;;;;;;;;;;;;;;
(define-syntax (bind_def stx)
  (syntax-case stx ()
    [(_ lhs bind)
     (with-syntax ([((binds ...) (devs ...)) (extract-bind #'bind)])
     #'((bind_extract lhs (binds ...)) top_defs ...))]))

(define-for-syntax (raise-missing-bind bind)
  (raise-syntax-error
   #f "Conditional bound value had no corresponding bind or default" bind))

(define-for-syntax (raise-double-bind bind)
  (raise-syntax-error
   #f "Reference bound twice" bind))

(define-for-syntax (outer-join-branches cond lefts rights defaults)
  ;;  for each key in left, get from rights or from defaults, add to output.
  ;;  for each key in right, if key not in left, get from defaults, add to output
  ;;  if ever not in defaults, raise
  (foldl
   (match-lambda ((bind right) binds)
                 (if (dict-has-key? lefts bind)
                     binds
                     (let ([left (dict-ref defaults bind (lambda () (raise-missing-bind bind)))])
                       (dict-set bind #'(bind_expr_def bind (if_expr cond left right))))))
   (dict-map/copy
    (lambda (bind left)
      (let ([right (dict-ref
                    rights bind
                    (lambda ()
                      (dict-ref
                       defaults bind
                       (lambda () (raise-missing-bind bind)))))])
        #'(bind_expr_def bind (if_expr cond left right)))))))

(define-for-syntax (wrap-let ident assign exprs)
  (map (match-lambda ((bind_expr_def bind value))
                     #'(bind_expr_def bind (let_expr (left_binding ident assign) value)))
       exprs))

(define-for-syntax (dict-merge left right)
  (foldl (match-lambda (((ident value) binds))
                       (dict-set binds ident value))
         left
         (dict->list right)))
(define-for-syntax (dict-are-mutex? left right)
  (empty? (filter (curry dict-has-key? left)
                  (dict-keys right))))

;; These should be the only cases left after desugar and destructuring
(define-for-syntax (extract-bind-defaults bind defaults)
  (syntax-case bind (if_bind left_binding let_bind right_binding bindset)
    [(if_bind cond then else)
     (with-syntax ([ident (generate-temporary #'cond)]
                   [(then_binds then_devs) (extract_bind-defaults then defaults)]
                   [(else_binds else_devs) (extract_bind-defaults else defaults)])

       (list (outer-join-branches cond then_binds else_binds defaults)
             (cons (bind_expr_def ident cond) (append then_devs else_devs)))]
    [(let_bind (left_binding assign expr) body)
     (with-syntax ([ident (generate-temporary #'cond)])
       (match (extract-bind-defaults body defaults)
         [(binds devs)
          (let ([wrap-binds (wrap-let ident assign binds)]
                [wrap-devs (wrap-let ident assign devs)])
            (list wrap-binds (cons (bind_expr_def ident expr) wrap-devs))]))]
    [(bindset binding bindings ...)
     (let* ([(bindings devices) (extract-bind-defaults binding defaults)]
            [(rest_bindings rest_devices) (extract-bind-defaults binding defaults)]
            [joined_bindings (begin
                               (cond (dict-are-mutex? bindings rest-bindings)
                                     (raise-double-bind binding))
                               (dict-merge bindings rest-bindings)])
       (list joined_bindings (append devices rest_devices)))))]
    [(bindset binding)
     (extract-bind-defaults binding defaults)]
    [(right_binding left right)
     (with-syntax ([ident (reference-identifier right)])
       (list (make-free-id-table ident (bind_expr_def right left)) '()))]

(define-syntax (binding stx)
  (syntax-case stx ()
    [(_ expr)
     #'(list expr)]))

(define-for-syntax (reference-identifier stx)
  (with-syntax ([id stx])
    #'|stx|))
;; ;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;
;; (define-syntax (if_templ_def clause then else)
;;    (if clause then else))
;; ;;(define bindings (foreach (define for_templ_def)))
;; ;;;
